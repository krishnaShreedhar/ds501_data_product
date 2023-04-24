# Load libraries ------------------------------------------------
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(car)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)


# Load data
df_airbnb = read.csv("data/airbnb_europe_cities.csv")

cond_price_lte_1000 = df_airbnb$price <= 1000
df_airbnb = df_airbnb[cond_price_lte_1000,]

list_num_cols = names(df_airbnb)[sapply(df_airbnb, is.numeric)]
v_cols = names(df_airbnb)

list_cities = unique(df_airbnb$city)
list_cities = c(list_cities, c("all"))

TARGET_COL = "price"


# Server code-------------------------------------------------------

server = function(input, output) {
  
  # Visualization code ------------------------------------------------------------
  output$plot <- renderPlot({
    p = ggplot(df_airbnb,
               aes(x = df_airbnb[, input$f_num])) +
      geom_histogram(aes(y = after_stat(density)),
                     colour = "black",
                     fill = "white") +
      geom_density(alpha = .2, fill = "#FF6666")
    
    p + geom_vline(
      aes(xintercept = mean(df_airbnb[, input$f_num])),
      color = "blue",
      linetype = "dashed",
      size = 1
    )
 
  })
  
  # Raw table code ------------------------------------------------------------
  output$RawData <- DT::renderDataTable(
    DT::datatable({
      df_airbnb
    },
    options = list(lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = v_cols
    ))
  
  # Normality code ------------------------------------------------------------
  
  var_transform <- reactive({
    
    list_target = df_airbnb[, TARGET_COL]
    
    if(input$power_transform == 0){
      log_target = log(list_target)
    }
    else {
      log_target = list_target ^ as.double(input$power_transform)
    }
    
    if(input$power_transform == 1){
      title = TARGET_COL
    }
    else{
      title = paste("Log Transformed ", TARGET_COL)
    }
    
    Resultado <- cbind(log_target, title)
    
    
  })
  
  # var_transform2 <- reactive({
  #   
  #   attach(datos)
  #   if(input$power_transform == 0){
  #     log_target = log(list_target)
  #   }
  #   else{
  #     log_target = list_target ^ as.double(input$power_transform)
  #   }
  #   
  #   if(input$power_transform == 1){
  #     title = TARGET_COL
  #   }
  #   else{
  #     title = paste("Log Transformed ", TARGET_COL)
  #   }
  #   
  #   log_target
  # })
  
  output$hist_target <- renderPlot({
    
    ggplot(NULL,aes(as.double(var_transform()[,1])))+geom_histogram(bins=nclass.Sturges(as.double(var_transform()[,1])),color="white",
                                                                    fill="seagreen1",aes(y=..density..),lwd=0.8)+geom_density(color="seagreen4",
                                                                                                                              alpha=0.3,fill="seagreen4",lty=1)+
      labs(title = paste(var_transform()[1,2], "\n histogram"),x=var_transform()[1,2],y="Density")+
      theme(plot.title = element_text(color="navy", size=15, face="bold.italic",hjust=0.5),
            axis.title.x = element_text(color="navy", size=13, face="bold"),
            axis.title.y = element_text(color="navy", size=13, face="bold"))
    
    
  })
  
  output$box_target <- renderPlot({
    
    ggplot(NULL,aes(x=0,y=as.double(var_transform()[,1])))+geom_boxplot(color="black",fill="skyblue",alpha=0.5)+ stat_summary(fun.y=mean, colour="darkred", geom="point",shape=18, size=3)+
      labs(title = paste(var_transform()[1,2], "\n box_target"),x="",y=var_transform()[1,2])+
      theme(plot.title = element_text(color="navy", size=15, face="bold.italic",hjust=0.5),
            axis.title.x = element_text(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_text(color="navy", size=13, face="bold"))
    
    
  })
  
  output$qqp_target <- renderPlot({
    
    par(font.main=4,font.lab=2,col.main="navy",col.lab="navy",cex.lab=1.1)
    qqPlot(as.double(var_transform()[,1]),grid=F,xlab="",ylab="")
    u <-par("usr")
    rect(u[1], u[3], u[2], u[4], col="#EBE9E9", border=TRUE)
    grid(NULL,NULL,col="white",lty=1)
    par(new=TRUE)
    qqPlot(as.double(var_transform()[,1]),col="coral",pch=16,id=T,lwd=1.9,col.lines = "black",grid = F, main = paste(var_transform()[1,2], "\n Q-Q plot"),xlab="Normal quantiles",ylab=var_transform()[1,2])
    box(col="white")
    
    
  })
  
  do_analytics_test <- reactive({
    
    if(input$name_analytics_test == 1){
      # Shapiro fails if number of samples exceed 5000
      ana_test_results <- shapiro.test(as.double(var_transform()[,1][seq_along(remove) %% 2 > 0][1:5000]))
      
    }
    else
    {
      
      if(input$name_analytics_test ==2){
        
        ana_test_results <- ad.test(as.double(var_transform()[,1]))
        
      }
      else
      {
        
        if(input$name_analytics_test == 3){
          
          ana_test_results <- cvm.test(as.double(var_transform()[,1]))
          
        }
        else
        {
          
          if(input$name_analytics_test == 4){
            
            ana_test_results <- lillie.test(as.double(var_transform()[,1]))
            
          }
          else
          {
            
            ana_test_results <- jarque.bera.test(as.double(var_transform()[,1]))
            
          }
          
        }
        
      }
      
      
    }
    
    ana_test_results$data.name <- var_transform()[1,2]
    ana_test_results
    
  })
  
  output$ana_test_results <- renderPrint({
    
    do_analytics_test()
    
  })
  
  output$norm_conclusion <- renderText({
    
    if(do_analytics_test()$p.value < 0.05){
      mensaje = paste("We reject the NULL Hypothesis, the variable ", TARGET_COL," is not normally distributed")
    }
    else{
      mensaje = paste("We accept the NULL Hypothesis, the variable ", TARGET_COL," is normally distributed")
    }
    
    mensaje
    
  })
  
  output$ReadMore <- renderUI({
    
    
    if(input$name_analytics_test == 1){
      
      More <- p("Read more about this test here → ", 
                a(href="https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test", 
                  icon("wikipedia-w"),
                  target="_blank"),
                style="color:black;text-align:center")
      
    }
    else
    {
      
      if(input$name_analytics_test == 2){
        
        More <- p("Read more about this test here → ", 
                  a(href="https://en.wikipedia.org/wiki/Anderson%E2%80%93Darling_test", 
                    icon("wikipedia-w"),
                    target="_blank"),
                  style="color:black;text-align:center")
        
      }
      else
      {
        
        if(input$name_analytics_test == 3){
          
          More <- p("Read more about this test here → ", 
                    a(href="https://en.wikipedia.org/wiki/Cram%C3%A9r%E2%80%93von_Mises_criterion", 
                      icon("wikipedia-w"),
                      target="_blank"),
                    style="color:black;text-align:center")
          
        }
        else
        {
          
          if(input$name_analytics_test ==4){
            
            More <- p("Read more about this test here → ", 
                      a(href="https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test", 
                        icon("wikipedia-w"),
                        target="_blank"),
                      style="color:black;text-align:center")
            
            
          }
          else
          {
            
            More <- p("Read more about this test here → ", 
                      a(href="https://en.wikipedia.org/wiki/Jarque%E2%80%93Bera_test", 
                        icon("wikipedia-w"),
                        target="_blank"),
                      style="color:black;text-align:center")
            
            
          }
          
        }
        
      }
      
    }
    
    More
    
  })
  
  # Modelling code --------------
  
  selected <- reactive({
      vars = input$include_features
      sort(vars,decreasing = F)
      xnames2 <- paste0(colnames(todasvariables2[as.double(vars)]))
      vars
  })
  
  output$txt <- renderText({
    icons <- paste(input$include_features, collapse = ", ")
    paste("You chose", icons)
  })
  
  
  selecciondevariables <- reactive({
    
    features <- c("price",
                  "room_capacity_persons",
                  "rating_cleanliness",
                  "rating_guest_satisfaction",
                  "dist_city_center_km",
                  "dist_metro_km",
                  "index_normalised_attraction",
                  "index_normalised_restraunt")
    
    df_features <- df_airbnb[,features]
    
    Model3 <- lm(price ~ ., data=df_features)
    
    Model3
    
  })

  
  output$Determinacionfinal <- renderUI({
    
    coeficiente = summary(selecciondevariables())$adj.r.squared
    p(paste("With the final model you built, you get an adjusted square R of:",
            coeficiente),
      style="padding:25px;background-color:papayawhip;border-left:8px solid coral;border-top: 1px solid black;border-right:1px solid black;border-bottom: 1px solid black;color:black;text-align:center" )
    
  })
  
  
  output$model_summ <- renderPrint({
    
    summary(selecciondevariables())
    
  })
  
  
  output$Anothermessage <- renderUI({
    
    p("Those variables whose betas are not significant should be eliminated from the model, 
      try to get them out one by one prioritizing those whose betas have a higher p-value (Pr(>|t|))",
      style="padding:25px;background-color:papayawhip;border-left:8px solid coral;border-top: 1px solid black;border-right:1px solid black;border-bottom: 1px solid black;color:black;text-align:center")
    
    
  })
  
  
}
