# Load libraries and data ------------------------------------------------
library(ggplot2)

df_airbnb = read.csv("data/airbnb_europe_cities.csv")

cond_price_lte_1000 = df_airbnb$price <= 1000
df_airbnb = df_airbnb[cond_price_lte_1000,]

list_num_cols = names(df_airbnb)[sapply(df_airbnb, is.numeric)]

list_cities = unique(df_airbnb$city)
list_cities = c(list_cities, c("all"))


# Server code-------------------------------------------------------

server = function(input, output) {
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
  
}
