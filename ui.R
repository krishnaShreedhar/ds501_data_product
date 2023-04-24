# Load libraries, data -----------------------------------------------
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

TARGET_COL = "price"

# Page 1 - Home - Show raw data -------------------------------------
airbnb_table = tabPanel("Dataset",
         
         fluidRow(column(tags$img(src="airbnb-logo.png", 
                                  width="400px", 
                                  height="300px"), 
                         width=4),
                  column(
                    br(),
                    p("This application visualizes and does Linear Regression 
                      modelling of Airbnb rental prices in the European cities.", 
                      "The dataset downloaded from Kaggle.",
                      style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                    br(),
                    
                    p("Airbnb, Inc. is an American San Francisco-based company 
                      operating an online marketplace for short-term homestays 
                      and experiences. ",
                      "For more information please check the",em("Wikipedia"),"page clicking ",
                      a(href="https://en.wikipedia.org/wiki/Airbnb", 
                        "here."),
                    style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                    
                    p(
                      br(),
                      a(href = "https://www.kaggle.com/datasets/dipeshkhemani/airbnb-cleaned-europe-dataset", 
                        "Data Source: Kaggle - Airbnb Cleaned Europe Dataset, by Dipesh Khemani"),
                      style="text-align:center;color:black"),
                    
                    width=8),
                  ),
         
         hr(),
         tags$style(".fa-database {color:#E87722}"),
         h3(p(em("Airbnb Cleaned Europe Dataset"),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
         fluidRow(column(DT::dataTableOutput("RawData"),
                         width = 12)),
         
         hr(),
         p(em("Developed by"),
           br("Shreedhar Kodate"),
           style="text-align:center; font-family: times")
)

normality_panel = tabPanel("Normality tests",
                           
                           fluidRow(column(width=2),
                                    column(
                                      h4(p("Normality",style="color:black;text-align:center")),
                                      width=8,style="background-color:lavender;border-radius: 10px")
                           ),
                           br(),
                           fluidRow(column(width=2, icon("hand-point-right","fa-5x"),align="center"),
                                    column(
                                      p("In general, we might assume that the TARGET_COL is normally distributed. 
                                      However, it is a good idea to establish whether the assumptions regarding distribution actually hold true.
                                      Let's assume that the response (dependent/target) variable is normally distributed.
                                        And, following this assumption, we will try to achieve this:",style="color:black;text-align:justify"),
                                      withMathJax(),
                                      p('$$H_0:~Y ~ \\sim ~ Normal( ~\\mu ~,~ \\sigma~ )$$',
                                        style="color:black;border:1px solid black;background-color:white"),
                                      p("In our case we will take as a response variable", strong(em(TARGET_COL)), "since this 
                                      a value of our interest which might be useful to bear in mind the housing expenditures. ",
                                        style="color:black;text-align:justify"),
                                      width=8,
                                      style="background-color:lavender;border-radius: 10px")
                           ),
                           br(),
                           fluidRow(column(width=2),
                                    column(
                                      p("Let's do some graphical and analytical tests in order to conclude about the Normality hypothesis",
                                        style="color:black;text-align:center"),
                                      width=8,style="background-color:papayawhip;border-radius: 10px")
                           ),
                           hr(),
                           tags$style(".fa-chart-pie {color:#E87722}"),
                           h3(p(em("Graphical tests "),icon("chart-pie",lib = "font-awesome"),style="color:black;text-align:center")),
                           tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}")),
                           tags$style(HTML(".js-irs-0 .irs-max, .js-irs-0 .irs-min {background:papayawhip}")),
                           
                           br(),
                           sidebarLayout(
                             sidebarPanel(
                               
                               sliderInput("power_transform",
                                           p("Try different power transformations to test if normality could be achieved:", 
                                             em(TARGET_COL),
                                             style="color:black;text-align:center"),
                                           value=1,
                                           min=-3,
                                           max=5,
                                           step=0.1),
                               br(),
                               
                               
                             ),
                             mainPanel(
                               
                               fluidRow(
                                 column(br(),plotOutput("hist_target"),br(),width=4,style="border:1px solid black"),
                                 column(br(),plotOutput("box_target"),br(),width=4,style="border: 1px solid black;border-left: none"),
                                 column(br(),plotOutput("qqp_target"),br(),width=4,style="border:1px solid black;border-left:none")
                                 
                               )
                             )),
                           hr(),
                           tags$style(".glyphicon-folder-open {color:#E87722}"),
                           h3(p(em("Analytical tests"),icon("folder-open",lib = "glyphicon"),style="color:black;text-align:center")),
                           br(),
                           sidebarLayout(
                             
                             sidebarPanel(
                               
                               selectInput("name_analytics_test",
                                           p("Please select the test you want to try:",
                                             style="color:black; text-align:center"),
                                           choices=c("Shapiro-Wilk"=1,
                                                     "Anderson-Darling"=2,
                                                     "Cramér-von Mises"=3,
                                                     "Kolmogorov-Smirnov"=4,
                                                     "Jarque-Bera"=5)),
                               uiOutput("ReadMore")
                             ),
                             mainPanel(
                               
                               fluidRow(
                                 
                                 tags$head(tags$style("#norm_conclusion{color: navy;
                                                      font-size: 15px;
                                                             font-style: italic;
                                                             font-weight: bold;
                                                             text-align: center
                                                             }")),
                                 tags$head(tags$style("#ana_test_results{height: 155px; border: 1px solid black; background-color: lavender}")),
                                 column(verbatimTextOutput("ana_test_results"),
                                        br(),width = 6),
                                 column(br(),
                                        p("We accept the NULL Hypothesis if we get a p-value greater than 0.05 (for a confidence level of 95%), so:",
                                          style="color:black"),
                                        br(),
                                        textOutput("norm_conclusion"),
                                        br(),
                                        width = 6,
                                        style="background-color:lavender;border-left:8px solid blue"
                                        )
                                 
                               )
                             ))
)


# Page 1 - Introduction ----------------------------------------------
intro_panel = tabPanel(
  icon("home"),
  
  titlePanel("Airbnb prices in the European cities"),
  
  img(src = "london_tower_bridge.jpg", 
      height = 600, 
      width = 1000),
  
  br(), 
  br(),
  
  p("This is an R Shiny application to understand the reasons of 
    Airbnb price variations in the European cities. 
    This can also act as a guide to set the expectations of expenditures for 
    Airbnb housing."),
  
  p(a(href = "https://www.kaggle.com/datasets/dipeshkhemani/airbnb-cleaned-europe-dataset", 
      "Data Source: Kaggle - Airbnb Cleaned Europe Dataset, by Dipesh Khemani")
    )
)

# Page 2 - Algorithm -------------------------------------------

algo_panel = tabPanel(
  
  "Algorithm",
  
  titlePanel("Regression algorithm"),
  
  p("Use the selector"),
  
  # sidebarLayout(
  #   sidebar_content, 
  #   main_content
  # )
)


# Page 3 - Vizualization -------------------------------------------
list_cols_airbnb = colnames(df_airbnb)
list_cols_airbnb = list_cols_airbnb[! list_cols_airbnb %in% c('city', 
                                                              'room_type')]
list_num_cols = names(df_airbnb)[sapply(df_airbnb, is.numeric)]

list_cat_cols = list_cols_airbnb[! list_cols_airbnb %in% list_num_cols]

list_cities = unique(df_airbnb$city)
list_cities = c(c("all"), list_cities)

sb_panel = sidebarPanel(
  selectInput(
    "f_num",
    label = "Numerical Feature",
    choices = list_num_cols,
    selected = 'rating_cleanliness'
  ),
  selectInput(
    "f_cat",
    label = "Categorical Feature",
    choices = list_num_cols,
    selected = 'rating_cleanliness'
  ),
  selectInput(
    "city_c",
    label = "City C",
    choices = list_cities,
    selected = list_cities[1]
  )
)

airbnb_main = mainPanel(
  plotOutput("plot")
)

visualization_panel = tabPanel(
  
  "Visualization",
  
  titlePanel("Visualize the Airbnb statistics for European cities"),
  
  p("Please use the sidebar to select a feature to visualize."),
  
  sidebarLayout(
    sb_panel,
    airbnb_main
  )
)


model_panel = tabPanel("Modelling",
                       
                       fluidRow(column(width=2),
                                column(
                                  h4(p("Final model",
                                       style="color:black;text-align:center")),
                                  width=8,
                                  style="background-color:lavender;border-radius: 10px")),
                       br(),
                       
                       fluidRow(column(width=2, 
                                       icon("hand-point-right","fa-5x"),
                                       align="center"),
                                column(
                                  p("Now we are going to build the final model, for this you will have to 
                                     select the independent variables you want to include in the model
                                     and especially select for which of them you want to include some non-linearity 
                                     in the model (this is related to the power transformations made to the 
                                     variables independent in a previous step)",
                                    style="color:black;text-align:justify"),
                                  br(),
                                  p("In this step it is very important to achieve a high adjusted coefficient of 
                                     determination and also to make the parameters of the model statistically significant, 
                                     for that reason we are going to test the following hypothesis:",style="color:black;text-align:justify"),
                                  p('$$H_0: ~ \\beta_i = 0$$',
                                    style="color:black;border:1px solid black;background-color:white"),
                                  width=8,
                                  style="background-color:lavender;border-radius: 10px")),
                       br(),
                       fluidRow(column(width=2),
                                column(
                                  p("Let's build our regression model",
                                    style="color:black; text-align:center"),
                                  width = 8,
                                  style="background-color:papayawhip; border-radius: 10px"
                                )),
                       hr(),
                       sidebarLayout(
                         
                         sidebarPanel(
                           
                           
                           fluidRow(column(
                             checkboxGroupInput("include_features",
                                                p("Select the independent variables you would like to include:",
                                                  style="color:coral;text-align:justify"),
                                                choiceNames =
                                                  list("room_capacity_persons",
                                                       "rating_cleanliness",
                                                       "rating_guest_satisfaction",
                                                       "dist_city_center_km",
                                                       "dist_metro_km",
                                                       "index_normalised_attraction",
                                                       "index_normalised_restraunt"),
                                                choiceValues =
                                                  list("room_capacity_persons",
                                                       "rating_cleanliness",
                                                       "rating_guest_satisfaction",
                                                       "dist_city_center_km",
                                                       "dist_metro_km",
                                                       "index_normalised_attraction",
                                                       "index_normalised_restraunt")
                                                ),
                             width = 6)
                             
                             ),
                           br(),
                           uiOutput("Anothermessage"),
                           br()
                           
                           
                         ),
                         mainPanel(
                           
                           h3(p(strong('Model summary',
                                       style="color:salmon")),
                              align="center"),
                           uiOutput("selected"),
                           
                           tags$head(tags$style("#model_summ{height: 400px;width:auto;border: 1px solid black; background-color: lavender}")),
                           
                           tags$head(tags$style("#ModeloBack{height: 400px;width:auto;border: 1px solid black; background-color: lavender}")),
                           
                           fluidRow(column(verbatimTextOutput("model_summ"),
                                           width = 12),

                                    column(
                                      uiOutput("significance"),
                                      br(),
                                      uiOutput("Determinacionfinal"),
                                      textOutput("txt"),
                                      width = 8)
                                    ),
                           hr()
                           # ,
                           # h3(p(strong('Final model',
                           #             style="color:salmon")),
                           #    align="center"),
                           # fluidRow(column(verbatimTextOutput("ModeloBack"),
                           #                 width=8),
                           #          column(uiOutput("Determinacionfinal"),
                           #                 width=4))
                           
                           
                         )
                         
                       ))


# User Interface -----------------------------------------------------
ui = fluidPage(theme = shinytheme("cerulean"),
               
               titlePanel("Modeling process: Linear regression for Airbnb Prices in European Cities"),
               
               navbarPage(
                 "Let's begin",
                 intro_panel,
                 airbnb_table,
                 normality_panel,
                 visualization_panel,
                 model_panel
                 )
)