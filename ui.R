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

# Page 1 - Home - Show raw data -------------------------------------
airbnb_table = tabPanel(icon("home"),
         
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

# Page 1 - Introduction ----------------------------------------------
intro_panel = tabPanel(
  "Introduction",
  
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

second_panel = tabPanel(
  
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

third_panel = tabPanel(
  
  "Visualization",
  
  titlePanel("Visualize the Airbnb statistics for European cities"),
  
  p("Please use the sidebar to select a feature to visualize."),
  
  sidebarLayout(
    sb_panel,
    airbnb_main
  )
)

# User Interface -----------------------------------------------------
ui = fluidPage(theme = shinytheme("cerulean"),
               
               titlePanel("Modeling process: Linear regression for Airbnb Prices in European Cities"),
               
               navbarPage(
                 "Let's begin",
                 airbnb_table,
                 intro_panel,
                 second_panel,
                 third_panel
                 )
)