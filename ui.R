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
  fluidRow(
           column(tags$img(src = "london_tower_bridge.jpg", 
                           height = "450px", 
                           width = "670px"), 
                  width=8),
           column(tags$img(src="airbnb-logo.png", 
                           width="300px", 
                           height="223px"), 
                  width=4)
  ),
  
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
                                     select the independent variables you want to include in the model.
                                     ",
                                    style="color:black;text-align:justify"),
                                  br(),
                                  p("We are going to test the following hypothesis for all the selected variables:",style="color:black;text-align:justify"),
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
                                      # textOutput("txt"),
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

answers_panel = tabPanel("Description",
                        
                        fluidRow(column(tags$img(src="airbnb-logo.png", 
                                                 width="400px", 
                                                 height="300px"), 
                                        width=4),
                                 column(
                                   br(),
                                   p(strong("What data you collected?"),
                                     br(),
                                     "The dataset was downloaded from Kaggle.",
                                     a(href = "https://www.kaggle.com/datasets/dipeshkhemani/airbnb-cleaned-europe-dataset", 
                                       "Data Source: Kaggle - Airbnb Cleaned Europe Dataset, by Dipesh Khemani"),
                                     br(),
                                     "This data records the Airbnb rental prices in the European cities.", 
                                     br(),
                                     br(),
                                     "This dataset consists of 18 features and the target variable, price column. 
                                     There are about 41K data instances and a file size of 8.2MB.", 
                                     
                                     br(),
                                     br(),
                                     "Following are the column distributions:",
                                     
                                     br(),
                                     br(),
                                     "1. TARGET column: ",
                                     br(),
                                     "    1.1. price: value of the Airbnb per day in USD.",
                                     
                                     br(),
                                     br(),
                                     "2. CATEGORICAL columns: ",
                                     br(),
                                     "    2.1. city: name of the city where the room is located.",
                                     br(),
                                     "    2.2. day: Whether it is a weekday of weekend when the price is recorded.",
                                     br(),
                                     "    2.3. room_type: Private/Shared/Entire apartment.",
                                     br(),
                                     "    2.4. is_shared_room: Whether the room is shared with other guests.",
                                     br(),
                                     "    2.5. is_private_room: Whether the room has private amenities.",
                                     br(),
                                     "    2.6. is_business: Whether the room is owned by another real-estate business",
                                     br(),
                                     "    2.7. is_superhost: Is the property under the care of a Superb host? ",
                                     br(),
                                     "    2.8. has_multiple_rooms: Does the property have multiple rooms.",
                  
                                     
                                     br(),
                                     br(),
                                     "3. NUMERICAL columns: ",
                                     br(),
                                     "    3.1. room_capacity_persons",
                                     br(),
                                     "    3.2. rating_cleanliness",
                                     br(),
                                     "    3.3. rating_guest_satisfaction",
                                     br(),
                                     "    3.4. dist_city_center_km",
                                     br(),
                                     "    3.5. num_bedrooms",
                                     br(),
                                     "    3.6. dist_city_center_km",
                                     br(),
                                     "    3.7. dist_metro_km",
                                     br(),
                                     "    3.8. index_attraction",
                                     br(),
                                     "    3.9. index_normalised_attraction",
                                     br(),
                                     "    3.10. index_restraunt",
                                     br(),
                                     "    3.11. index_normalised_restraunt",
                                     
                                     br(),
                                     br(),
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   br(),
                                   
                                   p(strong("Why this topic is interesting or important to you?"),
                                       br(),
                                       "This topic is interesting due to multiple reasons. 
                                       Before that, a short intro of the Airbnb business:
                                       Airbnb, Inc. is an American San Francisco-based company 
                                       operating an online marketplace for short-term homestays 
                                       and experiences. ",
                                     br(),
                                     br(),
                                       "Following are the multiple reasons for choosing this dataset:",
                                     br(),
                                     br(),
                                     "1. Airbnb is a very good option for short-term stays. 
                                     However, it's difficult to understand the patterns for prices.
                                     This dataset enables us to analyze the various factors affecting 
                                     the variations in Airbnb housing prices.",
                                     
                                     br(),
                                     br(),
                                     "2. Tourism business ideas can benefit a lot from such kind data analysis and modeling.
                                      The home owners could understand if their house is a potential candidate for Airbnb business.",
                                     
                                     br(),
                                     br(),
                                     
                                       style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
                                     ),
                                   br(),
                                   
                                   p(strong("How did you analyze the data?"),
                                     br(),
                                     "I followed the Data Science Lifecycle to complete the project.
                                     Further, to avoid some outliers, I limited the price to be less than or equals to $1000.",
                                     br(),
                                     br(),
                                     "1. I visualized the target variable ", strong("price")," to understand the distribution. 
                                     I also followed a tutorial to analyze the Normality hypothesis of the target variable. 
                                     The price variable does not follow Normal distribution. 
                                     In fact it has a right-skewed tail due to expensive prices for some high-end Airbnb properties.",
                                     br(),
                                     br(),
                                     "2. I visualized the data distributions for the Numerical Independent variables",
                                     br(),
                                     br(),
                                     "3. Shortlisted important features and trained a Linear Regression model",
                                     br(),
                                     br(),
                                     "4. After modelling, we could also observe feature importances of various independent variables 
                                     and thus shortlist or change our selected features based on the p-value significance.",
                                     br(),
                                     br(),
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   br(),
                                   
                                   
                                   p(strong("What did you find in the data?"),
                                     br(),
                                     "I found that amongst all the features: room capacity, cleanliness, and proximity to city center 
                                     are the statistically significant features based on Linear Regression model summary.",
                                     br(),
                                     br(),
                                     "Following are the multiple observations regarding this dataset:",
                                     br(),
                                     br(),
                                     "1. The mean value of prices is: ", 
                                     strong("$250 per day"), 
                                     " while most of the Airbnb houses have prices in the range $90-125. 
                                               This is almost 50% of the actual mean.",
                                     br(),
                                     br(),
                                     "2. Tourism business ideas can benefit a lot from such kind data analysis and modeling.
                                     The home owners could understand if their house is a potential candidate for Airbnb business.",
                                     
                                     br(),
                                     br(),
                                     
                                     style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                                   
                                   width=8),
                                 br()
                        ),
                        
                        
                        hr(),
                        p(em("Developed by"),
                          br("Shreedhar Kodate"),
                          style="text-align:center; font-family: times")
)


# User Interface -----------------------------------------------------
ui = fluidPage(theme = shinytheme("cerulean"),
               
               titlePanel("Modeling process: Linear regression for Airbnb Prices in European Cities"),
               
               navbarPage(
                 "Let's begin",
                 intro_panel,
                 airbnb_table,
                 normality_panel,
                 visualization_panel,
                 model_panel,
                 answers_panel
                 )
)