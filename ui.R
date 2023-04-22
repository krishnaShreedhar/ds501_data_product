# Load libraries, data -----------------------------------------------

df_airbnb = read.csv("data/airbnb_europe_cities.csv")

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

sb_num_cols = sidebarPanel(
  selectInput(
    "f_num",
    label = "Numerical Feature",
    choices = list_num_cols,
    selected = 'rating_cleanliness'
  )
)

sb_cat_cols = sidebarPanel(
  selectInput(
    "f_cat",
    label = "Categorical Feature",
    choices = list_num_cols,
    selected = 'rating_cleanliness'
  )
)

sb_list_cities = sidebarPanel(
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
    sb_num_cols,
    airbnb_main
  )
)



# User Interface -----------------------------------------------------
ui = navbarPage(
  "Airbnb Prices in European Cities",
  intro_panel,
  second_panel,
  third_panel
)