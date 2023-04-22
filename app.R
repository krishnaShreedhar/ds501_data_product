# Install packages ----------------------------
install.packages("shiny")

# Load libraries ------------------------------
library("shiny")
source("ui.R")
source("server.R")

# Create your shiny app -----------------------
shinyApp(ui = ui, 
         server = server)