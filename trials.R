
df_airbnb = read.csv("data/airbnb_europe_cities.csv")

names(df_airbnb)

TARGET_COL = "price"
list_target = df_airbnb[, TARGET_COL]
typeof(list_target)
list_target

attach(df_airbnb)
typeof(price)

log_target = log(list_target)

title = "price X"
Resultado <- cbind(log_target, title)
Resultado[,1]
prueba <- shapiro.test(as.double(Resultado[,1][seq_along(remove) %% 2 > 0][1:5000]))
prueba

cond_price_lte_1000 = df_airbnb$price <= 1000
df_airbnb = df_airbnb[cond_price_lte_1000, ]

p = ggplot(df_airbnb, aes(x=df_airbnb[,"price"])) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

p
# Add mean line
p + geom_vline(aes(xintercept=mean(df_airbnb[,"price"])),
               color="blue", 
               linetype="dashed", 
               size=1)

list_num_cols = names(df_airbnb)[sapply(df_airbnb, is.numeric)]
list_num_cols
list_num_cols[1]

sb_list_cities = unique(df_airbnb$city)
sb_list_cities = c(sb_list_cities, c("all"))
sb_list_cities

library(shiny)
shiny::runGitHub("shiny-examples", "rstudio", subdir = "001-hello")




# -------------------------------
features <- c("price",
  "room_capacity_persons",
              "rating_cleanliness",
              "rating_guest_satisfaction",
              "dist_city_center_km",
              "dist_metro_km",
              "index_normalised_attraction",
              "index_normalised_restraunt")
typeof(features[0])
length(features)

df_features <- df_airbnb[,features]

attach(df_features)

todasvariables2 <- df_airbnb[,features]


TARGET_COL = "price"
list_target = df_airbnb[, TARGET_COL]

xnames2 = paste0(colnames(df_features[as.double(features)]))

fmla3 <- as.formula(paste(deparse(list_target), "~",paste(xnames2, collapse = " + ")))

Model3 <- lm(price ~ ., data=df_features)
summary(Model3)
