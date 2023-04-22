
df_airbnb = read.csv("data/airbnb_europe_cities.csv")

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
