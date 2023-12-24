library(readr)
library(dplyr)

setwd(choose.dir())

# Mobile Price
mobile_price <- read_csv("MobilePrice_train.csv") %>% as.data.frame()


# TOP 1000 Twitch Streamers
twitch <- read_csv("twitchdata-update.csv") %>% as.data.frame()


# Airbnb

calendar <- read_csv(paste0(getwd(),"/Airbnb/calendar.csv")) %>% as.data.frame()
listing <- read_csv(paste0(getwd(),"/Airbnb/listings.csv")) %>% as.data.frame()
listing_detailed <- read_csv(paste0(getwd(),"/Airbnb/listings_detailed.csv")) %>% as.data.frame()
neighbourhoods <- read_csv(paste0(getwd(),"/Airbnb/neighbourhoods.csv")) %>% as.data.frame()
reviews <- read_csv(paste0(getwd(),"/Airbnb/reviews.csv")) %>% as.data.frame()
reviews_detailed <- read_csv(paste0(getwd(),"/Airbnb/reviews_detailed.csv")) %>% as.data.frame()




# Existen páginas analizándolos y eso no le va a molar la profe

# Customer Personality
customer_personality <- read.csv2("CustomerPersonality.csv", sep = "\t")

# Wine Quality
wine_quality <- read_csv("WineQT.csv") %>% as.data.frame()
