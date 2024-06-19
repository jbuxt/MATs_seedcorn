library(dplyr)
library(ggplot2)
library(tibble)
library(dplyr)
library(tmap)

#Price paid data accessed from HM Land Registry. Other years available, this code uses 2023.
#Can be found here: https://www.gov.uk/government/statistical-data-sets/price-paid-data-downloads
#Citation requirement: Contains HM Land Registry data © Crown copyright and database right 2021. This data is licensed under the Open Government Licence v3.0.

#Postcode data accessed from: https://www.getthedata.com/open-postcode-geo
#Multiple data citation requirements - please look into these (bottom of page)


#Import price data and add column names

price_data = read.csv("~/pp-2023.csv", header = FALSE)

colnames(price_data) <- c("Transcation_ID", "Price", "Date_of_Transfer","postcode","Property_Type",
                          "New_build", "Duration", "PAON", "SAON", "Street", "Locality",
                          "Town/City", "District", "County", "PPD Category Type", "Record Status - monhtly file only")


#Import postcode-lat/long data and add column names

post_code <- read.csv('~/open_postcode_geo.csv',header=FALSE)

colnames(post_code) <- c('postcode','status', 'usertype','easting','northing',
                         'positional_quality_indicator','country','latitude','longitude',
                         'postcode_no_space','postcode_fixed_width_seven','postcode_fixed_width_eight',
                         'postcode_area','postcode_district','postcode_sector','outcode','incode')

#Join both datasets by their postcode values 

price_data_geo_info <- price_data %>% 
  inner_join(post_code)

#Convert lat, long, price to numeric

price_data_geo_info$longitude <- as.numeric(price_data_geo_info$longitude)
price_data_geo_info$latitude <- as.numeric(price_data_geo_info$latitude)
price_data_geo_info$Price <- as.numeric(price_data_geo_info$Price)
price_data_geo_info$easting <- as.numeric(price_data_geo_info$easting)
price_data_geo_info$northing <- as.numeric(price_data_geo_info$northing)

#Calculate log of sale price
price_data_geo_info$log_Price <- log(price_data_geo_info$Price)



#Plot data with ggplot

ggplot() + 
  borders("world", fill = "white", colour = "grey80") +    # specific ggplot layer for world maps 
  coord_fixed(ratio = 1.3, 
              xlim = c(-10,3), 
              ylim = c(50, 59)) +
  geom_point( data = price_data_geo_info               # our data frame for this layer
              #-------- with aesthetic mapping you assign "varying" values to graph properties
              , aes(x = longitude, y = latitude,
                    colour = log_Price)  # mapping of variables to aesthetics (here: x- and y-position on our graph)
              #-------- then you can assign "fixed" (aka static) properties
              , size = 0.003            # plot all points with size 0.003
  ) +
  scale_color_gradient(low="blue", high="red", 
                        limits = c(9.1, 16), oob = scales::squish) +
  ggtitle('Logarithm of house sale price 2023')



#Export to csv
write.csv(price_data_geo_info,'~/2023_price_data_with_lat_long.csv')
