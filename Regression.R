library(dplyr)
library(tidyverse)
library(lubridate) 
library(stringr)
library(ggplot2)
library(lmtest)

setwd("C:\\Users\\shubh\\OneDrive - Indian Institute of Technology Bombay\\Documents\\College\\Grad - UChicago\\Courses\\Autumn 2023\\Statistical Analysis\\Assignments\\Final Assignment")

# Defining the standardized column names
column_names <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement',
                  'bldclasscurr','address','aptnum','zip','resunits','comunits','totunits',
                  'landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

# Reading each of the files and standardize column names
data1 <- read.csv("2016_brooklyn.csv", header = TRUE, col.names = column_names)[-c(1:4), ]
data2 <- read.csv("2017_brooklyn.csv", header = TRUE, col.names = column_names)[-c(1:4), ]
data3 <- read.csv("2018_brooklyn.csv", header = TRUE, col.names = column_names)[-c(1:4), ]
data4 <- read.csv("2019_brooklyn.csv", header = TRUE, col.names = column_names)[-c(1:4), ]
data5 <- read.csv("2020_brooklyn.csv", header = TRUE, col.names = column_names)[-c(1:7), ]


# Combining all datasets into one
all_data <- bind_rows(data1, data2, data3, data4, data5)

# DATA CLEANING AND STANDARDIZATION
# Removing white spaces
all_data$neighborhood <- str_squish(all_data$neighborhood)
all_data$bldclasscat <- str_squish(all_data$bldclasscat)
all_data$bldclasssale <- str_squish(all_data$bldclasssale)
all_data$price <- str_squish(all_data$price)
all_data$grosssqft <- str_squish(all_data$grosssqft)

# Converting relevant columns to numeric type
all_data$resunits <- as.numeric(all_data$resunits)
all_data$comunits <- as.numeric(all_data$comunits)
all_data$totunits <- as.numeric(all_data$totunits)
all_data$yrbuilt <- as.numeric(all_data$yrbuilt)
all_data$landsqft <- as.numeric(gsub("[,]", "", all_data$landsqft))
all_data$grosssqft <- as.numeric(gsub("[,]", "", all_data$grosssqft))
all_data$price <- as.numeric(gsub("[$,]", "", all_data$price))
all_data$price <- str_squish(all_data$price)
class(all_data$price) = "numeric"

# Converting to date type: Initially all types of date formats are here, so I'm using the lubridate package for cleaning
mdy <- mdy(all_data$date) 
dmy <- dmy(all_data$date) 
mdy[is.na(mdy)] <- dmy[is.na(mdy)] 
all_data$date <- mdy 

summary(all_data)


# Applying the conditions stated in the problem
filtered_data <- all_data %>%
  filter(
    substr(bldclasssale, 1, 1) %in% c('A', 'R'),  # Building class starts with 'A' or 'R'
    totunits == 1,  # Number of total units is 1
    resunits == 1,  # Number of residential units is 1
    grosssqft > 0,  # Gross square footage is more than 0
    !is.na(price)   # Sale price is non-missing
  )

# I found one zip entry as 0, so I am removing it
filtered_data$zip <- as.numeric(filtered_data$zip)
filtered_data <- subset(filtered_data, zip != 0)

# I ended up with 19,639 rows after cleaning


# EXPLORATORY DATA ANALYSIS

# (a) Price vs date
ggplot(filtered_data, aes(x = date, y = price)) +
  geom_point() +
  labs(title = "Price vs Date",
       x = "Date",
       y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# (b) Price vs zip
ggplot(filtered_data, aes(x = zip, y = price)) +
  geom_point() +
  labs(title = "Price vs Zip",
       x = "Zip",
       y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# (c) Price vs gross sqft
plot(filtered_data$grosssqft, filtered_data$price)

# (d) Price vs bldclasssale
filtered_data$bldclasssale <- as.factor(filtered_data$bldclasssale)
ggplot(filtered_data, aes(x = bldclasssale, y = price)) +
  geom_point() +
  labs(title = "Price vs Bldclasssale",
       x = "Bldclasssale",
       y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# (e) Price vs Neighborhood
#filtered_data$neighborhood <- as.factor(filtered_data$neighborhood)
ggplot(filtered_data, aes(x = neighborhood, y = price)) +
  geom_point() +
  labs(title = "Price vs Neighborhood",
       x = "Neighborhood",
       y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# PRE-MODELING AND FEATURE ENGINEERING

# restricting the number of rows by removing outliers in the price
filtered_data <- filtered_data[filtered_data$price>7000, ]
filtered_data <- filtered_data[filtered_data$price<5500000, ]
## Finally, I now have 13,535 rows


## I would like to keep neighborhood in my model because qualitatively, it seems like an important feature.
## Initial model
lm1 <- lm(price ~ neighborhood + grosssqft + date, data = filtered_data)
summary(lm1)
## Getting R^2: 0.61; RMSE: 444,600; DoF: 62

## In neighborhood, there are too many degrees of freedom, so I am clubbing similar neighborhoods (based on price patterns from the EDA) together.
## I am taking the median of each neighborhood type and then I will use K-Means Clustering to get 48 groups
median_prices <- filtered_data %>%
  group_by(neighborhood) %>%
  summarize(median_price = median(price, na.rm = TRUE))

#print(median_prices, n=60)

data_for_clustering <- median_prices$median_price
num_clusters <- 44
kmeans_result <- kmeans(data_for_clustering, centers = num_clusters)
median_prices$cluster <- as.factor(kmeans_result$cluster)
print(median_prices)
#write.csv(median_prices, "median_prices.csv", row.names = FALSE)


filtered_data$neighborhood[filtered_data$neighborhood == "BERGEN BEACH"] <- "BERGEN_BEACH_AND_BUSHWICK"
filtered_data$neighborhood[filtered_data$neighborhood == "BUSHWICK"] <- "BERGEN_BEACH_AND_BUSHWICK"

filtered_data$neighborhood[filtered_data$neighborhood == "WILLIAMSBURG-CENTRAL"] <- "WYCKOFF_AND_WILLIAMSBURG-CENTRAL"
filtered_data$neighborhood[filtered_data$neighborhood == "WYCKOFF HEIGHTS"] <- "WYCKOFF_AND_WILLIAMSBURG-CENTRAL"

filtered_data$neighborhood[filtered_data$neighborhood == "FLATBUSH-CENTRAL"] <- "SUNSET_AND_FLATBUSH-CENTRAL_AND_CROWN"
filtered_data$neighborhood[filtered_data$neighborhood == "SUNSET PARK"] <- "SUNSET_AND_FLATBUSH-CENTRAL_AND_CROWN"
filtered_data$neighborhood[filtered_data$neighborhood == "CROWN HEIGHTS"] <- "SUNSET_AND_FLATBUSH-CENTRAL_AND_CROWN"

filtered_data$neighborhood[filtered_data$neighborhood == "MANHATTAN BEACH"] <- "MANHATTAN_BEACH_AND_PARK_SLOPE_SOUTH"
filtered_data$neighborhood[filtered_data$neighborhood == "PARK SLOPE SOUTH"] <- "MANHATTAN_BEACH_AND_PARK_SLOPE_SOUTH"

filtered_data$neighborhood[filtered_data$neighborhood == "BRIGHTON BEACH"] <- "BRIGHTON_BEACH_AND_OCEAN_HILL"
filtered_data$neighborhood[filtered_data$neighborhood == "OCEAN HILL"] <- "BRIGHTON_BEACH_AND_OCEAN_HILL"

filtered_data$neighborhood[filtered_data$neighborhood == "DOWNTOWN-FULTON FERRY"] <- "DOWTOWNFULTONFERRY_AND_REDHOOK"
filtered_data$neighborhood[filtered_data$neighborhood == "RED HOOK"] <- "DOWTOWNFULTONFERRY_AND_REDHOOK"

filtered_data$neighborhood[filtered_data$neighborhood == "DOWNTOWN-METROTECH"] <- "DOWNTOWN-METROTECH_AND_MILL_BASIN_AND_WILLIAMSBURG-EAST"
filtered_data$neighborhood[filtered_data$neighborhood == "MILL BASIN"] <- "DOWNTOWN-METROTECH_AND_MILL_BASIN_AND_WILLIAMSBURG-EAST"
filtered_data$neighborhood[filtered_data$neighborhood == "WILLIAMSBURG-EAST"] <- "DOWNTOWN-METROTECH_AND_MILL_BASIN_AND_WILLIAMSBURG-EAST"

filtered_data$neighborhood[filtered_data$neighborhood == "CANARSIE"] <- "CANARSIE_AND_GERRITSEN"
filtered_data$neighborhood[filtered_data$neighborhood == "GERRITSEN BEACH"] <- "CANARSIE_AND_GERRITSEN"

filtered_data$neighborhood[filtered_data$neighborhood == "BROWNSVILLE"] <- "BROWNSVILLE_AND_EASTNEWYORK"
filtered_data$neighborhood[filtered_data$neighborhood == "EAST NEW YORK"] <- "BROWNSVILLE_AND_EASTNEWYORK"

filtered_data$neighborhood[filtered_data$neighborhood == "KENSINGTON"] <- "KENSINGTON_AND_WILLIAMSBURG-NORTH"
filtered_data$neighborhood[filtered_data$neighborhood == "WILLIAMSBURG-NORTH"] <- "KENSINGTON_AND_WILLIAMSBURG-NORTH"
filtered_data$neighborhood[filtered_data$neighborhood == "OCEAN PARKWAY-SOUTH"] <- "KENSINGTON_AND_WILLIAMSBURG-NORTH"

filtered_data$neighborhood[filtered_data$neighborhood == "CONEY ISLAND"] <- "CONEYISLAND_AND_OLDMILLBASIN_AND_FLATLANDS_AND_FLATBUSH"
filtered_data$neighborhood[filtered_data$neighborhood == "OLD MILL BASIN"] <- "CONEYISLAND_AND_OLDMILLBASIN_AND_FLATLANDS_AND_FLATBUSH"
filtered_data$neighborhood[filtered_data$neighborhood == "FLATLANDS"] <- "CONEYISLAND_AND_OLDMILLBASIN_AND_FLATLANDS_AND_FLATBUSH"
filtered_data$neighborhood[filtered_data$neighborhood == "FLATBUSH-EAST"] <- "CONEYISLAND_AND_OLDMILLBASIN_AND_FLATLANDS_AND_FLATBUSH"

filtered_data$neighborhood[filtered_data$neighborhood == "DOWNTOWN-FULTON MALL"] <- "FULTON_AND_FULTON_AND_WILLIAMSBURG_AND_OCN"
filtered_data$neighborhood[filtered_data$neighborhood == "WILLIAMSBURG-SOUTH"] <- "FULTON_AND_FULTON_AND_WILLIAMSBURG_AND_OCN"
filtered_data$neighborhood[filtered_data$neighborhood == "OCEAN PARKWAY-NORTH"] <- "FULTON_AND_FULTON_AND_WILLIAMSBURG_AND_OCN"

#lm2 <- lm(price ~ neighborhood + sqrt(grosssqft) + zip, data = filtered_data)
#summary(lm2)

#lm3 <- lm(price ~ neighborhood + grosssqft + zip, data = filtered_data)
#summary(lm3)

# Removing non-significant neighborhoods
filtered_data <- filtered_data[filtered_data$neighborhood != "DYKER HEIGHTS", ]
filtered_data <- filtered_data[filtered_data$neighborhood != "BUSH TERMINAL", ]

set.seed(123) 
madison_indices <- which(filtered_data$neighborhood == "MADISON")
indices_to_remove <- sample(madison_indices, 150)
filtered_data <- filtered_data[-indices_to_remove, ]

## STOPPING POINT
lm_final <- lm(price ~ neighborhood + sqrt(grosssqft) + date, data = filtered_data)
summary(lm_final)

predictions <- predict(lm_final)

# Calculate residuals
residuals <- filtered_data$price - predictions

# Calculate RMSE
rmse <- sqrt(mean(residuals^2))

# Print RMSE
print(paste("RMSE:", rmse))

#write.csv(filtered_data, "filtered_data.csv", row.names = FALSE)

### FINAL: R-squared = 0.6011, RMSE = 449,398 , DoF = 41 and Observations used = 13,049

# Testing OLS Assumptions of final model

# 1. Normality test
qqnorm(lm_final$residuals)
qqline(lm_final$residuals)
# Not normal!

#2. Serial correlation test
dwtest(lm_final)
residuals <- residuals(lm_final)
acf(residuals, main = "ACF Plot of Residuals", lag.max = 20)
# There is auto-correlation

# 3. Heteroskedasticity test
bptest(lm_final)
plot(lm_final$fitted.values, lm_final$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted")

# There is heteroskedasticity

saveRDS(list(model=lm_final, data=filtered_data), file='ShubhankarKumar.RDS') 
