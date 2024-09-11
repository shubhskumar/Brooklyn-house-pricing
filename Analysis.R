library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(lmtest)


filtered_data <- read.csv("filtered_data.csv",header=TRUE)
head(filtered_data)
# Converting to date format
'''
mdy <- mdy(filtered_data$date) 
dmy <- dmy(filtered_data$date) 
mdy[is.na(mdy)] <- dmy[is.na(mdy)] 
filtered_data$date <- mdy 
str(filtered_data)
'''

data_2020 <- filtered_data %>%
  filter(year(date) == 2020)

data_2020 <- data_2020 %>%
  mutate(month = month(date))
class(data_2020$month) = "numeric"

head(data_2020)

# Making a separate column indicating whether quarter 3 or 4
data_2020 <- data_2020 %>%
  mutate(quarter = case_when(month>6 & month<10  ~ "3",
                             month>9 & month<13  ~ "4")) %>%
  filter(quarter %in% c("3", "4"))

class(data_2020$quarter) = "character"
head(data_2020)


# EXPLORATORY DATA ANALYSIS
new_filtered_data <- data_2020
new_filtered_data$neighborhood <- as.factor(new_filtered_data$neighborhood)
ggplot(new_filtered_data, aes(x = neighborhood, y = price)) +
  geom_point() +
  labs(title = "Price vs Neighborhood",
       x = "Neighborhood",
       y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Histogram
ggplot(data_2020, aes(price)) +
  geom_histogram(fill = "lightblue", color = "grey30") +
  scale_x_continuous(labels = scales::number_format(scale = 1e-6)) +
  labs(x = "Price (in millions)") +
  facet_wrap(~ quarter) + ggtitle("Distribution of Brooklyn Home Prices for Q3 and Q4 2020") 

# Mean comparison
summary(data_2020 %>% filter(quarter == 3) %>% .$price)
summary(data_2020 %>% filter(quarter == 4) %>% .$price)




# Conducting a t-test to check difference in means
t.test(data_2020$price[data_2020$quarter == '3'], data_2020$price[data_2020$quarter == '4'])
# p-value of 0.15 > 0.05 indicates that there is no sufficient evidence to conclude that there is a significant difference in means between quarter 3 and quarter 4

# MODELING 

# Adding the quarter as a predictor to my previous model is needed so that the question whether house prices changed between Q3 and Q4 can be answered.
lm_new = lm(price ~ quarter + grosssqft + neighborhood, data = data_2020)
summary(lm_new)
# Not taking sqrt of grosssqft because model is doing better without it in this case
# Getting R^2 = 0.7, RMSE = 444k... so model has good predictive power
# 1. Normality test
qqnorm(lm_new$residuals)
qqline(lm_new$residuals)

shapiro.test(lm_new$residuals)
# Not normal!

#2. Serial correlation test
dwtest(lm_new)
residuals <- residuals(lm_new)
acf(residuals, main = "ACF Plot of Residuals", lag.max = 20)
# There is no auto-correlation

# 3. Heteroskedasticity test
bptest(lm_new)
plot(lm_new$fitted.values, lm_new$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted")

# There is heteroskedasticity

ggplot(data_2020, aes(x=grosssqft, y=price, color=quarter)) + geom_point() +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6)) +
  labs(y = "Price (in millions)") +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")  + ggtitle("Brooklyn House Prices vs Gross Square Feet - Q3 to Q4") 

unique_neighborhoods <- unique(data_2020$neighborhood)
print(unique_neighborhoods)
data_2020$neighborhood[data_2020$neighborhood == "KENSINGTON_AND_WILLIAMSBURG-NORTH"] <- "KSGTON_WBURG-N_OCEAN-S"
data_2020$neighborhood[data_2020$neighborhood == "FULTON_AND_FULTON_AND_WILLIAMSBURG_AND_OCN"] <- "FULTON_WBURG-S_OCEAN-N"
data_2020$neighborhood[data_2020$neighborhood == "MANHATTAN_BEACH_AND_PARK_SLOPE_SOUTH"] <- "MANHATTANBEACH_PARKSLOPE-S"


filtered_neighborhood_data <- filter(data_2020, neighborhood=="BROOKLYN HEIGHTS" |neighborhood=="PARK SLOPE" |neighborhood=="FULTON_WBURG-S_OCEAN-N"
                                     |neighborhood=="KSGTON_WBURG-N_OCEAN-S" | neighborhood=="BOERUM HILL"
                                     |neighborhood=="MANHATTANBEACH_PARKSLOPE-S" |neighborhood=="BOROUGH PARK")

ggplot(data = filtered_neighborhood_data, aes(x = neighborhood, y = price, color = as.factor(quarter))) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.7) +  # Add jitter for better visibility
  labs(title = "Price vs Neighborhood for Quarters 3 and 4", x = "Neighborhood", y = "Price (in millions") +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6)) +
  scale_color_manual(values = c("3" = "blue", "4" = "red"), name = "Quarter") +  # Customize colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

ggplot(filtered_neighborhood_data, aes(x=neighborhood, y=price, fill=quarter)) + 
  geom_boxplot() + ggtitle("Home Prices for Neighborhoods, 2020 Q3 to Q4")+ 
  scale_y_continuous(labels = scales::number_format(scale = 1e-6)) +
  labs(y = "Price (in millions)") +
  theme(axis.title = element_text(size = 16),axis.text = element_text(angle=90, size= 5, color = "black"))





library(dplyr)
library(knitr)

# Filter data for quarter 3 in 2020 and extract the 'price' column
prices_2020_q4 <- data_2020 %>%
  filter(quarter == 4) %>%
  .$price

# Summary statistics
summary_stats <- summary(prices_2020_q4)

# Convert summary statistics to a data frame
summary_df <- as.data.frame(matrix(c(summary_stats), ncol = 6, byrow = TRUE))
colnames(summary_df) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")

# Print the summary table
kable(summary_df, caption = "Summary Statistics for Prices in Quarter 4 of 2020")


head(data_2020)
