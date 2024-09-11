**Brooklyn House Prices**

This project focuses on analyzing how **Brooklyn home purchase prices** shifted between **Q3 2020** and **Q4 2020**, utilizing a **linear regression model** built from housing data from 2016 to 2020.

## Table of Contents
- [Project Overview](#project-overview)
- [Data Overview](#data-overview)
- [Findings](#findings)
- [Methodology](#methodology)
- [Results](#results)
- [Limitations](#limitations)
- [Conclusion](#conclusion)

## Project Overview
The analysis aims to understand the change in Brooklyn house prices between Q3 and Q4 2020. It is based on a dataset comprising **13,100 transactions** of single-family residences and apartments/condos in Brooklyn. The model developed highlights key trends in housing prices, identifies significant factors affecting these prices, and examines neighborhood-level differences.

## Data Overview
- **Dataset**: Brooklyn housing data (2016-2020)
- **Transactions**: 13,100 transactions focused on single-family residences and single-unit apartments or condos.
- **Key Features**:
  - Borough Name
  - Neighborhood Name
  - Building Type
  - Tax Class
  - Zip Code
  - Gross Square Feet
  - Land Square Feet
  - Year Built
  - Sale Date

## Findings
- Between **Q3 2020** and **Q4 2020**, mean house prices increased from **$1,001,880** to **$1,085,676**, while median prices rose from **$750,000** to **$819,000**.
- The percentage change in mean house prices between Q3 and Q4 was approximately **8%**.
  
## Methodology
1. **Statistical t-test**: Used to evaluate the significance of the observed change in prices, resulting in a **p-value of 0.15**. This indicated a lack of strong statistical significance.
2. **Multiple Linear Regression**: A model was developed with **Price** as the response variable and predictors such as **Gross Square Footage**, **Quarter**, and **Neighborhoods** (grouped based on similar price distributions). 
   - The model yielded an **R-squared value of 0.7** and a **Root Mean Square Error (RMSE) of $444,000**.

   Regression Formula:
   ```math
   Price = β0 + (β1 * Quarter) + (β2 * Gross Square Feet) + Σ(βi+2 * Neighborhood(i)) + ε
   ```

3. **Visualization**:
   - **Histograms**: Showed significant increases in the number of homes sold within the **$0.5M to $1M** price range during Q4.
   - **Scatter Plots**: Illustrated differences in trends between Q3 and Q4 for **Gross Square Footage** vs **Sale Price**.
   - **Box Plots**: Highlighted variations in prices across the 7 most significant neighborhoods.

## Results
- **Price Distribution**: Significant increases in Q4 for the **$0.5M to $1M** range.
- **Neighborhood Differences**: Some areas like **Park Slope** and **Manhattan Beach** experienced price increases, while others like **Ocean Parkway North** saw decreases.
- **Regression Model**: The model effectively explained the variance in prices, with a notable impact from both gross square footage and neighborhood groupings.

## Limitations
- **Heteroskedasticity** and deviations from normality were identified, which could affect the model's reliability.
- Missing data for some neighborhoods (e.g., **Borough Park**) prevented a full comparison between Q3 and Q4 prices in those areas.
- The t-test indicated that the price change lacked statistical significance.

## Conclusion
This analysis highlights a notable **price change** in Brooklyn housing between **Q3 and Q4 2020**, although the t-test suggested the mean price change may not be statistically significant. The **regression model** and **visualizations** revealed important insights, particularly regarding neighborhood-specific trends. Further analysis, exploring **non-linear relationships** and **alternative models**, could provide deeper insights into Brooklyn’s housing market dynamics.
