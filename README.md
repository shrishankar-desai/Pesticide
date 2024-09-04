# Pesticide
Forecasting Future Pesticide Usage Trend In US

This project aims to predict the patterns of pesticide use over time for various chemicals in local catchments and watersheds, to assess levels of contamination. It entails forecasting the spatial and temporal distribution of pesticide concentrations, drawing on historical data on usage and environmental conditions.Companies could utilize predictive analytics on this data to project future trends in pesticide usage. This approach would aid in strategic decision-making concerning production scheduling, inventory control, and market strategy.

Data Description: 
Data link: https://www.kaggle.com/datasets/konradb/pesticide-usage-in-the-united-states

The data on agricultural pesticide use was gathered through the USGS. This data aims to enhance the understanding of pesticide presence in freshwater and its effects on water availability across the United States.
This data consists of six variables namely:
1.COMPOUND: This represents the name or type of pesticide compound being used or applied.
2.YEAR: This represents the year in which the pesticide application occurred.
3.STATE_FIPS_CODE: This represents the Federal Information Processing Standards (FIPS) code for the state where the pesticide application took place.
4.COUNTY_FIPS_CODE: This represents the Federal Information Processing Standards (FIPS) code for the county where the pesticide application took place.
5.EPEST_LOW_KG: This represents the lower estimate of the amount of pesticide applied, measured in kilograms.
6.EPEST_HIGH_KG: This represents the higher estimate of the amount of pesticide applied, also measured in kilograms.

EPEST_HIGH_KG is the primary variable in this study as it represents the higher estimates of agricultural pesticide usage found in water bodies. The dataset comprises 1,048,576 entries, which were cleaned, and missing values addressed using the `na. interp` function. It was then converted into a time series format with annual data points. A logarithmic transformation was applied for normalization, followed by unit root testing to ensure stationarity. The data was subsequently divided into an 80% training set and a 20% testing set. The models employed in the analysis included Simple Exponential Smoothing (SES), Holt's Linear Trend Model, Exponential Smoothing State Space Model (ETS), ARIMA, and Dynamic Regression.
The ARIMA (1,0,1) model was determined to be the most effective based on the RMSE of the back-transformed training data and was used to forecast the next four years.
