# data-analysis-R

3. Mental health service availability, suicide rates, and GDP per capita (By Weijie) 
The analysis of the relationship between mental health service availability, suicide rates, and GDP per capita worldwide is a significant area of research that aims to explore the complex interplay between mental health infrastructure, socio-economic factors, and public health outcomes. This multifaceted analysis investigates how the provision of mental health services, the prevalence of suicide rates, and the level of economic development are interconnected, influencing the mental well-being and overall health of populations on a global scale.

3. 1. Data Collection
The data used in this study are sourced from reputable organizations—the World Health Organization (WHO) and the World Bank—which offer comprehensive and reliable datasets on mental health services and economic indicators, respectively.
The dataset on mental health service availability is obtained from the WHO Global Health Observatory (GHO). It presents information on the presence and density of mental health facilities and services across different countries and territories. The dataset on suicide rates is also collected from the WHO GHO, focusing on age-specific suicide rates to offer a deeper understanding of suicide patterns. Additionally, the analysis includes GDP per capita data from the World Bank, providing insights into the economic prosperity of nations. 
The data mentioned above can be downloaded in CSV file format. We use the built-in function “read.csv” to read them:
# Reading data from CSV files
df_mh <- read.csv("Mental_health_service_availability_facilities.csv")
df_sr <- read.csv("suicide_rate.csv")
df_gpc <- read.csv("gdpPerCapita.csv")


3. 2. Data Cleaning and Preprocessing
Raw data often contain errors, missing values, outliers, and inconsistencies. Data cleaning involves removing or correcting these issues to ensure the data is accurate and reliable. Data Preprocessing includes transforming, normalizing, scaling, and standardizing the data to make it suitable for analysis. The code performs data cleaning and data preprocessing in the following aspects:
a. It extracts interesting data and handles missing values by filling them with the median value of the respective columns. The reason for using the median is that it is less sensitive to extreme values (outliers) in the data compared to the mean.
# Handling missing values in mental health service availability data
for (i in 2:ncol(df_mh)) {
  df_mh[, i][is.na(df_mh[, i])] <- median(df_mh[, i], na.rm=TRUE)
}

# Filtering suicide rate data to keep only "Both sexes" rows
filtered_df_sr <- df_sr[df_sr$X.1 == ' Both sexes',]

# Handling missing values (where a value equal 0) in suicide rate data
for (i in 2:ncol(filtered_df_sr)) {
  filtered_df_sr[, i][filtered_df_sr[, i] == 0] <- median(filtered_df_sr[, i], na.rm=TRUE)
}

# Extracting GDP per capita data of year 2016 and handling missing values (where a value equal 0)
filtered_df_gpc <- df_gpc[, c("Country.Name", "X2016")]
for (i in 2:ncol(filtered_df_gpc)) {
  filtered_df_gpc[, i][filtered_df_gpc[, i] == 0] <- median(filtered_df_gpc[, i], na.rm=TRUE)
}

b. It converts the data from character type to numeric type to ensure that mathematical operations and calculations can be performed accurately on the data.
# Check the data type using “mode”.
mode(filtered_df_sr$Crude.suicide.rates..per.100.000.population.) #character
# Convert columns containing numbers in character format to numeric using “as.numeric”.
for (col in colnames(filtered_df_sr[, 3:ncol(filtered_df_sr)])) {
  filtered_df_sr[[col]] = as.numeric(filtered_df_sr[[col]])
}
# Check if the data type is converted.
mode(filtered_df_sr$Crude.suicide.rates..per.100.000.population.) #numeric

c. It standardizes the data to make it suitable for analysis. The columns in the mental health service availability data and suicide rate data are standardized with the self-defined function “standardize” to bring their features to the same scale and facilitates comparison.
# Apply the formula z = (xi - x~) / sx to standardize each column, where x~ is the mean and sx is the standard deviation.
standardize <- function(df){
  standardized_df <- df
  for (i in 1:ncol(df)) {
    mean <- mean(df[,i])
    sd <- sd(df[,i])
    for (j in 1:nrow(df)) {
      standardized_df[j,i] <- (df[j,i] - mean) / sd
    }
  }
  return(standardized_df)
}


# Data standardization applied to the numeric columns of mental health service availability data
df_mh[,3:ncol(df_mh)] <- standardize(df_mh[,3:ncol(df_mh)])

# Data standardization applied to the numeric columns of suicide rate data
df_sr[,3:ncol(df_sr)] <- standardize(df_sr[,3:ncol(df_sr)])


3. 3. Data Modeling
The code calculates scores based on weighted features, which is a form of modeling. It assigns weights to the features representing mental health service availability and calculates the sum of all weighted features as a score for each country or region.
The reason for using weighted features is that the initial sorting of data based on individual indicators, as done in the built-in function "order," may not provide a comprehensive representation of the mental health service landscape.
# Consider sorting the mental health service availability with “order” and using the order of each country or region as a score.
# When using multiple parameters for sorting, the “order” function prioritizes the values of the first parameter and only considers subsequent parameters when values of the first are equal. This method inadvertently focuses heavily on one key indicator, the number of specialized hospitals offering psychological treatment.
sorted_df_mh = df_mh[order(
df_mh$Mental.hospitals..per.100.000.population., df_mh$Mental.health.units.in.general.hospitals..per.100.000.population., df_mh$Mental.health.outpatient.facilities..per.100.000.population., df_mh$Mental.health.day.treatment.facilities..per.100.000.population., df_mh$Community.residential.facilities..per.100.000.population.),]

By adopting a weighted sum approach, the analysis addresses the limitations of the previous sorting method. It allows a more nuanced ranking that considers the contribution of each mental health facility type to the overall mental health service availability score. Facilities with higher weights, such as specialized hospitals, will have a more significant impact on the final rankings, reflecting their greater importance in the mental health system.
# Define weights representing the importance of each feature in mental health service availability data
# Assume that the importance of each feature decreases twice from left to right.
weights_mh <- c(16, 8, 4, 2, 1)

# Calculating composite scores for each country based on weighted features
weighted_df_mh <- df_mh
for (i in 1:nrow(df_mh)) {
# Use vectorized multiplication instead of for loop here.
  weighted_df_mh[i, 3:ncol(weighted_df_mh)] <- df_mh[i, 3:ncol(df_mh)] * weights_mh
}

# Calculate the sum of weighted features for each row and add it as the last column "Score".
weighted_df_mh$Score <- rowSums(weighted_df_mh[,3:ncol(weighted_df_mh)])

It should be pointed out that when observing the suicide rates of different age groups, there is no obvious need for weighted summation. As for the GDP per capita data, since the extracted data frame has only one column except for the country or region name, there is no need for weighted summation.
3. 4. Data Integration
Data integration involves combining data from multiple sources or datasets into a single, comprehensive dataset for further analysis. The R language provides a function named “merge” for the integration of two data frames. The merging is performed based on a common column, which is the country or region name.
To minimize the impact of outliers, the code removes the 10 largest and the 10 smallest values in each data frame before merging them.
# Sort the rows in mental health service availability dataframe based on the column "Score"
sortedByScore_df_mh <- weighted_df_mh[order(weighted_df_mh$Score),]
# Remove the first and last 10 rows of the sorted dataframe
sortedByScore_df_mh <- sortedByScore_df_mh[10:(nrow(sortedByScore_df_mh) - 10),]
weighted_df_mh <- sortedByScore_df_mh

# Sort the rows in suicide rate dataframe based on the column "Score"
sortedByScore_df_sr <- df_sr[order(df_sr$Score),]
# Remove the first and last 10 rows of the sorted dataframe
sortedByScore_df_sr <- sortedByScore_df_sr[10:(nrow(sortedByScore_df_sr) - 10),]
df_sr <- sortedByScore_df_sr

# Sort the rows in gdp per capita dataframe based on the column "X2016"
sortedByGDP_df_gpc <- df_gpc[order(df_gpc$X2016),]
# Remove the first and last 10 rows of the sorted dataframe
sortedByGDP_df_gpc <- sortedByGDP_df_gpc[10:(nrow(sortedByGDP_df_gpc) - 10),]
df_gpc <- sortedByGDP_df_gpc

The code merges data frame pairs after the removal of outliers.
# Merge dataframe pairs based on the common country or region name column
merged_df_mh_sr <- merge(weighted_df_mh, df_sr)
merged_df_sr_gpc <- merge(df_sr, df_gpc)
merged_df_mh_gpc <- merge(weighted_df_mh, df_gpc)

3. 5. Data Interpretation
The correlation coefficient is a statistical measure that quantifies the strength and direction of the linear relationship between two variables. A correlation coefficient is a value in the range from −1 to +1, where ±1 indicates the strongest possible agreement and 0 the strongest possible disagreement. The built-in function “cor” of  R can be used to calculate the correlation coefficient of two certain columns in a data frame.
# Calculate correlation between mental health service availability and suicide rate
correlation_mh_sr = cor(merged_df_mh_sr$Score.x, merged_df_mh_sr$Score.y)
# The result indicates low negative correlation between the two variables.


# Calculate correlation between suicide rate and GDP per capita
correlation_sr_gpc = cor(merged_df_sr_gpc$Score, merged_df_sr_gpc$X2016)
# The result indicates a moderate negative correlation.

# Calculate correlation between mental health service availability and GDP per capita
correlation_mh_gpc = cor(merged_df_mh_gpc$Score, merged_df_mh_gpc$X2016)
# correlation3's calculation result indicates a moderate positive correlation.

The correlation coefficient (correlation_mh_sr) between mental health service availability and suicide rates is approximately 0.0056. This value indicates a very weak linear relationship between these two variables. It suggests that there is almost no linear association between mental health service availability and suicide rates worldwide.
The correlation coefficient (correlation_sr_gpc) between suicide rates and GDP per capita is approximately -0.1386. This value indicates a weak negative correlation between the two variables. It implies that there is a small negative relationship between suicide rates and GDP per capita. However, the correlation is not strong, and other factors may have a more significant impact on suicide rates.
The correlation coefficient (correlation_mh_gpc) between GDP per capita and mental health service availability is approximately 0.3962. This value indicates a moderately positive correlation between the two variables. It suggests that there is a moderate positive relationship between GDP per capita and mental health service availability worldwide. As GDP per capita increases, the availability of mental health services also tends to increase, which aligns with common sense.
3. 6. Data Visualization
The code includes various visualization techniques like boxplots, histograms, and scatter plots to present the data and analysis results visually. For example, the code draws a histogram (Figure 3. 1) and a boxplot (Figure 3. 2) for the scores in mental health service availability data.
# Call hist and boxplot to visualize the data distribution in mental health service availability
hist(weighted_df_mh$Score, main="MH_Histogram", xlab="Score", ylab="Frequency")
boxplot(weighted_df_mh$Score, main="MH_Boxplot", ylab="Score")

![image](https://github.com/zhang-weijie/data-analysis-R/assets/60659396/079e3dbb-1401-4ceb-af7a-c9b58f505124)

The histogram shows that the scores are concentrated in the lower intervals (-10 ~ 0), which is consistent with the distribution reflected by the box plot, where the minimum, the first quartile, the median, and the third quartile of the data are all between -10 and 0. Also, the box plot indicates that there are about 10 oversized outliers in the data.
In addition to histograms and boxplots, the code also draws scatter plots to show correlations between different data.
# Generate a scatter plot
# Set margins (top, right, bottom, left)
par(mar = c(4, 4, 2, 2))

plot(merged_df_mh_sr$Score.x, merged_df_mh_sr$Score.y, main = "MH_SR", xlab = "Mental Health Service Availability", ylab = "Suicide Rate")

plot(merged_df_sr_gpc$Score, merged_df_sr_gpc$X2016, main = "SR_GPC", xlab = "Suicide Rate", ylab = "GDP per capita")

plot(merged_df_mh_gpc$Score, merged_df_mh_gpc$X2016, main = "MH_GPC", xlab = "Mental Health Service Availability", ylab = "GDP per capita")

![image](https://github.com/zhang-weijie/data-analysis-R/assets/60659396/75532fae-f9d6-4fbd-9e81-dd6350bf7439)


It can be seen that the degree of linear association of the data in the three scatter plots increases successively, which is consistent with the three correlation coefficients calculated in the data interpretation section.
