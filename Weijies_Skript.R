# R script by Weijie
# The code here is only partly referenced in the paper
df_mh <- read.csv("./Mental_health_service_availability_facilities.csv")
# Note that the values in the column “year” are different, which may affect the analysis; we need to observe if the span of years is large. If it's small, we can ignore it.
# Observe extreme values in the years using built-in functions df[which.min(df$colname),] and df[which.max(df$colname),].
yearMin = df[which.min(df_mh$Year),] # yearMin is 2013
yearMax = df[which.max(df_mh$Year),] # yearMax is 2017
yearSpan <- yearMax$Year - yearMin$Year # The maximum year span is 4. Considering changes in mental health facilities within 4 years may not be significant across countries, and there are no more accurate statistics available, we'll treat this dataset as data from the same year.

# Call built-in function to analyze the dataset
summary(df_mh)

# Check the column names of the dataset
colnames(df_mh)
# The column names in the dataset are:
# [1] "Countries..territories.and.areas"
# [2] "Year"
# [3] "Mental.hospitals..per.100.000.population."
# [4] "Mental.health.units.in.general.hospitals..per.100.000.population."
# [5] "Mental.health.outpatient.facilities..per.100.000.population."
# [6] "Mental.health.day.treatment.facilities..per.100.000.population."
# [7] "Community.residential.facilities..per.100.000.population."

# Calculate means
means <- colMeans(df_mh[, 3:ncol(df_mh)], na.rm = TRUE)
# The result for means is num [1:5] 0.219 0.342 2.427 3.063 0.962

# Calculate medians
medians <- sapply(df_mh[, 3:ncol(df_mh)], median, na.rm = TRUE)
# The result for medians is num [1:5] 0.062 0.137 0.718 0.268 0.224

# Calculate standard deviations
sds <- sapply(df_mh[, 3:ncol(df_mh)], sd, na.rm = TRUE)
# The result for sds is num [1:5] 0.764 0.617 6.071 10.486 1.528

# Calculate variances = sd ^ 2
vars <- sapply(df_mh[, 3:ncol(df_mh)], var, na.rm = TRUE)
# The result for vars is num [1:5] 0.583 0.38 36.862 109.947 2.334

# Calculate ranges
ranges <- sapply(df_mh[, 3:ncol(df_mh)], range, na.rm = TRUE)
# The result for ranges is a 2x5 table, which has a different format from means, medians, sds, and vars.

# Define a function to convert ranges to strings of the form (min, max)
range2Str <- function(ranges) {
  rangeStrs <- c()
  for (i in 1:ncol(ranges)) {
    rangeMin <- ranges[1,i]
    rangeMax <- ranges[2,i]
    rangeStr <- paste("(", min(rangeMin, rangeMax), ", ", max(rangeMin, rangeMax), ")", sep = "")
    rangeStrs <- c(rangeStrs, rangeStr)
  }
  return(rangeStrs)
}

# Apply the self-defined function “range2Str” to the 2x5 table obtained using the built-in function “range”
rangeStr <- range2Str(ranges)
# The result for rangeStr is chr [1:5] "(0, 8.314)" "(0.006, 4.542)" "(0.006, 61.387)" "(0.002, 61.387)" "(0.006, 6.178)"

# Check the number of NAs (missing data) in each column
# Use build-in functions “colSums” and “is.na” to calculate the number of NAs in each column
naCounts <- colSums(is.na(df_mh))
# The result for naCounts is num [1:7] 0 0 32 25 24 89 98

# Fill NA-data with the column median. Median is used because it's more robust than mean, especially when there are many extreme values in the dataset.
for (i in 2:ncol(df_mh)) {
  df_mh[, i][is.na(df_mh[, i])] <- median(df_mh[, i], na.rm = TRUE)
}

# Check the number of NAs after filling with medians
naCounts2 <- colSums(is.na(df_mh))
# The result for naCounts2 is num [1:7] 0 0 0 0 0 0 0
# we see that the original NA-data are replaced

# Considering that the importance of facilities represented by the columns decreases from left to right (e.g., the weight of specialized hospitals offering psychological treatment should be higher than general hospitals with mental health units), we sort all data accordingly.
sorted_df_mh = df_mh[order(df_mh$Mental.hospitals..per.100.000.population., df_mh$Mental.health.units.in.general.hospitals..per.100.000.population., df_mh$Mental.health.outpatient.facilities..per.100.000.population., df_mh$Mental.health.day.treatment.facilities..per.100.000.population., df_mh$Community.residential.facilities..per.100.000.population.),]

# Check the first 10 rows of the sorted table using two different methods
head(sorted_df_mh, 10)
sorted_df_mh[1:10, 1]


# Check the last 10 rows of the sorted table using two different methods
tail(sorted_df_mh, 10)
sorted_df_mh[(nrow(sorted_df_mh) - 9):nrow(sorted_df_mh), 1]

# The previous sorting may not be entirely accurate. When using multiple parameters  in the built-in  function “order”, it uses the parameters one by one and determines the sorting order. If the values of the first parameter are the same, it continues to compare the next parameter, and so on.
# This means that not all indicators are considered during sorting. In fact, almost only the number of specialized hospitals offering psychological treatment, which is the most important mental health facility, is used as the sorting criterion.
# Consider using a more reasonable sorting method by sorting based on the weighted sum of standardized values in each column.
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

df_mh[,3:ncol(df_mh)] <- standardize(df_mh[,3:ncol(df_mh)])

# Set weights for each feature (except the Year) to score each country
# We assume that the influence of each feature decreases twice from left to right.
weights_mh <- c(16, 8, 4, 2, 1)

# Apply weights to each feature in each row
weighted_df_mh <- df_mh
for (i in 1:nrow(df_mh)) {
  weighted_df_mh[i, 3:ncol(weighted_df_mh)] <- df_mh[i, 3:ncol(df_mh)] * weights_mh
}

# Calculate the sum of scores for each row and add it as the last column "Score"
weighted_df_mh$Score <- rowSums(weighted_df_mh[,3:ncol(weighted_df_mh)])

# Sort the columns based on the "Score"
sortedByScore_df_mh <- weighted_df_mh[order(weighted_df_mh$Score),]

# Remove the extreme data from the first and last 10 rows of the sorted table
sortedByScore_df_mh <- sortedByScore_df_mh[10:(nrow(sortedByScore_df_mh) - 10),]
weighted_df_mh <- sortedByScore_df_mh

# Call summary and boxplot to visualize the data distribution
hist(weighted_df_mh$Score)
summary(weighted_df_mh$Score)
boxplot(weighted_df_mh$Score)

# Import another dataframe which stores the suicide rate of the population of each age group (grouped by 10 years old) in 2016 in countries and regions of the world.
df_sr <- read.csv("./suicide_rate.csv")

# Remove gender-specific data to consider both sexes only
df_sr <- df_sr[df_sr$X.1 == ' Both sexes',]

# Check if the data in df_sr is usable for calculations (i.e., numeric type); if not, convert it to numeric type.
mode(df_sr$Crude.suicide.rates..per.100.000.population.)
# Note that the values in df_sr are in character format instead of numeric.

# Convert columns containing numbers in character format to numeric using the built-in function “as.numeric”.
for (col in colnames(df_sr[, 3:ncol(df_sr)])) {
  df_sr[[col]] = as.numeric(df_sr[[col]])
}
mode(df_sr$Crude.suicide.rates..per.100.000.population.)
# Note that the data types in “df3” are converted to numeric.

# Notice that there are many zeros in df_sr. Consider using column medians to replace them.
for (i in 2:ncol(df_sr)) {
  df_sr[, i][df_sr[, i] == 0] <- median(df_sr[, i], na.rm = TRUE)
}

# Standardize the columns in df_sr
df_sr[, 3:ncol(df_sr)] <- standardize(df_sr[, 3:ncol(df_sr)])

# Calculate the sum of scores for each row and add it as the last column "Score" to weightedDf3
df_sr$Score <- rowSums(df_sr[, 3:ncol(df_sr)])

# Sort the columns based on the "Score"
sortedByScore_df_sr <- df_sr[order(df_sr$Score),]

# Remove the extreme data from the first and last 10 rows of the sorted table
sortedByScore_df_sr <- sortedByScore_df_sr[10:(nrow(sortedByScore_df_sr) - 10),]
df_sr <- sortedByScore_df_sr

# Call summary and boxplot to visualize the data distribution
hist(df_sr$Score)
summary(df_sr$Score)
boxplot(df_sr$Score)

# Consider calling R's cor() function to examine the relationship between mental health service availability scores and suicide rate scores for different countries.
# This operation assumes the prior merging of two dataframes, namely weighted_df_mh and weighted_df_sr, using R's merge() function.
# Use R's all() function to compare whether the columns containing country names in both tables are the same.
is_same <- all(weighted_df_mh$Countries..territories.and.areas == df_sr$X)
print(is_same) # False

# The result indicates that the two dataframes have different country columns' lengths, so it is necessary to extract the common elements.
print(dim(weighted_df_mh)) # 163 * 8
print(dim(df_sr)) # 183 * 11

# Extract rows from weighted_df_mh and weighted_df_sr with matching country names, creating a new dataframe.
# Since weighted_df_mh has fewer rows, it serves as the basis for excluding the surplus rows from weighted_df_sr based on country names.
col <- colnames(weighted_df_mh)[1]
colnames(df_sr)[1] <- col

# Merge the two dataframes based on 'col', which represents country names.
merged_df_mh_sr <- merge(weighted_df_mh, df_sr, by = col)


# Examine the correlation between mental health service availability and suicide rates
correlation_mh_sr = cor(merged_df_mh_sr$Score.x, merged_df_mh_sr$Score.y)
# The correlation result is -0.01451792, indicating a very weak linear relationship between mental health service availability and suicide rates.

# Generate a scatter plot
par(mar = c(4, 4, 2, 2))  # Set margins (top, right, bottom, left)
plot(merged_df_mh_sr$Score.x, merged_df_mh_sr$Score.y, main = "MH_SR", xlab = "Mental Health Service Availability", ylab = "Suicide Rate")
# Since the absolute value of the correlation is very small, there is no obvious linear distribution in the scatter plot.

# Now, let's examine the relationship between GDP per capita and suicide rates
df_gpc <- read.csv("./gdpPerCapita.csv")
# Since the GDP per capita table contains data for multiple years, we need to filter and extract the columns "Country.Name" and "X2016".
df_gpc <- df_gpc[, c("Country.Name", "X2016")]

# Check if the data type is numeric; if not, convert it to numeric.
mode(df_gpc$X2016)
# The result is numeric.

# Replace NA with the median of the column since some rows have entire columns with no data.
for (i in 2:ncol(df_gpc)) {
  df_gpc[, i][df_gpc[, i] == 0] <- median(df_gpc[, i], na.rm = TRUE)
}

# Sort the rows in gdp per capita dataframe based on the column "X2016"
sortedByGDP_df_gpc <- df_gpc[order(df_gpc$X2016),]
# Remove the first and last 10 rows of the sorted dataframe
sortedByGDP_df_gpc <- sortedByGDP_df_gpc[10:(nrow(sortedByGDP_df_gpc) - 10),]
df_gpc <- sortedByGDP_df_gpc

# Call summary and boxplot to visualize the data distribution
hist(df_gpc$X2016)
summary(df_gpc$X2016)
boxplot(df_gpc$X2016)

col2 <- colnames(df_sr)[1]
colnames(df_gpc)[1] <- col2
# Merge weighted_df_sr and filtered_df_gpc based on the common country names
merged_df_sr_gpc <- merge(df_sr, df_gpc)

correlation_sr_gpc = cor(merged_df_sr_gpc$Score, merged_df_sr_gpc$X2016)
# The correlation is -0.232942, indicating a weak negative correlation between suicide rates and GDP per capita.

# Generate a scatter plot
plot(merged_df_sr_gpc$Score, merged_df_sr_gpc$X2016, main = "SR_GPC", xlab = "Suicide Rate", ylab = "GDP per capita")

# Finally, let's compare mental health service availability and GDP per capita
col3 <- colnames(weighted_df_mh)[1]
colnames(df_gpc)[1] <- col3
merged_df_mh_gpc <- merge(weighted_df_mh, df_gpc)
correlation_mh_gpc = cor(merged_df_mh_gpc$Score, merged_df_mh_gpc$X2016)
print(correlation_mh_gpc)
# The correlation is 0.4695782, indicating a moderately positive correlation between GDP per capita and mental health service availability, which aligns with common sense.

# Generate a scatter plot
plot(merged_df_mh_gpc$Score, merged_df_mh_gpc$X2016, main = "MH_GPC", xlab = "Mental Health Service Availability", ylab = "GDP per capita")
