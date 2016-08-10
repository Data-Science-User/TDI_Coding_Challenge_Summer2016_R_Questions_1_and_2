# Upload libraries
library(readxl)
library(data.table)

# Upload CSV and Dictionary of Columns
data <-read.csv("Historic_Secured_Property_Tax_Rolls.csv")
dict <-read_excel("ASR-0001_DataDictionary_historic-secured-property-rolls.xlsx")

# Properties are uniquely defined by the "Block and Lot Number" column. 
# Most properties have assessements for more than one year, so look only at the latest
# assessment.

# Subset data frame by taking the most recent assessment per property 
temp<-data[with(data, # Sort data by year in descending order per property
                order(Block.and.Lot.Number,-Closed.Roll.Fiscal.Year)), ]
unique_data<-temp[!duplicated(temp$Block.and.Lot.Number), ] # Take first row

rm(temp) # Removed to save space

# Q1: What fraction of assessments are for properties of the most common class? 
# For now, consider all the assessments, even though some properties may be listed 
# more than once.

sort(prop.table(table(data$Property.Class.Code.Definition)), decreasing=TRUE)*100
## Dwelling is the most common property class assessed at 47.07253%

# Q2: What is the median assessed improvement value, considering only non-zero assessments?
# Consider only the latest assessment value for each property, which is uniquely identified
# by the "Block and Lot Number" column.

# Subset latest assessments by removing non-zero assessments
temp<-unique_data[ which(unique_data$Closed.Roll.Assessed.Improvement.Value > 0) , ]
# Obtain median
median(temp$Closed.Roll.Assessed.Improvement.Value)
## The median assessed improvement value is $209,240.
rm(temp) # Removed to save space

# Q3: Calculate the average improvement value (using only non-zero assessments) in each
# neighborhood. What is the difference between the greatest and least average values?

# Removed properties missing Neighborhood Code and Description and
# had zero assessments
temp<-unique_data[which(unique_data$Neighborhood.Code != "" & 
                          unique_data$Closed.Roll.Assessed.Improvement.Value > 0),]

# Calculate average improvement value per neighborhood
avg_imp<-aggregate(temp$Closed.Roll.Assessed.Improvement.Value, 
               list(temp$Neighborhood.Code.Definition), mean)

max(avg_imp$x) - min(avg_imp$x) # Difference between the greatest and least average values
## The difference between the greatest and least average values is $5,085,780.
rm(temp)

# Q4: What is the yearly growth rate of Land Values over the years covered by this data?
# Take a simplistic model: the value is given by P=P0ert, where t is measured in
# years. (A more complete model would give each property its own base price P(i)0P0(i).)
# Estimate r over all assessments with a non-zero land value. (Hint: Consider using linear
# regression and logarithms.)

# Removed properties with zero land values
temp<-data[ which(data$Closed.Roll.Assessed.Land.Value > 0),]

temp<-temp[with(temp, order(Closed.Roll.Fiscal.Year)), ]

# Calculate yearly growth rate
fit <- lm(log(Closed.Roll.Assessed.Land.Value) - log(1) ~ Closed.Roll.Fiscal.Year - 1, 
          data = temp)
summary(fit)
# R = 0.00588
rm(temp)

# Q5: We can use the property locations to estimate the areas of the neighborhoods.
# Represent each as an ellipse with semi-axes given by a single standard deviation of
# the longitude and latitude. What is the area, in square kilometers, of the largest
# neighborhood measured in this manner? Be sure to filter out invalid coordinates.
temp<-unique_data$Property.Location
tempp<-unique_data$Location


# Q6: What is the difference between the average number of units in buildings build in
# or after 1950, and that for buildings built before 1950? Consider only buildings that
# have non-zero units reported, and ignore buildings with obviously incorrect years.
# To try to avoid the effect of improvements to buildings, use the earliest record for
# each property, not the latest.

# Filter out buildings with obviously incorrect years
temp<-data[ which(data$Year.Property.Built >= 1791 & data$Year.Property.Built <= 2015
                  & data$Number.of.Units > 0),]

# Take the earliest record for each property
temp<-temp[with(temp, # Sort data by year in descending order per property
                order(Block.and.Lot.Number, Closed.Roll.Fiscal.Year)), ]
unique_temp<-temp[ !duplicated(temp$Block.and.Lot.Number), ] # Take first row

diff<-(mean(unique_temp[which(unique_temp$Year.Property.Built >= 1950),"Year.Property.Built"]) - 
  mean(unique_temp[which(unique_temp$Year.Property.Built < 1950),"Year.Property.Built"]))

# The difference in average units is 52.33512
rm(temp)

# Q7: Considering only properties with non-zero numbers of bedrooms and units, calculate
# the average number of bedrooms per unit in each zip code. Use the ratio of the means
# instead of the mean of the ratio. What is this ratio in the zip code where it achieves
# its maximum?
temp<-unique_data[ which(data$Number.of.Units > 0 & data$Number.of.Bedrooms > 0),]

ratio<-aggregate(temp[, c(11,14)], list(temp$Neighborhood.Code), mean)

ratio$ratio.means<-ratio[,2]/ratio[,3]

sort(ratio$ratio.means)
# The largest ratio of means of bedrooms per unit is 3.44966887.
rm(temp)

# Q8: Estimate how built-up each zip code is by comparing the total property area to the
# total lot area. What is the largest ratio of property area to surface area of all zip
# codes?

total_area<-aggregate(unique_data[, c(20,22)], list(unique_data$Neighborhood.Code), sum)
total_area$ratio<-total_area[,2]/total_area[,3]
sort(total_area$ratio)
# The largest ratio of total property area to total lot area is 8.15077208