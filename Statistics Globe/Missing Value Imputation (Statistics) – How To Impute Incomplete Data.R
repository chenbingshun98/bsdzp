# Load data
data(airquality)

# Data summaries
head(airquality)

# Check for number of complete cases
sum(complete.cases(airquality))

# Install and load the R package mice
install.packages("mice")
library("mice")

# Impute missing data
imp <- mice(airquality, m = 1)

# Store imputed data as new data frame
airquality_imputed <- complete(imp)

# Data summaries of imputed data
head(airquality_imputed)
summary(airquality_imputed)
