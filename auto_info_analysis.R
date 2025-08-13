#task 3

# load raw dataset
d <- read.csv("auto_info.csv")


# converting columns into a numeric values to access them in the function
numeric_cols <- c("engine_size", "horsepower", "curb_weight", 
                  "fuel_efficiency", "age", "price")

# step 1 - data pre processing (clean empty values, duplicates, and outliers)

# identify outliers before cleaning 

# function to flag outliers using IQR
flag_outliers <- function(x){
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  return(x < lower | x > upper)
}

# apply the function to all numeric columns
outlier_flags <- lapply(d[numeric_cols], flag_outliers)

# count outliers per column
sapply(outlier_flags, sum)

#library for visualizing outliers
# library(ggplot2)

# Loop through numeric columns and plot boxplots
for(col in numeric_cols){
  p <- ggplot(d, aes(y = .data[[col]])) +
    geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.shape = 8) +
    ggtitle(paste("Boxplot for", col, "with Outliers")) +
    theme_minimal()
  
  print(p)  # print the ggplot object inside the loop
}



# dplyr package for removeing duplicates and outiers step
# library(dplyr)

# remove null values
d1 <- na.omit(d)

# remove duplicate rows
d2 <- distinct(d1)

# remove outliers with Inter Quartile Range function that loops through the columns
remove_outliers <- function(df, cols) {
  for (col in cols) {
    Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR_value
    upper <- Q3 + 1.5 * IQR_value
    df <- df[df[[col]] >= lower & df[[col]] <= upper, ]
  }
  return(df)
}

# remove outliers 
d3 <- remove_outliers(d2, numeric_cols)


#check if outliers are removed using previous two functions 
# function to flag outliers using IQR
flag_outliers <- function(x){
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  return(x < lower | x > upper)
}

# apply the function to all numeric columns
new_outlier_flags <- lapply(d3[numeric_cols], flag_outliers)

# count outliers per column
sapply(new_outlier_flags, sum)

# 1 OUTLIER DETECTED AFTER CLEANING! in fuel_efficiency

#visualize that outlier
ggplot(d3, aes(y = fuel_efficiency)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.shape = 8) +
  ggtitle("Fuel Efficiency Boxplot") +
  theme_minimal()

# check if remaining outlier is harmless (possible real life scenario)
summary(d3$fuel_efficiency)
d3$fuel_efficiency[which(new_outlier_flags$fuel_efficiency)]

# this shows that outlier 38.22 is above the 3rd quartile (28.48)
# but still within the plausible range of 10–50 mpg for vehicles
# based in the data dictionary provided


#library for visualizing outliers
# library(ggplot2)

# Loop through numeric columns and plot boxplots
for(col in numeric_cols){
  p <- ggplot(d3, aes(y = .data[[col]])) +
    geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.shape = 8) +
    ggtitle(paste("Boxplot for", col, "with New Outliers")) +
    theme_minimal()
  
  print(p)  # print the ggplot object inside the loop
}



# step 2 - draw box plots 