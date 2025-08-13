#task 3 - ct and bell curves

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

# remove outliers - d3 is the clean dataset now
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



# step 2 - get central tendencies for numerical columns

# view summary for individual columns
summary(d3$engine_size)
summary(d3$horsepower)
summary(d3$curb_weight)
summary(d3$fuel_efficiency)
summary(d3$age)
summary(d3$price)

# mode function
get_mode <- function(v) {
  uniq_vals <- unique(v)
  freq <- tabulate(match(v, uniq_vals))
  mode_val <- uniq_vals[which.max(freq)]
  return(mode_val)
}

# calculate modes
mode_engine_size <- get_mode(d3$engine_size)
mode_horsepower <- get_mode(d3$horsepower)
mode_curb_weight <- get_mode(d3$curb_weight)
mode_fuel_efficiency <- get_mode(d3$fuel_efficiency)
mode_age <- get_mode(d3$age)
mode_price <- get_mode(d3$price)

# display modes
mode_engine_size
mode_horsepower
mode_curb_weight
mode_fuel_efficiency
mode_age
mode_price


# standard deviation for each column
sd_values <- sapply(d3[numeric_cols], sd)
sd_values



# step 3 - bell curve
# using a function to efficiently get all plots by going through the 
# numerical column thing

# Function to plot bell curve
plot_bell_curve <- function(data, col_name){
  
  # Extract variable
  variable <- data[[col_name]]
  
  # compute central tendencies for plot
  mean_val <- mean(variable)
  median_val <- median(variable)
  mode_val <- get_mode(variable)
  sd_val <- sd(variable)
  
  # plot bell curve function
  p <- ggplot(data.frame(x = c(mean_val - 4*sd_val, mean_val + 4*sd_val)), aes(x)) +
    stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val),
                  color = "blue", linewidth = 1) +
    labs(title = paste("Bell Curve for", col_name),
         x = col_name, y = "Density") +
    geom_vline(xintercept = mean_val, color = "red", linetype = "dashed") +
    geom_vline(xintercept = median_val, color = "green", linetype = "dashed") +
    geom_vline(xintercept = mode_val, color = "purple", linetype = "dashed") +
    annotate("text", x = mean_val, y = 0.02,
             label = paste("Mean =", round(mean_val,2)), color="red", hjust=-0.1) +
    annotate("text", x = median_val, y = 0.018,
             label = paste("Median =", round(median_val,2)), color="darkgreen", hjust=-0.1) +
    annotate("text", x = mode_val, y = 0.016,
             label = paste("Mode =", round(mode_val,2)), color="purple", hjust=-0.1) +
    theme_bw()
  
  # save plot as png image
  ggsave(filename = paste0(col_name, "_bell_curve.png"), plot = p, width = 6, height = 4)
  
  return(p)
}

# loop through numeric columns and create bell curves
bell_curves <- lapply(numeric_cols, function(col) plot_bell_curve(d3, col))



# curb weight and price had flat bell curves



# Flexible bell curve function
plot_new_bell_curve <- function(data, col_name, transform = NULL, auto_scale = TRUE) {
  
  # Extract variable
  variable <- data[[col_name]]
  
  # Remove NA
  variable <- na.omit(variable)
  
  # Apply transformation if provided
  if (!is.null(transform)) {
    variable <- transform(variable)
    col_label <- paste0(deparse(substitute(transform)), "(", col_name, ")")
  } else {
    col_label <- col_name
  }
  
  # Auto scale if SD is huge
  sd_val <- sd(variable)
  if (auto_scale && sd_val > 100) {
    variable <- scale(variable)  # mean 0, sd 1
    sd_val <- sd(variable)
    col_label <- paste0(col_label, " (scaled)")
  }
  
  # Central tendencies
  mean_val <- mean(variable)
  median_val <- median(variable)
  mode_val <- get_mode(variable)
  
  # Plot range
  x_min <- mean_val - 4 * sd_val
  x_max <- mean_val + 4 * sd_val
  
  # Plot
  p <- ggplot(data.frame(x = c(x_min, x_max)), aes(x)) +
    stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), 
                  color = "blue", linewidth = 1) +
    labs(title = paste("Bell Curve for", col_label),
         x = col_label, y = "Density") +
    geom_vline(xintercept = mean_val, color = "red", linetype = "dashed") +
    geom_vline(xintercept = median_val, color = "green", linetype = "dashed") +
    geom_vline(xintercept = mode_val, color = "purple", linetype = "dashed") +
    annotate("text", x = mean_val, y = 0.02,
             label = paste("Mean =", round(mean_val,2)), color="red", hjust=-0.1) +
    annotate("text", x = median_val, y = 0.018,
             label = paste("Median =", round(median_val,2)), color="darkgreen", hjust=-0.1) +
    annotate("text", x = mode_val, y = 0.016,
             label = paste("Mode =", round(mode_val,2)), color="purple", hjust=-0.1) +
    theme_bw()
  
  # Save plot
  ggsave(filename = paste0(gsub("[()]", "_", col_label), "_new_bell_curve.png"), plot = p, width = 6, height = 4)
  
  return(p)
}


# bell curve  for flats with log to make more bell(y)
plot_new_bell_curve(d3, "curb_weight", transform = log)


plot_new_bell_curve(d3, "price", transform = log)




#non numerical column analysis
table(d3$vehicle_type)
prop.table(table(d3$vehicle_type)) * 100  # % of distribution

table(d3$brand)
prop.table(table(d3$brand)) * 100

# idk what this says





# task 4 - hypothesis testing: vehicle type - prive

# null H0: the mean price is the same for all vehicle types
# alternative H1: at least one vehicle type has a different mean price.

















