# Load the Data into R
getwd()
housing_data <- read.csv(file.choose(),h=T)
head(housing_data)
summary(housing_data)

# Check column names

cat("Column Names:\n")
print(colnames(housing_data))

# Rename column names

colnames(housing_data) <- c("Contributor_Name", "Address", "Zip_Code", "Price_Sold", 
                            "Date_Sold", "Bedrooms", "Bathrooms", "Year_Built", 
                            "Squared_Feet", "Lot_Size", "Parking_Spots")

cat("Rename column names:\n")
print(colnames(housing_data))

# Check Datatypes:

data_types <- sapply(housing_data, class)
cat("Data Types:",data_types,"\n")

# Checking Rows & Columns

num_rows <- nrow(housing_data)
num_cols <- ncol(housing_data)
cat("\nNumber of Rows:", num_rows, "\n")
cat("Number of Columns:", num_cols, "\n")


# Renaming N/A values to NA
housing_data[housing_data == "N/A"] <- NA


# Checking for NA values according to columns

null_values <- colSums(is.na(housing_data))
cat("\nNull Values:\n")
for (col_name in names(null_values)) {
  cat(col_name, ": ", null_values[col_name], "\n")
}

# Removing NA values

housing_data <- housing_data[complete.cases(housing_data$Bedrooms, housing_data$Bathrooms, 
                                            housing_data$Year_Built, housing_data$Squared_Feet, 
                                            housing_data$Lot_Size, housing_data$Price_Sold), ]


# Replacing the NA or Null values in parking spots with 0

housing_data$Parking_Spots[is.na(housing_data$Parking_Spots) | housing_data$Parking_Spots == ""] <- 0


# Check unique non-numeric values in Price_Sold and Squared_Feet, Lot_Size

unique_prices <- unique(housing_data$Price_Sold)
unique_sq_feet <- unique(housing_data$Squared_Feet)
unique_sq_feet <- unique(housing_data$Lot_Size)

unique_prices
unique_sq_feet
unique_sq_feet


# Convert Price_Sold, Squared_Feet, Lot_Size to numeric
housing_data$Price_Sold <- as.numeric(gsub("[^0-9.]", "", housing_data$Price_Sold))
housing_data$Squared_Feet <- as.numeric(gsub(",", "", housing_data$Squared_Feet))
housing_data$Lot_Size <- as.numeric(gsub(",", "", housing_data$Lot_Size))

housing_data$Zip.Code

# Visualizations
  # 1 Top 10 Zip codes with highest average selling price
average_price <- tapply(housing_data$Price_Sold, housing_data$Zip_Code, mean, na.rm = TRUE)

sorted_zip_codes <- sort(average_price, decreasing = TRUE)

top_10_zip_codes <- head(sorted_zip_codes, 10)

barplot(top_10_zip_codes, main = "Top 10 Zip Codes with Highest Average Price Sold",
        xlab = "Zip Code", ylab = "Average Price Sold", col = "purple", ylim = c(0, max(top_10_zip_codes) * 1.1))


  
  # 2 Zipcode with most sales 

zip_code_counts <- table(housing_data$Zip_Code)


sorted_zip_codes <- sort(zip_code_counts, decreasing = TRUE)


top_10_zip_codes <- head(sorted_zip_codes, 10)


pie(top_10_zip_codes, main = "Top 10 Zip Codes with Most Sales", 
    labels = names(top_10_zip_codes), col = rainbow(length(top_10_zip_codes)))

  
  # 3 Heat map of correlation matrix to see how variables are related.
install.packages("corrplot")

library(corrplot)

numerical_data <- housing_data[, c("Price_Sold", "Bedrooms", "Bathrooms", "Year_Built", "Squared_Feet", "Lot_Size", "Parking_Spots")]
numerical_data <- sapply(numerical_data, as.numeric)


correlation_matrix <- cor(numerical_data, use = "complete.obs")

corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, diag = FALSE)



  # 4 Price by Parking spots


housing_data$Parking_Spots <- as.numeric(ifelse(is.na(housing_data$Parking_Spots) | housing_data$Parking_Spots == "", 0, housing_data$Parking_Spots))


ggplot(housing_data, aes(x = factor(Parking_Spots), y = Price_Sold)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Box Plot of Price by Parking Spots", x = "Parking Spots", y = "Price Sold")

 # 5 Changes of prices over time

  library(ggplot2)
  
  
  housing_data$Date_Sold <- as.Date(housing_data$Date_Sold, format = "%m/%d/%Y")
  
  
  housing_data$Year <- lubridate::year(housing_data$Date_Sold)
  
  
  price_over_time <- aggregate(Price_Sold ~ Year, data = housing_data, FUN = mean)
  
  ggplot(price_over_time, aes(x = Year, y = Price_Sold)) +
    geom_line(color = "skyblue") +
    geom_point(color = "blue") +
    labs(title = "Average House Price Over Time", x = "Year", y = "Average Price") +
    scale_y_continuous(labels = scales::comma) +  # Set y-axis labels to display full numbers
    xlim(1980, 2030) +  # Set x-axis limits
    theme_minimal()

# 6 Variation in price according to Year built 
  filtered_data <- housing_data[housing_data$Year_Built >= 1980 & housing_data$Year_Built <= 2030, ]
  

  ggplot(filtered_data, aes(x = Price_Sold, fill = factor(Year_Built))) +
    geom_density(alpha = 0.5) +
    scale_x_continuous(labels = scales::comma_format()) + # Format x-axis labels
    labs(title = "Density Plot of Price by Year Built", x = "Price", y = "Density") +
    theme_minimal()
  
  
# 7 Variations in prices vs Square plots
  install.packages("plotly")
  library(plotly)
  

  plot_ly(housing_data, x = ~Squared_Feet, y = ~Price_Sold, type = "scatter", mode = "markers",
          marker = list(color = "rgba(17, 157, 255, 0.5)"), text = ~paste("Price: $", Price_Sold, "<br>Square Feet: ", Squared_Feet)) %>%
    layout(title = "Price vs. Square Feet",
           xaxis = list(title = "Square Feet"),
           yaxis = list(title = "Price Sold"),
           hovermode = "closest")

  
# 8 Temporal Analysis of percentage change in price vs Year
  

  library(ggplot2)
  

  housing_data$Date_Sold <- as.Date(housing_data$Date_Sold, format = "%m/%d/%Y")
  

  average_price_by_year <- aggregate(Price_Sold ~ Year, data = housing_data, FUN = mean)
  percentage_change <- c(NA, diff(average_price_by_year$Price_Sold) / lag(average_price_by_year$Price_Sold, default = 1) * 100)
  min_length <- min(length(average_price_by_year$Year), length(percentage_change))
  years <- average_price_by_year$Year[1:min_length]
  percentage_change <- percentage_change[1:min_length]

  price_change_data <- data.frame(Year = years, Percentage_Change = percentage_change)

  filtered_price_change_data <- price_change_data[price_change_data$Year >= 1980 & price_change_data$Year <= 2030, ]

  ggplot(filtered_price_change_data, aes(x = Year, y = Percentage_Change)) +
    geom_line(color = "blue") +
    labs(title = "Temporal Analysis of Price Appreciation", x = "Year", y = "Percentage Change in Price") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(1980, 2090, by = 10))