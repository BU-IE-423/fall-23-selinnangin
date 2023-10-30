library(ggplot2) 
library(lubridate) 
library(dplyr) 
library(gtrendsR) 
# Define the path
file_path <- "~/Downloads/all_ticks_long.csv.gz"
# Read the gzipped CSV file
my_data <- read.csv(gzfile(file_path), header = TRUE)
# Check the first few rows of the data head(my_data)
# Get unique short_name values
unique_names <- unique(my_data$short_name)
# Display at least the first 20 unique short_names head(unique_names, 200)
#via this, the companies chosen are: AKBNK, AYGAZ, BANVT, CCOLA, SISE, YKBNK
# List of companies to check
companies <- c("AKBNK", "AYGAZ", "BANVT", "CCOLA", "SISE", "YKBNK")
# Convert the timestamp to a date-time object
my_data$timestamp <- as.POSIXct(my_data$timestamp, format="%Y- %m-%dT%H:%M:%SZ")
# Extract year and month from the timestamp my_data$year_month <- format(my_data$timestamp, "%Y-%m")
filtered_data <- subset(my_data, short_name %in% companies)
# Generate separate boxplots for each company using facet_wrap ggplot(filtered_data, aes(x=year_month, y=price)) +
geom_boxplot(outlier.color="red", outlier.shape=16, outlier.size=2) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~ short_name, scales="free_y", ncol=1) + labs(title="Monthly Stock Prices by Company")
# Calculate outliers using IQR for each company and month outliers_data <- filtered_data %>%
group_by(short_name, year_month) %>% mutate(Q1 = quantile(price, 0.25),
                                            Q3 = quantile(price, 0.75),
                                            IQR = Q3 - Q1,
                                            lower_bound = Q1 - 1.5 * IQR, upper_bound = Q3 + 1.5 * IQR) %>%
  filter(price < lower_bound | price > upper_bound) %>% ungroup()
#via this method, 5489 rows of data are generated. So this isn’t a very good method because it looks at the data one by one. That’s why I started to look at the outstanding months’ means.
# Calculate monthly means for the selected companies monthly_means <- filtered_data %>%
group_by(short_name, year_month) %>% summarize(monthly_mean = mean(price), .groups = "drop")
# Add a 'year' column to the monthly_means data monthly_means$year <- as.integer(format(as.Date(monthly_means$year_month, format = "%Y- %m"), "%Y"))
# Compute the annual statistics for each company annual_stats <- monthly_means %>%
group_by(short_name, year) %>% summarise(annual_mean = mean(monthly_mean),
                                         Q1 = quantile(monthly_mean, 0.25),
                                         Q3 = quantile(monthly_mean, 0.75),
                                         IQR = Q3 - Q1,
                                         lower_bound = annual_mean - 1.5 * IQR, upper_bound = annual_mean + 1.5 * IQR, .groups = "drop")
# Determine outlier months for each company and year based on the computed bounds
outlier_months <- monthly_means %>%
  
  left_join(annual_stats, by = c("short_name", "year")) %>%
  filter(monthly_mean < lower_bound | monthly_mean > upper_bound) %>%
  select(short_name, year_month, monthly_mean) print(outlier_months, n=21)
#there are 21 outlier months depicted in the image below.