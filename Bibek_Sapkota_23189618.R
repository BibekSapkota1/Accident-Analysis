## --------------------------------------------------------------------------------------------------------------------------
packages <- c("tidyverse", "dplyr", "lubridate")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(tidyverse)
library(dplyr)
library(lubridate)


## --------------------------------------------------------------------------------------------------------------------------
df <- read_csv("accidents.csv")
head(df)


## --------------------------------------------------------------------------------------------------------------------------
tail(df)


## --------------------------------------------------------------------------------------------------------------------------
dim(df)


## --------------------------------------------------------------------------------------------------------------------------
view(df)


## --------------------------------------------------------------------------------------------------------------------------
str(df)


## --------------------------------------------------------------------------------------------------------------------------
summary(df)


## --------------------------------------------------------------------------------------------------------------------------
df_clean <- df


## --------------------------------------------------------------------------------------------------------------------------
df_clean$'Accident Date' <- dmy(df_clean$'Accident Date')
str(df_clean)


## --------------------------------------------------------------------------------------------------------------------------
df_clean$`Time (24hr)` <- format(as.POSIXct(sprintf("%04d", df_clean$`Time (24hr)`),
                                            format="%H%M", tz = "UTC"), format="%H:%M", usetz = FALSE)
str(df_clean)


## --------------------------------------------------------------------------------------------------------------------------
missing_values <- colSums(is.na(df_clean))
print("Columns with Missing Values:",)
missing_values <- missing_values[missing_values > 0]
print(missing_values)


## --------------------------------------------------------------------------------------------------------------------------
missing_age <- df %>% filter(is.na(`Age of Casualty`))
print(missing_age)


## --------------------------------------------------------------------------------------------------------------------------
missing_day <- df %>% filter(is.na(`Daylight/Dark`))
print(missing_day)


## --------------------------------------------------------------------------------------------------------------------------
ggplot(df_clean, aes(x = factor(`Lighting Conditions`), fill = factor(is.na(`Daylight/Dark`)))) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Missing Values in Daylight/Dark by Lighting Conditions",
       x = "Lighting Conditions",
       y = "Count") +
  scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red"), name = "Missing Values") +
  theme_minimal()


## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of 1st Road Class:\n")
unique(df_clean$`1st Road Class`)


## --------------------------------------------------------------------------------------------------------------------------
road_class_mapping <- c("1" = "Motorway","2" = "A(M)","3" = "A","4" = "B","5" = "C","6" = "Unclassified")

df_clean <- df_clean %>%
  mutate(`1st Road Class` = map_chr(`1st Road Class`, function(x) {
    ifelse(as.character(x) %in% names(road_class_mapping), road_class_mapping[as.character(x)], as.character(x))
  }))


## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of 1st Road Class:\n")
unique(df_clean$`1st Road Class`)


## --------------------------------------------------------------------------------------------------------------------------
df_clean <- df_clean %>%
  mutate(
    `1st Road Class` = case_when(
      `1st Road Class` == "U" ~ "Unclassified",
      grepl("^A\\d+", `1st Road Class`) ~ "A",
      grepl("^A\\(M\\)$", `1st Road Class`) ~ "A(M)",
      grepl("^A\\d+\\(M\\)$", `1st Road Class`) ~ "A(M)",
      grepl("^B\\d+", `1st Road Class`) ~ "B",
      `1st Road Class` == "M62" ~ "Motorway",
      TRUE ~ `1st Road Class`
    )
  )



## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of 1st Road Class:\n")
unique(df_clean$`1st Road Class`)


## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of Road Surface:\n")
unique(df_clean$`Road Surface`)


## --------------------------------------------------------------------------------------------------------------------------
road_surface_mapping <- c(
  "Wet/Damp" = "Wet / Damp", 
  "Wet" = "Wet / Damp",
  "Frost/Ice" = "Frost / Ice", 
  "Ice" = "Frost / Ice",
  "1" = "Dry", 
  "2" = "Wet / Damp",
  "3" = "Snow", 
  "4" = "Frost / Ice",
  "5" = "Flood (surface water over 3cm deep)",
  "Wet \xa8 Damp" = "Wet / Damp"
)

df_clean <- df_clean %>%
  mutate(`Road Surface` = map_chr(`Road Surface`, function(x) {
    ifelse(as.character(x) %in% names(road_surface_mapping), road_surface_mapping[as.character(x)], as.character(x))
  }))


## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of Road Surface:\n")
unique(df_clean$`Road Surface`)


## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of Lighting Conditions:\n")
unique(df_clean$`Lighting Conditions`)


## --------------------------------------------------------------------------------------------------------------------------
lighting_conditions_mapping <- c("1" = "Daylight: street lights present",
                                 "2" = "Daylight: no street lighting",
                                 "3" = "Daylight: street lighting unknown",
                                 "4" = "Darkness: street lights present and lit",
                                 "5" = "Darkness: street lights present but unlit",
                                 "6" = "Darkness: no street lighting",
                                 "7" = "Darkness: street lighting unknown")

df_clean <- df_clean %>%
  mutate(`Lighting Conditions` = map_chr(`Lighting Conditions`, function(x) {
    ifelse(as.character(x) %in% names(lighting_conditions_mapping), lighting_conditions_mapping[as.character(x)], as.character(x))
  }))


## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of Lighting Conditions:\n")
unique(df_clean$`Lighting Conditions`)


## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of Weather Conditions:\n")
unique(df_clean$`Weather Conditions`)


## --------------------------------------------------------------------------------------------------------------------------
weather_conditions_mapping <- c("1" = "Fine without high winds",
                                "2" = "Raining without high winds",
                                "3" = "Snowing without high winds",
                                "4" = "Fine with high winds",
                                "5" = "Raining with high winds",
                                "6" = "Snowing with high winds",
                                "7" = "Fog or mist ? if hazard",
                                "8" = "Other",
                                "9" = "Unknown")

df_clean <- df_clean %>%
  mutate(`Weather Conditions` = map_chr(`Weather Conditions`, function(x) {
    ifelse(as.character(x) %in% names(weather_conditions_mapping), weather_conditions_mapping[as.character(x)], as.character(x))
  }))


## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of Weather Conditions:\n")
unique(df_clean$`Weather Conditions`)


## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of Casualty Class:\n")
unique(df_clean$`Casualty Class`)


## --------------------------------------------------------------------------------------------------------------------------
casualty_class_mapping <- c("1" = "Driver or rider",
                            "2" = "Vehicle or pillion passenger",
                            "3" = "Pedestrian")

df_clean <- df_clean %>%
  mutate(`Casualty Class` = map_chr(`Casualty Class`, function(x){
    ifelse(as.character(x) %in% names(casualty_class_mapping), casualty_class_mapping[as.character(x)], as.character(x))
  }))


## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of Casualty Class:\n")
unique(df_clean$`Casualty Class`)


## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of Casualty Severity:\n")
unique(df_clean$`Casualty Severity`)


## --------------------------------------------------------------------------------------------------------------------------
casualty_severity_mapping <- c("1" = "Fatal",
                               "2" = "Serious",
                               "3" = "Slight")

df_clean <- df_clean %>%
  mutate(`Casualty Severity` = map_chr(`Casualty Severity`, function(x) {
    ifelse(as.character(x) %in% names(casualty_severity_mapping), casualty_severity_mapping[as.character(x)], as.character(x))
  }))


## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of Casualty Severity:\n")
unique(df_clean$`Casualty Severity`)


## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of Type of Vehicle:\n")
unique(df_clean$`Type of Vehicle`)


## --------------------------------------------------------------------------------------------------------------------------
vehicle_type_mapping <- c("1" = "Pedal cycle",
                          "2" = "M/cycle 50cc and under",
                          "3" = "Motorcycle over 50cc and up to 125cc",
                          "4" = "Motorcycle over 125cc and up to 500cc",
                          "5" = "Motorcycle over 500cc",
                          "8" = "Taxi/Private hire car",
                          "9" = "Car",
                          "10" = "Minibus (8 – 16 passenger seats)",
                          "11" = "Bus or coach (17 or more passenger seats)",
                          "14" = "Other motor vehicle",
                          "15" = "Other non-motor vehicle",
                          "16" = "Ridden horse",
                          "17" = "Agricultural vehicle (includes diggers etc.)",
                          "18" = "Tram / Light rail",
                          "19" = "Goods vehicle 3.5 tonnes mgw and under",
                          "20" = "Goods vehicle over 3.5 tonnes and under 7.5 tonnes mgw",
                          "21" = "Goods vehicle 7.5 tonnes mgw and over",
                          "22" = "Mobility Scooter",
                          "90" = "Other Vehicle",
                          "97" = "Motorcycle - Unknown CC")

df_clean <- df_clean %>%
  mutate(`Type of Vehicle` = map_chr(`Type of Vehicle`, function(x) {
    ifelse(as.character(x) %in% names(vehicle_type_mapping), vehicle_type_mapping[as.character(x)], as.character(x))
  }))


## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of Type of Vehicle:\n")
unique(df_clean$`Type of Vehicle`)


## --------------------------------------------------------------------------------------------------------------------------
df_clean %>%
  filter(`Type of Vehicle` == "23") %>%
  print()


## --------------------------------------------------------------------------------------------------------------------------
df_clean <- df_clean %>%
  mutate(`Type of Vehicle` = recode(`Type of Vehicle`, "23" = "[Not used]"))


## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of Casualty Severity:\n")
unique(df_clean$`Type of Vehicle`)


## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of Sex of Casualty:\n")
unique(df_clean$'Sex of Casualty')


## --------------------------------------------------------------------------------------------------------------------------
sex_of_casualty_mapping <- c("1" = "Male",
                             "2" = "Female")

df_clean <- df_clean %>%
  mutate(`Sex of Casualty` = map_chr(`Sex of Casualty`, function(x) {
    ifelse(as.character(x) %in% names(sex_of_casualty_mapping), sex_of_casualty_mapping[as.character(x)], as.character(x))
  }))


## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of Sex of Casualty:\n")
unique(df_clean$'Sex of Casualty')


## --------------------------------------------------------------------------------------------------------------------------
cat("Unique values of Local Authority:\n")
unique(df_clean$'Local Authority')


## --------------------------------------------------------------------------------------------------------------------------
df_clean <- df_clean %>%
  select(-`Local Authority`)


## --------------------------------------------------------------------------------------------------------------------------
view(df_clean)


## --------------------------------------------------------------------------------------------------------------------------
missing_daylight_dark <- df_clean[is.na(df_clean$`Daylight/Dark`), ]
print(missing_daylight_dark)

## --------------------------------------------------------------------------------------------------------------------------
str(df_clean)


## --------------------------------------------------------------------------------------------------------------------------
lighting_mapping <- c(
  "Darkness: no street lighting" = "Dark"
)

df_clean <- df_clean %>%
  mutate(`Daylight/Dark` = ifelse(is.na(`Daylight/Dark`), lighting_mapping[`Lighting Conditions`], `Daylight/Dark`))


## --------------------------------------------------------------------------------------------------------------------------
missing_daylight_dark <- df_clean[is.na(df_clean$`Daylight/Dark`), ]
print(missing_daylight_dark)


## --------------------------------------------------------------------------------------------------------------------------
df_clean$`Lighting Conditions` <- sub("^(Daylight:|Darkness:)(.*)", "\\2", df_clean$`Lighting Conditions`)
view(df_clean)


## --------------------------------------------------------------------------------------------------------------------------
boxplot(df_clean$`Age of Casualty`, main = "Boxplot of Age of Casualty", 
        ylab = "Age of Casualty")

outlier_box <- boxplot(df_clean$`Age of Casualty`, plot = FALSE)$out


## --------------------------------------------------------------------------------------------------------------------------
# Display the number of outliers
cat("Number of outliers using boxplots:", outlier_box, "\n")


## --------------------------------------------------------------------------------------------------------------------------
mean_age <- mean(df_clean$`Age of Casualty`, na.rm = TRUE)
sd_age <- sd(df_clean$`Age of Casualty`, na.rm = TRUE)

upper_bound_3sigma <- mean_age + 3 * sd_age
lower_bound_3sigma <- mean_age - 3 * sd_age

outliers_3sigma <- df_clean %>% filter((`Age of Casualty` > upper_bound_3sigma) | (`Age of Casualty` < lower_bound_3sigma))

outliers_3sigma$`Age of Casualty`


## --------------------------------------------------------------------------------------------------------------------------
median_age <- median(df_clean$`Age of Casualty`, na.rm = TRUE)
mad_age <- mad(df_clean$`Age of Casualty`, na.rm = TRUE)

upper_bound <- median_age + 3 * mad_age
lower_bound <- max(0, median_age - 3 * mad_age)

outliers <- df_clean %>% filter(`Age of Casualty` > upper_bound | `Age of Casualty` < lower_bound)

outliers$`Age of Casualty`


## --------------------------------------------------------------------------------------------------------------------------
view(df_clean)
write.csv(df_clean, "clean_accident.csv", row.names = FALSE,  quote= FALSE, fileEncoding = "UTF-8")


## --------------------------------------------------------------------------------------------------------------------------
data <- read_csv("clean_accident.csv")
head(data)


## --------------------------------------------------------------------------------------------------------------------------
colors <- sample(colors(), length(unique(data$`Casualty Severity`)))


## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = `Casualty Class`)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribution of Casualty Classes",
       x = "Road Class",
       y = "Number of Accidents") +
  theme_minimal()


## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = `Weather Conditions`)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribution of Weather Conditions",
       x = "Weather Conditions",
       y = "Number of Accidents") +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))


## --------------------------------------------------------------------------------------------------------------------------
gender_data <- data %>%
  filter(`Sex of Casualty` %in% c("Male", "Female"), `Casualty Class` %in% c("Driver or rider", "Vehicle or pillion passenger"))
weather_gender_table <- table(gender_data$`Weather Conditions`, gender_data$`Sex of Casualty`)

print(weather_gender_table)


## --------------------------------------------------------------------------------------------------------------------------
weather_gender_df <- as.data.frame(weather_gender_table)

colnames(weather_gender_df) <- c("Weather Conditions", "Sex of Casualty", "Count")

ggplot(weather_gender_df, aes(x = `Weather Conditions`, y = Count, fill = `Sex of Casualty`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Casualty Count by Weather Conditions and Gender",
       x = "Weather Conditions",
       y = "Count",
       fill = "Sex of Casualty") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = `1st Road Class`)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribution of Road Classes",
       x = "Road Class",
       y = "Number of Accidents") +
  theme_minimal()


## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = `1st Road Class`, fill = `Casualty Severity`)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Accident Severity across Different Road Classes",
       x = "1st Road Class",
       y = "Number of Accidents",
       fill = "Casualty Severity") +
  scale_fill_manual(values = colors) +  # Set random fill colors
  theme_minimal()


## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = `Road Surface`)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribution of Road Surface Conditions",
       x = "Road Surface",
       y = "Number of Accidents") +
  theme_minimal()



## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = `Road Surface`, fill = `Casualty Severity`)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Accidents based on Road Surface Conditions",
       x = "Road Surface Conditions",
       y = "Number of Accidents",
       fill = "Casualty Severity") +
    scale_fill_manual(values = colors) + 
  theme_minimal()


## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = `Lighting Conditions`)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribution of Lighting Conditions",
       x = "Lighting Conditions",
       y = "Number of Accidents") +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))


## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = `Lighting Conditions`, fill = `Casualty Severity`)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Accidents based on Lighting Condition",
       x = "Lighting Condition",
       y = "Number of Accidents",
       fill = "Casualty Severity") +
  scale_fill_manual(values = colors) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = `Daylight/Dark`)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribution of Daylight / Dark",
       x = "Daylight / Dark",
       y = "Number of Accidents") +
  theme_minimal() 


## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = `Daylight/Dark`, fill = `Casualty Severity`)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Accidents based on Daylight / Dark",
       x = "Daylight/Dark",
       y = "Number of Accidents",
       fill = "Casualty Severity") +
    scale_fill_manual(values = colors) + 
  theme_minimal()


## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = `Weather Conditions`)) +
  geom_bar(fill = "blue", color = "black", stat = "count") +
  labs(title = "Distribution of Weather Conditions",
       x = "Weather Conditions",
       y = "Number of Accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = `Weather Conditions`, fill = `Casualty Severity`)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Accidents based on Weather Conditions",
       x = "Weather Conditions",
       y = "Number of Accidents",
       fill = "Casualty Severity") +
    scale_fill_manual(values = colors) + 
   theme(axis.text.x = element_text(angle = 45, hjust = 1))


## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = `Type of Vehicle`)) +
  geom_bar(fill = "blue", color = "black", stat = "count") +
  labs(title = "Distribution of Type of Vehicle",
       x = "Type of Vehicle",
       y = "Number of Accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = `Type of Vehicle`, fill = `Casualty Severity`)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Accidents based on Weather Conditions",
       x = "Weather Conditions",
       y = "Number of Accidents",
       fill = "Casualty Severity") +
    scale_fill_manual(values = colors) + 
   theme(axis.text.x = element_text(angle = 45, hjust = 1))


## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = `Weather Conditions`, y = `Number of Vehicles`)) +
  geom_jitter(color = "blue", alpha = 0.7, width = 0.3) +
  labs(title = "Relationship Between Weather Conditions and Number of Vehicles ",
       x = "Weather Conditions",
       y = "Number of Vehicles") +
  theme_minimal()+
theme(axis.text.x = element_text(angle = 45, hjust = 1))


## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = `Casualty Severity`, fill = `Casualty Severity`)) +
  geom_bar(fill = colors, color = "black") +  # Use random colors
  labs(title = "Distribution of Casualty Severity",
       x = "Casualty Severity",
       y = "Number of Accidents") +
  theme_minimal()


## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = `Accident Date`)) +
  geom_bar(fill = "red", color = "blue") +
  labs(title = "Distribution of Accidents over Time",
       x = "Accident Date",
       y = "Number of Accidents") +
  theme_minimal()


## --------------------------------------------------------------------------------------------------------------------------
yearly_data <- data %>%
  mutate(Year = year(`Accident Date`)) %>%
  group_by(Year) %>%
  summarise(Count = n())

yearly_data$Year <- as.factor(yearly_data$Year)

ggplot(yearly_data, aes(x = Year, y = Count, fill = Year)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Accidents By Year",
       x = "Year",
       y = "Count",
       fill = "Year") +
  scale_fill_discrete(name = "Year") +
  theme_minimal()


## --------------------------------------------------------------------------------------------------------------------------
ggplot(yearly_data, aes(x = Year, y = Count, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") + 
  labs(title = "Total Accidents By Year",
       x = "Year",
       y = "Count") +
  theme_minimal()



## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = month(`Accident Date`, label = TRUE))) +
  geom_bar(stat = "count") +
  labs(title = "Accidents by Month", x = "Month", y = "Number of Accidents") +
  theme_minimal()


## --------------------------------------------------------------------------------------------------------------------------
monthly_data <- data %>%
  mutate(Month = month(`Accident Date`, label = TRUE)) %>%
  group_by(Month) %>%
  summarize(Accident_Count = n())

# Create the line plot
ggplot(monthly_data, aes(x = Month, y = Accident_Count, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Accidents by Month", x = "Month", y = "Number of Accidents") +
  theme_minimal()


## --------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = weekdays(`Accident Date`))) +
  geom_bar(stat = "count") +
  labs(title = "Accidents by Day ", x = "Day of the Week", y = "Number of Accidents") +
  theme_minimal()


## --------------------------------------------------------------------------------------------------------------------------
weekly_data <- data %>%
  mutate(Day = weekdays(`Accident Date`)) %>%
  group_by(Day) %>%
  summarize(Accident_Count = n()) %>%
  arrange(match(Day, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

# Create the line plot
ggplot(weekly_data, aes(x = Day, y = Accident_Count, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Accidents by Day ", x = "Day of the Week", y = "Number of Accidents") +
  theme_minimal()


## --------------------------------------------------------------------------------------------------------------------------
data$`Casualty Class` <- as.factor(data$`Casualty Class`)
data$`Casualty Severity` <- as.factor(data$`Casualty Severity`)
data$`Type of Vehicle` <- as.factor(data$`Type of Vehicle`)
data$`Weather Conditions` <- as.factor(data$`Weather Conditions`)


## --------------------------------------------------------------------------------------------------------------------------
train_data <- data %>%
  filter(!is.na(`Age of Casualty`))


## --------------------------------------------------------------------------------------------------------------------------
lm_model <- lm(`Age of Casualty` ~ `Casualty Class` + `Casualty Severity` 
               + `Type of Vehicle` + `Weather Conditions`, data = train_data)


## --------------------------------------------------------------------------------------------------------------------------
print(summary(lm_model))


## --------------------------------------------------------------------------------------------------------------------------
r_squared <- summary(lm_model)$r.squared
cat("R-squared value:", r_squared, "\n")


## --------------------------------------------------------------------------------------------------------------------------
missing_age_data <- data %>%
  filter(is.na(`Age of Casualty`))


## --------------------------------------------------------------------------------------------------------------------------
predict_missing_age <- function(model, missing_age_data) {
  predict(model, newdata = missing_age_data)
}

predicted_age <- predict_missing_age(lm_model, missing_age_data)


## --------------------------------------------------------------------------------------------------------------------------
data$`Age of Casualty`[is.na(data$`Age of Casualty`)] <- predicted_age
predicted_age


## --------------------------------------------------------------------------------------------------------------------------
data <- data %>%
  mutate(`Age of Casualty` = round(`Age of Casualty`)) %>%
  mutate(`Age of Casualty` = as.integer(`Age of Casualty`))


## --------------------------------------------------------------------------------------------------------------------------
check_missing_values <- function(data) {
  colSums(is.na(data))
}

missing_values <- check_missing_values(data)
cat("Columns with Missing Values:\n")
print(missing_values[missing_values > 0])


## --------------------------------------------------------------------------------------------------------------------------
cat("Dataset dimensions:", dim(data), "\n")


## --------------------------------------------------------------------------------------------------------------------------
view(data)


## --------------------------------------------------------------------------------------------------------------------------
write.csv(data, "regression.csv", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")

