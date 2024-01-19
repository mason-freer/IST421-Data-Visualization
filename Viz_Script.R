# IST 421 Plot Reviews
# Author: Mason Freer

myFile<-file.choose()
# prompts you to select the csv file you need

mental<-read.csv(file=myFile, header=TRUE, stringsAsFactors = FALSE)
# creates df that actually reads the  csv

#install.packages("ggplot2")
library(ggplot2)
library(dplyr)

mentaldf = mental

column_sum <- table(mental$Indicator)
column_sum

# Needed Counseling or Therapy but did not get it and Age

subgroups <- c("18 - 29 years",
               "30 - 39 years",
               "40 - 49 years",
               "50 - 59 years",
               "60 - 69 years",
               "70 - 79 years",
               "80 years and above")

mental_filtered = mental[mental$Subgroup %in% subgroups, ]

mental_filtered <- mental_filtered[mental_filtered$Indicator == "Needed Counseling or Therapy But Did Not Get It, Last 4 Weeks",]

mental_filtered$Date.Median <- as.Date(mental_filtered$Date.Median, format = "%m/%d/%Y")

result <- mental_filtered %>%
  mutate(MonthYear = format(Date.Median, "%Y-%m")) %>%
  group_by(MonthYear, Subgroup) %>%
  summarize(AverageValueColumn = mean(Value, na.rm = TRUE))

result <- result %>%
  group_by(Subgroup) %>%
  mutate(AverageValueColumn = replace(AverageValueColumn, is.na(AverageValueColumn), mean(AverageValueColumn, na.rm = TRUE)))

plot <- ggplot(result, aes(x = MonthYear, y = AverageValueColumn, color = Subgroup)) +
  geom_line(aes(group = Subgroup)) +
  geom_point() +
  labs(title = "Age Groups that Needed Counseling or Therapy But Did Not Get It, Last 4 Weeks",
       x = "Month",
       y = "Percentage",
       color = "Age Group") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Show the plot
print(plot)

# Needed Counseling or Therapy but did not get it and Gender

subgroups <- c("Male",
               "Female")

mental_filtered = mental[mental$Subgroup %in% subgroups, ]

mental_filtered <- mental_filtered[mental_filtered$Indicator == "Needed Counseling or Therapy But Did Not Get It, Last 4 Weeks",]

mental_filtered$Date.Median <- as.Date(mental_filtered$Date.Median, format = "%m/%d/%Y")

result <- mental_filtered %>%
  mutate(MonthYear = format(Date.Median, "%Y-%m")) %>%
  group_by(MonthYear, Subgroup) %>%
  summarize(AverageValueColumn = mean(Value, na.rm = TRUE))

result <- result %>%
  group_by(Subgroup) %>%
  mutate(AverageValueColumn = replace(AverageValueColumn, is.na(AverageValueColumn), mean(AverageValueColumn, na.rm = TRUE)))

plot <- ggplot(result, aes(x = MonthYear, y = AverageValueColumn, color = Subgroup)) +
  geom_line(aes(group = Subgroup)) +
  geom_point() +
  labs(title = "Genders that Needed Counseling or Therapy But Did Not Get It, Last 4 Weeks",
       x = "Month",
       y = "Percentage",
       color = "Gender") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Show the plot
print(plot)

# Needed Counseling or Therapy but did not get it and Education

subgroups <- c("Less than a high school diploma",
               "High school diploma or GED",
               "Some college/Associate's degree",
               "Bachelor's degree or higher")

mental_filtered = mental[mental$Subgroup %in% subgroups, ]

mental_filtered <- mental_filtered[mental_filtered$Indicator == "Needed Counseling or Therapy But Did Not Get It, Last 4 Weeks",]

mental_filtered$Date.Median <- as.Date(mental_filtered$Date.Median, format = "%m/%d/%Y")

result <- mental_filtered %>%
  mutate(MonthYear = format(Date.Median, "%Y-%m")) %>%
  group_by(MonthYear, Subgroup) %>%
  summarize(AverageValueColumn = mean(Value, na.rm = TRUE))

result <- result %>%
  group_by(Subgroup) %>%
  mutate(AverageValueColumn = replace(AverageValueColumn, is.na(AverageValueColumn), mean(AverageValueColumn, na.rm = TRUE)))

plot <- ggplot(result, aes(x = MonthYear, y = AverageValueColumn, color = Subgroup)) +
  geom_line(aes(group = Subgroup)) +
  geom_point() +
  labs(title = "Education that Needed Counseling or Therapy But Did Not Get It, Last 4 Weeks",
       x = "Month",
       y = "Percentage",
       color = "Education") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Show the plot
print(plot)

# Needed Counseling or Therapy but did not get it and Race

subgroups <- c("Hispanic or Latino",
               "Non-Hispanic White, single race",
               "Non-Hispanic Black, single race",
               "Non-Hispanic Asian, single race",
               "Non-Hispanic, other races and multiple races")

mental_filtered = mental[mental$Subgroup %in% subgroups, ]

mental_filtered$Subgroup[mental_filtered$Subgroup == "Non-Hispanic White, single race"] <- "White"
mental_filtered$Subgroup[mental_filtered$Subgroup == "Non-Hispanic Asian, single race"] <- "Asian"
mental_filtered$Subgroup[mental_filtered$Subgroup == "Non-Hispanic Black, single race"] <- "Black"
mental_filtered$Subgroup[mental_filtered$Subgroup == "Non-Hispanic, other races and multiple races"] <- "Non-Hispanic Other"

mental_filtered <- mental_filtered[mental_filtered$Indicator == "Needed Counseling or Therapy But Did Not Get It, Last 4 Weeks",]

mental_filtered$Date.Median <- as.Date(mental_filtered$Date.Median, format = "%m/%d/%Y")

result <- mental_filtered %>%
  mutate(MonthYear = format(Date.Median, "%Y-%m")) %>%
  group_by(MonthYear, Subgroup) %>%
  summarize(AverageValueColumn = mean(Value, na.rm = TRUE))

result <- result %>%
  group_by(Subgroup) %>%
  mutate(AverageValueColumn = replace(AverageValueColumn, is.na(AverageValueColumn), mean(AverageValueColumn, na.rm = TRUE)))

plot <- ggplot(result, aes(x = MonthYear, y = AverageValueColumn, color = Subgroup)) +
  geom_line(aes(group = Subgroup)) +
  geom_point() +
  labs(title = "Races that Needed Counseling or Therapy But Did Not Get It, Last 4 Weeks",
       x = "Month",
       y = "Percentage",
       color = "Race") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Show the plot
print(plot)

# Took Prescription Medication for Mental Health and Age

subgroups <- c("18 - 29 years",
               "30 - 39 years",
               "40 - 49 years",
               "50 - 59 years",
               "60 - 69 years",
               "70 - 79 years",
               "80 years and above")

mental_filtered = mental[mental$Subgroup %in% subgroups, ]

mental_filtered <- mental_filtered[mental_filtered$Indicator == "Took Prescription Medication for Mental Health, Last 4 Weeks",]

mental_filtered$Date.Median <- as.Date(mental_filtered$Date.Median, format = "%m/%d/%Y")

result <- mental_filtered %>%
  mutate(MonthYear = format(Date.Median, "%Y-%m")) %>%
  group_by(MonthYear, Subgroup) %>%
  summarize(AverageValueColumn = mean(Value, na.rm = TRUE))

result <- result %>%
  group_by(Subgroup) %>%
  mutate(AverageValueColumn = replace(AverageValueColumn, is.na(AverageValueColumn), mean(AverageValueColumn, na.rm = TRUE)))

plot <- ggplot(result, aes(x = MonthYear, y = AverageValueColumn, color = Subgroup)) +
  geom_line(aes(group = Subgroup)) +
  geom_point() +
  labs(title = "Took Prescription Medication for Health, Last 4 Weeks",
       x = "Time Period Start Date",
       y = "Percentage",
       color = "Age Group") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Show the plot
print(plot)


# Received Counseling or Therapy, Last 4 Weeks and Age

subgroups <- c("18 - 29 years",
               "30 - 39 years",
               "40 - 49 years",
               "50 - 59 years",
               "60 - 69 years",
               "70 - 79 years",
               "80 years and above")

mental_filtered = mental[mental$Subgroup %in% subgroups, ]

mental_filtered <- mental_filtered[mental_filtered$Indicator == "Received Counseling or Therapy, Last 4 Weeks",]

mental_filtered$Date.Median <- as.Date(mental_filtered$Date.Median, format = "%m/%d/%Y")

result <- mental_filtered %>%
  mutate(MonthYear = format(Date.Median, "%Y-%m")) %>%
  group_by(MonthYear, Subgroup) %>%
  summarize(AverageValueColumn = mean(Value, na.rm = TRUE))

result <- result %>%
  group_by(Subgroup) %>%
  mutate(AverageValueColumn = replace(AverageValueColumn, is.na(AverageValueColumn), mean(AverageValueColumn, na.rm = TRUE)))

plot <- ggplot(result, aes(x = MonthYear, y = AverageValueColumn, color = Subgroup)) +
  geom_line(aes(group = Subgroup)) +
  geom_point() +
  labs(title = "Received Counseling or Therapy, Last 4 Weeks",
       x = "Time Period Start Date",
       y = "Percentage",
       color = "Age Group") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Show the plot
print(plot)


# Took Prescription Medication And/Or Received Counseling/Therapy, Last 4 Weeks and Age

subgroups <- c("18 - 29 years",
               "30 - 39 years",
               "40 - 49 years",
               "50 - 59 years",
               "60 - 69 years",
               "70 - 79 years",
               "80 years and above")

mental_filtered = mental[mental$Subgroup %in% subgroups, ]

mental_filtered <- mental_filtered[mental_filtered$Indicator == "Took Prescription Medication for Mental Health And/Or Received Counseling or Therapy, Last 4 Weeks",]

mental_filtered$Date.Median <- as.Date(mental_filtered$Date.Median, format = "%m/%d/%Y")

result <- mental_filtered %>%
  mutate(MonthYear = format(Date.Median, "%Y-%m")) %>%
  group_by(MonthYear, Subgroup) %>%
  summarize(AverageValueColumn = mean(Value, na.rm = TRUE))

result <- result %>%
  group_by(Subgroup) %>%
  mutate(AverageValueColumn = replace(AverageValueColumn, is.na(AverageValueColumn), mean(AverageValueColumn, na.rm = TRUE)))

plot <- ggplot(result, aes(x = MonthYear, y = AverageValueColumn, color = Subgroup)) +
  geom_line(aes(group = Subgroup)) +
  geom_point() +
  labs(title = "Took Prescription Medication And/Or Received Counseling/Therapy, Last 4 Weeks",
       x = "Time Period Start Date",
       y = "Percentage",
       color = "Age Group") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Show the plot
print(plot)


#### United States Map

# install.packages("rworldmap")
library(maps) #Displays maps
library(mapproj)  #Converts latitude/longitude into projected coordinates.
library(rworldmap)

mental_filter = mental[mental$Group == "By State", ]
mental_filter = mental_filter[mental_filter$Indicator == "Needed Counseling or Therapy But Did Not Get It, Last 4 Weeks", ]
mental_filter = mental_filter[mental_filter$Date.Median == "5/3/2022", ]
mental_filter

maps::map("world", regions="USA")

par(mar=c(0,0,0,0))
maps::map("state")
m<-maps::map("state")
#view(m)

plot(m$x,m$y)

state.order <- match.map(database = 'state', regions = mental_filter$State, 
                         exact = FALSE,  warn = TRUE)

#agg.dat<-mental_filter %>% group_by(State) %>% summarise(victims=sum(Total.Number.of.Victims))
#agg.dat

library(viridis)

num.cols <- 15
my.color.vec <- heat.colors(num.cols) 
#my.color.vec <- viridis_pal(option = "C", direction = 1)(num.cols)
my.color.vec

library(plotrix)

#agg.dat$index<-rescale(x=agg.dat$Value, c(1,num.cols))  
mental_filter$index<-rescale(x=mental_filter$Value, c(1,num.cols))
  
#Then scale num of victims to num of colors, and round them to integers
mental_filter$index<-round(rescale(x=mental_filter$Value, c(1,num.cols)),0)    
    
#Assign each state to a color based on index, creating a color column
mental_filter$color<-my.color.vec[mental_filter$index]

maps::map("state", col = mental_filter$color[state.order], fill = TRUE
          , resolution = 0,lty = 1, projection = "polyconic", border="tan")

pretty_values <- pretty(range(mental_filter$Value), n = 10)

legend("topright", legend = pretty_values, fill = mental_filter$color,
       title = "Values", cex = 0.7, ncol = 1)

## Return Uniques

num_unique <- mental %>% 
  pull(Time.Period.Label) %>% 
  n_distinct()

print(num_unique)