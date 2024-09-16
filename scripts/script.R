# load data 

here()

# load dataset from project folder "original"

rawdat <- read.csv(here("original", "disaster.csv"), header = TRUE)

# filter data and create subset to only include years 2000-2019 and disaster types "Earthquake" and "Drought"

filter_data <- filter(rawdat, rawdat$Year %in% 2000:2019 & rawdat$Disaster.Type == c("Earthquake", "Drought"))

# subset data to include only Year, ISO and Disaster.Type

sub_data <- select(filter_data, Year, ISO, Disaster.Type)

# create duummy variables for drought and earthquake

sub_data$Drought <- ifelse(sub_data$Disaster.Type == "Drought", 1, 0)
sub_data$Earthquake <- ifelse(sub_data$Disaster.Type == "Earthquake", 1, 0)

#Use group_by() and summarize () function to create data set where only one row of observations exists for each country and each year

final_data <- sub_data %>% group_by(Year, ISO)
final_data <- summarize(sub_data, sub_data$Year, sub_data$ISO)