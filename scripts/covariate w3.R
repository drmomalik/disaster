# load packages

library(here)
library(tidyverse)

# load data 

here()

# load dataset from project folder "original"

rawdat <- read.csv(here("original", "conflictdata.csv"), header = TRUE)

summary(rawdat)

#create new datascript that takes conflict variable and create binary variable
#First set variable Year +1  since conflict variable is lagged
rawdat$Year <- rawdat$year + 1
rawdat2 <- aggregate(rawdat$best, by = list(rawdat$Year, rawdat$ISO), FUN=sum, drop = FALSE)
names(rawdat2) <- c("Year", "ISO", "Total_Best")
rawdat2$bin_conflict <- ifelse(rawdat2$Total_Best == 0 | is.na(rawdat2$Total_Best) ,0 ,1)

#download covariates.csv and merge data with this version of conflict dataset
covariates <- read.csv(here("original", "covariates.csv"), header = TRUE)
names(covariates)[names(covariates) == "year"] <- "Year"

#import matmort/worldbank dataset to incorporate into combine dataset
world_bank_mort <- read.csv(here("original", "world_bank_mort.csv"), header = TRUE)
summary(world_bank_mort)

#combine datasets 
df_list <- list(final_data, rawdat2, covariates, world_bank_mort)
combo_df <- df_list %>% reduce(full_join, by=c('ISO', 'Year'))
combo_df$total_droughts[is.na(combo_df$total_droughts)] <- 0
combo_df$total_earthquakes[is.na(combo_df$total_earthquakes)] <- 0
combo_df <- filter(combo_df, combo_df$Year %in% 2000:2019 )


#save new dataframe as .csv
write.csv(combo_df, "final_conflict.csv")

