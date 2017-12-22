# Study of Combine data since 1987
# Used google chrome webscraper extension
# Data scraped is saved to the github

# Libraries
library(tidyverse)
library(stringr)

Combine <- read.csv("CombineDataComp.csv", strip.white = TRUE, as.is = TRUE, header = TRUE)
head(Combine)

# Fix the read problem with year by grabbing substring from yearresults
names(Combine)[1:4] <- c("start.url", "yearResults", "yearResultshref", "Year")
CombineMetricYears <- str_detect(Combine$yearResults, "[0-9]{4} NFL Combine Results")
Combine[CombineMetricYears, "Year"] <-  substr(Combine[CombineMetricYears, "yearResults"], 1, 4)

# Remove the entries from the wrong data files and select relevant columns
Combine <- Combine[Combine$Year != "null",] %>% select(-start.url, -yearResults, -yearResultshref)

# Rename the activity columns to easier names
names(Combine)[c(5, 6, 10, 11)] <- c("Height", "Weight", "VertLeap", "BroadJump")

# NFL is American so standard units are inches, pounds, and yards

# Some introductory plots to get an idea of what each stat means
playerSize <- ggplot(data = Combine, aes(x = Year, y = Height))+
              geom_boxplot()
