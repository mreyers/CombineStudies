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
Combine <- Combine[Combine$Year != "null",] %>% select(-start.url, -yearResults, -yearResultshref) %>% arrange(Year, Name)

# Rename the activity columns to easier names
names(Combine)[c(5, 6, 10, 11)] <- c("Height", "Weight", "VertLeap", "BroadJump")

# NFL is American so standard units are inches, pounds, and yards

# Check for outliers and other missing data values

summary(Combine[5:dim(Combine)[2]]) # 9.99 is a place holder value for those that did not do timed events, set to NA
Combine <- na_if(Combine, 9.99) 




# Some introductory plots to get an idea of what each stat means
playerHeight <- ggplot(data = Combine, aes(x = Year, y = Height))+
              geom_boxplot() + ggtitle("Player Height by Year")
playerWeight <- ggplot(data = Combine, aes(x = Year, y = Weight))+
              geom_boxplot() + ggtitle("Player Weight by Year")



# Summary statistics
yearlyData <- ddply(Combine, .(Year, POS), summarize,
                    X40.YardAvg  = round(mean(X40.Yard, na.rm = TRUE), 2),
                    BenchAvg     = round(mean(Bench.Press, na.rm = TRUE), 2),
                    VertAvg      = round(mean(VertLeap, na.rm = TRUE), 2),
                    BroadJumpAvg = round(mean(BroadJump, na.rm = TRUE), 2),
                    ShuttleAvg   = round(mean(Shuttle, na.rm = TRUE), 2),
                    X3ConeAvg    = round(mean(X3Cone, na.rm = TRUE), 2))
head(yearlyData)
