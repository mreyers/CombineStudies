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
yearlyData <-  Combine %>% group_by(Year, POS) %>% 
                           summarise(X40.YardAvg  = round(mean(X40.Yard, na.rm = TRUE), 2),
                                     BenchAvg     = round(mean(Bench.Press, na.rm = TRUE), 2),
                                     VertAvg      = round(mean(VertLeap, na.rm = TRUE), 2),
                                     BroadJumpAvg = round(mean(BroadJump, na.rm = TRUE), 2),
                                     ShuttleAvg   = round(mean(Shuttle, na.rm = TRUE), 2),
                                     X3ConeAvg    = round(mean(X3Cone, na.rm = TRUE), 2))

WR40YardAvg <- yearlyData %>% filter(POS == "WR")

RB40YardAvg <- yearlyData %>% filter(POS == "RB") %>% ggplot(aes(x = Year, y = X40.YardAvg)) + geom_point() + geom_smooth(method = "lm", formula = y~x) +geom_point(aes(y = WR40YardAvg$X40.YardAvg, col = "red")) # geom_smooth() not printing for some reason
RB40YardAvg # Plot shows RB vs. WR breakaway speed, note WR are faster in every year except 2016


# Room for other general summary stats here

# Create an overall ranking for each player by position

  # Create the individual DFs
  for(i in unique(Combine$POS)){
    name <- paste0("Combine", i)
    assign(name, Combine[Combine$POS == i, ])
  }
  
  # Focus on the major groups: C, CB, DE, DT, FB, FS, ILB, OG, OLB, OT, QB, RB, SS, TE, WR
  head(CombineC)
  
  # Basic measure: Create a sum of total rank across each drill/measurement, measuring rank by desirable order (low time but high Broad jump)
  # Order naturally is ascending (1, 2, 3, 4..) so adjustments required for height, weight, and strength measures
  CentreOrder <- lapply(CombineC[,  3:13], order, decreasing = TRUE)
  CentreOrder$X40.Yard <- order(CombineC$X40.Yard, decreasing = TRUE)
  CentreOrder$Shuttle  <- order(CombineC$Shuttle, decreasing = TRUE)
  CentreOrder$X3Cone   <- order(CombineC$X3Cone, decreasing = TRUE)
  CentreOrder <- as.data.frame(CentreOrder)
  CentreOrder$Name <- CombineC$Name
  CentreOrder$Year <- CombineC$Year
  
  
  