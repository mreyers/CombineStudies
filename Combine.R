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
  # rank function gives lowest rank to lowest value (rank 1 is min value, good for timed events but opposite for others)
  # CentreOrder <- lapply(CombineC[, 3:13], order, decreasing = TRUE)
  # 
  # CentreOrder$X40.Yard <- order(-CombineC$X40.Yard, decreasing = TRUE)
  # CentreOrder$Shuttle  <- order(-CombineC$Shuttle, decreasing = TRUE)
  # CentreOrder$X3Cone   <- order(-CombineC$X3Cone, decreasing = TRUE)
  # CentreOrder <- as.data.frame(CentreOrder)
  # CentreOrder$Name <- CombineC$Name
  # CentreOrder$Year <- CombineC$Year
  # 
  # head(CentreOrder[order(CentreOrder$Weight),])
  # head(CombineC[order(CombineC$X40.Yard, decreasing = TRUE, na.last = TRUE), ])
  
# Function to create a dataframe of the rankings for each player at each stat, done manually because of lapply order problems  
PositionRanking <- function(df, ties){
  posOrder <- df
  NACounts <- lapply(as.data.frame(is.na(df)), sum)
  posOrder$Height <- rank(-df$Height, ties.method = ties)
  posOrder$Weight <- rank(-df$Weight, ties.method = ties)
  posOrder$X40.Yard <- rank(df$X40.Yard, ties.method = ties)
  posOrder$Bench.Press <- rank(-df$Bench.Press, ties.method = ties)
  posOrder$VertLeap <- rank(-df$VertLeap, ties.method = ties)
  posOrder$BroadJump <- rank(-df$BroadJump, ties.method = ties)
  posOrder$Shuttle <- rank(df$Shuttle, ties.method = ties)
  posOrder$X3Cone <- rank(df$X3Cone, ties.method = ties)
  posOrder$OverallRank <- rowSums(posOrder[, c(5, 6, 8:13)])
  posOrder$NonGenetic <- rowSums(posOrder[, 8:13])
  posOrder$SPARQ <- posOrder %>% select(X40.Yard, Bench.Press, VertLeap, Shuttle) %>% rowSums()
  return(list(posOrder, NACounts))
}  
# Function returns rankings with NA values listed last, meaning that discrepancies between the base file and the new sorted file are due to the number of NA values in a given column
# Can resolve this by also returning # of NA values per column and knocking these off each individual column when considering things
COrder <- PositionRanking(CombineC, "min")
CRank <- PositionRanking(CombineC, "min")[[1]]
NACount <- PositionRanking(CombineC, "min")[[2]]
CRank[order(CRank$SPARQ, decreasing = FALSE)[1:10],]

RBOrder <- PositionRanking(CombineRB, "min")
RBRank <- PositionRanking(CombineRB, "min")[[1]]
NACount <- PositionRanking(CombineRB, "min")[[2]]
head(RBRank[order(RBRank$OverallRank, decreasing = FALSE),])
head(RBRank[order(RBRank$NonGenetic, decreasing = FALSE),])
temp <- RBRank[RBRank$Year >= 2014, ]
temp[order(temp$SPARQ, decreasing = FALSE)[1:10], ]
# Do some plotting to understand how ranks differ based on player size
rankPlotCentres <- ggplot(data = CRank, aes(x = OverallRank, y = NonGenetic))+
  ggtitle("Comparison of Rank including/excluding Height and Weight") + xlab("With Height and Weight") + ylab("Without")+
  geom_point()
cor(CRank$OverallRank, CRank$NonGenetic)

rankPlotRBs <- ggplot(data = RBRank, aes(x = OverallRank, y = NonGenetic))+
  ggtitle("Comparison of Rank including/excluding Height and Weight") + xlab("With Height and Weight") + ylab("Without")+
  geom_point()
cor(RBRank$OverallRank, RBRank$NonGenetic)
# Plots seem to indicate that rankings are consistent regardless of Height/Weight

# Other idea: Create a function that returns the associated z-score of each player, being sure to reverse it for the timed drills
positionZScore <- function(df){
  physical <- apply(df[, c(5, 6, 9, 10, 11)], 2, function(x) (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))
  timed <- apply(df[, c(8, 12, 13)], 2, function(x) (mean(x, na.rm = TRUE) - x)/sd(x, na.rm = TRUE))
  zScores <- as.data.frame(cbind(df$Name, physical, timed)) 
  cols <- c(2:9)
  zScores[, cols] <- apply(zScores[, cols], 2, as.numeric) 
  print(str(zScores))
  zScores$GeneticOutlier <- rowSums(zScores[, 2:9], na.rm = TRUE)
  zScores$NonGeneticOut  <- rowSums(zScores[, 4:9], na.rm = TRUE)
  zScores$SPARQ          <- rowSums(zScores[, c(4, 5, 7, 8)])
  return(zScores)
}
zScoresCentres <- positionZScore(CombineC)
hist(zScoresCentres$GeneticOutlier)
hist(zScoresCentres$NonGeneticOut)


# SPARQ:  SPARQ input factors are 40-yard dash, vertical jump, 20-yard shuttle, and the bench press

############ Get College performance data for the following ###########

# Other measure Scholar Supremacy Score
  # Running Backs: YPC > 5.25, Receptions > 50, 40 Yard <4.5, 3 cone < 7, 20 yard shuttle < 4.35
  # Wide receiver: 150+ receptions, ypr > 15, % of teams pass yards > 33%, 40 yard < 4.5, 3 cone < 6.95, 20 yard shuttle < 4.1