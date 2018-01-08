# Create cummulative college records for each player in rushing, passing, and receiving
# Obviously only possible for TE, WR, RB, QB
library(plyr)
library(tidyverse)

cleanup <- function(df){
  df$Name <- gsub("[*]", "", df$Player)
  df <- df %>% select(-X, -Rk, -Conf, -Player)
  return(df)
}

passing <- read.csv("passingcollegestats.csv", as.is = TRUE, strip.white = TRUE, header = TRUE) %>% cleanup() %>% ddply(.(Name), summarize,
                                                                                                                        Games = sum(G),
                                                                                                                        Rating = max(PassingRate),
                                                                                                                        CareerCmp = sum(PassingCmp),
                                                                                                                        CareerAtt = sum(PassingAtt),
                                                                                                                        # CareerPct <- mean(PassignPct), Calculate this stat after
                                                                                                                        CareerYds = sum(PassingYds),
                                                                                                                        # YardsPerAttempt <- mean(PassingY.A), Calculate this stat after
                                                                                                                        # AdjYdsPerAttempt<- mean(PassingAY.A), Calculate this stat after
                                                                                                                        CareerPassTD = sum(PassingTD),
                                                                                                                        CareerInt = sum(PassingInt),
                                                                                                                        CareerRating = mean(PassingRate),
                                                                                                                        RushAtt = sum(RushingAtt),
                                                                                                                        RushYds = sum(RushingAvg),
                                                                                                                        #  RushAvg <- mean(), Calculate this stat after
                                                                                                                        RushTD = sum(RushingTD),
                                                                                                                        Start = min(Year),
                                                                                                                        End = max(Year))



rushing <- read.csv("rushingcollegestats.csv", as.is = TRUE, strip.white = TRUE, header = TRUE) %>% cleanup() %>% ddply(.(Name), summarize,
                                                                                                                        Attempts = sum(RushingAtt),
                                                                                                                        RushYards= sum(RushingYds),
                                                                                                                        RushTD   = sum(RushingTD),
                                                                                                                        Receptions=sum(ReceivingRec),
                                                                                                                        RecYards = sum(ReceivingYds),
                                                                                                                        RecTD    = sum(ReceivingTD),
                                                                                                                        Plays    = sum(ScrimmagePlays),
                                                                                                                        ScrimYds = sum(ScrimmageYds),
                                                                                                                        ScrimTD  = sum(ScrimmageTD),
                                                                                                                        Start = min(Year),
                                                                                                                        End = max(Year))
head(rushing)

receiving <- read.csv("receivingcollegestats.csv", as.is = TRUE, strip.white = TRUE, header = TRUE) %>% cleanup() %>% ddply(.(Name), summarize,
                                                                                                                            Receptions = sum(ReceivingRec),
                                                                                                                            RecYards   = sum(ReceivingYds),
                                                                                                                            RecTD      = sum(ReceivingTD),
                                                                                                                            RushAtt    = sum(RushingAtt),
                                                                                                                            RushYds    = sum(RushingYds),
                                                                                                                            RushTD     = sum(RushingTD),
                                                                                                                            Plays      = sum(ScrimmagePlays),
                                                                                                                            ScrimYds   = sum(ScrimmageYds),
                                                                                                                            ScrimTD    = sum(ScrimmageTD),
                                                                                                                            Start = min(Year),
                                                                                                                            End = max(Year))
head(receiving)


# Merge relevant extra data to the QB, RB, WR, TE, FB, HB
# QBPerf <- merge(CombineQB, passing, by = "Name")
# write.csv(QBPerf, "CombineAndCollegeQB.csv")
# 
# RBPerf <- merge(CombineRB, rushing, by = "Name")
# write.csv(RBPerf, "CombineAndCollegeRB.csv")
# 
# WRPerf <- merge(CombineWR, receiving, by = "Name")
# write.csv(WRPerf, "CombineAndCollegeWR.csv")
# 
# TEPerf <- merge(CombineTE, receiving, by = "Name")
# write.csv(TEPerf, "CombineAndCollegeTE.csv")
# 
# FBPerf <- merge(CombineFB, rushing, by = "Name")
# write.csv(FBPerf, "CombineAndCollegeFB.csv")


