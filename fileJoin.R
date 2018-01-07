# Create cummulative college records for each player in rushing, passing, and receiving
# Obviously only possible for TE, WR, RB, QB
library(plyr)
library(tidyverse)

passing <- read.csv("passingcollegestats.csv", as.is = TRUE, strip.white = TRUE, header = TRUE)
head(passing)

rushing <- read.csv("rushingcollegestats.csv", as.is = TRUE, strip.white = TRUE, header = TRUE)
head(rushing)

receiving <- read.csv("receivingcollegestats.csv", as.is = TRUE, strip.white = TRUE, header = TRUE)
head(receiving)