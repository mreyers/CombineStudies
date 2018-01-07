# Web scraper

library("rvest")
############ PASSING ###########
passingList <- list()
jump <- seq(1987, 2017, by = 1)
baseURL <- paste("https://www.sports-reference.com/cfb/years/", jump, "-passing.html", sep = "")

passingList <- lapply(baseURL, function (i) {
  webpage <- read_html(i)
  draft_page <- html_nodes(webpage, "#passing")
  draft <- html_table(draft_page)[[1]]
})

str(passingList)
testDF <- passingList[[1]]
head(testDF)
names(testDF) <- paste0(names(testDF), testDF[1,])
paste0(names(testDF), testDF[1,])

test <- list()
for( i in 1:length(jump)){
  test[[i]] <- passingList[[i]]
  names(test[[i]]) <- paste0(names(test[[i]]), test[[i]][1,])
  test[[i]] <- test[[i]][-1, ]
  test[[i]]$Year <- jump[i]
  test[[i]] <- test[[i]][test[[i]]$School != "School",]
}
str(test)

finaldfpassing <- do.call(rbind, test)
write.csv(finaldfpassing, "passingcollegestats.csv")

######### RUSHING ###############
rushingList <- list()
baseURLRush <- paste("https://www.sports-reference.com/cfb/years/", jump, "-rushing.html", sep = "")

rushingList <- lapply(baseURLRush, function (j) {
  webpage <- read_html(j)
  draft_rush <- html_nodes(webpage, "#rushing")
  draftRush <- html_table(draft_rush)[[1]]
})

test2 <- list()
for( i in 1:length(jump)){
  test2[[i]] <- rushingList[[i]]
  names(test2[[i]]) <- paste0(names(test2[[i]]), test2[[i]][1,])
  test2[[i]] <- test2[[i]][-1, ]
  test2[[i]]$Year <- jump[i]
  test2[[i]] <- test2[[i]][test2[[i]]$School != "School",]
}

finaldfrushing <- do.call(rbind, test2)
write.csv(finaldfrushing, "rushingcollegestats.csv")

########## RECEIVING ################
receivingList <- list()
baseURLReceive <- paste("https://www.sports-reference.com/cfb/years/", jump, "-receiving.html", sep = "")

receivingList <- lapply(baseURLReceive, function (k) {
  webpage <- read_html(k)
  draft_rec <- html_nodes(webpage, "#receiving")
  draftRec <- html_table(draft_rec)[[1]]
})

test3 <- list()
for( i in 1:length(jump)){
  test3[[i]] <- receivingList[[i]]
  names(test3[[i]]) <- paste0(names(test3[[i]]), test3[[i]][1,])
  test3[[i]] <- test3[[i]][-1, ]
  test3[[i]]$Year <- jump[i]
  test3[[i]] <- test3[[i]][test3[[i]]$School != "School",]
}

finaldfreceiving <- do.call(rbind, test3)
write.csv(finaldfreceiving, "receivingcollegestats.csv")
