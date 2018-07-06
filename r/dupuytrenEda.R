library(sas7bdat)
library(dplyr)
library(foreign)
library(rio)

setwd("/home/jorge/Documents/master/sixth-quartile/statistics-big-data")
dupuytrenData = read.sas7bdat("rcode/assignment_dd.sas7bdat")

#select random subjects and plot the area over time
subjects <- unique(dupuytrenData$ID)
first_hand_df <- dupuytrenData[dupuytrenData$Hand == 0, c("ID", "FU", "AREA")]
second_hand_df <- dupuytrenData[dupuytrenData$Hand == 1, c("ID", "FU", "AREA")]

first_hand_ids <- sample(unique(first_hand_df$ID), 6)
second_hand_ids <- sample(unique(second_hand_df$ID), 6)

first_hand_df <- first_hand_df[first_hand_df$ID %in% first_hand_ids, ]
second_hand_df <- second_hand_df[second_hand_df$ID %in% second_hand_ids, ]

X = seq(1, 12, by=1)
p = length(X)

for (i in 1:6){
  subjectId = first_hand_ids[i]
  Y = first_hand_df[first_hand_df$ID == subjectId, ]$AREA
  diff = p - length(Y)
  Y <- append(Y, rep(NA, diff))
  plot(X, Y, type="o", xlab="Follow Up", ylab="Area")
  title(main=paste("Subject with ID: ", subjectId))
}


for (i in 1:6){
  subjectId = second_hand_ids[i]
  Y = second_hand_df[second_hand_df$ID == subjectId, ]$AREA
  diff = p - length(Y)
  Y <- append(Y, rep(NA, diff))
  plot(X, Y, type="o", xlab="Follow Up", ylab="Area")
  title(main=paste("Subject with ID: ", subjectId))
}
