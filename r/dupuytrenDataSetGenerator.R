library(sas7bdat)
library(dplyr)
library(foreign)
library(rio)

#install.packages("sas7bdat")
#install.packages("dplyr")
#install.packages("foreign")
#install.packages("rio")
#install.packages("curl")
#install.packages("openxlsx")

setwd("/home/jorge/Documents/master/sixth-quartile/statistics-big-data")

dupuytrenData = read.sas7bdat("rcode/assignment_dd.sas7bdat")

dupuytrenData$AGE <- dupuytrenData$AGE0 + dupuytrenData$MONTH/12

length(unique(dupuytrenData$ID))

## We have 224 unique patients
table_df = as.data.frame.matrix(table(dupuytrenData$ID, dupuytrenData$Hand))
#dplyr::mutate(table_df, ID=row_number())

df_hand1 = subset(dupuytrenData, Hand == 0)
df_hand2 = subset(dupuytrenData, Hand == 1)

valid_subjects <- vector(mode="numeric", length=0)
valid_consistent_subjects = vector(mode="numeric", length=0)
subjects = unique(dupuytrenData$ID)

fu_threshold = 9;
for (subject in subjects){
  fu_counter = 0;
  fus_hand1 = df_hand1[df_hand1$ID == subject, ]$FU
  fus_hand2 = df_hand2[df_hand2$ID == subject, ]$FU
  add_subject = TRUE;
  if (length(fus_hand1) == length(fus_hand2)){
    for (fu in fus_hand1){
      if (!is.element(fu, fus_hand2)){
        add_subject = FALSE;
        break;
      }
      fu_counter = fu_counter + 1;
    }
  } else {
    add_subject = FALSE;
  }
    
  if (add_subject){
    valid_subjects <- c(valid_subjects, subject)
    if (fu_counter >= fu_threshold){
      valid_consistent_subjects <- c(valid_consistent_subjects, subject)
    }
  }
}

length(valid_subjects)
dupuytrenBothHands = dupuytrenData[dupuytrenData$ID %in% valid_subjects, ]
length(valid_consistent_subjects)
dupuytrenBothHandsConsistent = dupuytrenData[dupuytrenData$ID %in% valid_consistent_subjects, ]

write.csv(dupuytrenBothHands, "dp_both_hands.csv")
write.csv(dupuytrenBothHandsConsistent, "dp_both_hands_9fu.csv")

avoid_hand1 <- df_hand1[!complete.cases(df_hand1), ]$ID

#df_hand1 <- na.omit(df_hand1)
valid_consistent_subjects_hand1 = vector(mode="numeric", length=0)
subjects = unique(df_hand1$ID)

fu_threshold = 9;
for (subject in subjects){
  fu_counter = 0;
  fus_hand1 = df_hand1[df_hand1$ID == subject, ]$FU
  if (length(fus_hand1) >= fu_threshold && !(subject %in% avoid_hand1)){
    valid_consistent_subjects_hand1 <- c(valid_consistent_subjects_hand1, subject)
  }
}

length(valid_consistent_subjects_hand1)
dupuytrenHand1Consistent = df_hand1[df_hand1$ID %in% valid_consistent_subjects_hand1, ]

write.csv(df_hand1, "dp_hand1.csv")
write.csv(dupuytrenHand1Consistent, "dp_hand1_9fu.csv")

#dupuytrenHand1Consistent[dupuytrenHand1Consistent$ID == 85, ]

#m1 <- unique(dupuytrenHand1Consistent[dupuytrenHand1Consistent$ID == 145, ]$ID)


write.csv(df_hand2, "dp_hand2.csv")


hist(df_hand1$AREA)
hist(df_hand2$AREA)
hist(dupuytrenBothHands$AREA)

hist(dupuytrenBothHands$MONTH)
hist(dupuytrenBothHands$AGE0)

df_hands = dupuytrenBothHands[c("ID", "Hand", "AREA")]
dfh0 = df_hands[df_hands$Hand == 0, ]
dfh1 = df_hands[df_hands$Hand == 1, ]
#dfh = merge(dfh0, dfh1, by="ID")
cor(dfh0$AREA, dfh1$AREA)

summary(dupuytrenBothHands)
table(dupuytrenBothHands$ID, dupuytrenBothHands$Hand)
count(dupuytrenBothHands, "FU")

dupuytrenBothHands %>% count(FU)
hist(dupuytrenBothHands$FU,
     breaks=12,
     xlab="Follow up", 
     main="Histogram of follow ups for both hands")

