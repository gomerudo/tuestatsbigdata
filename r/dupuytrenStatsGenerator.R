library(sas7bdat)
library(dplyr)
library(foreign)
library(rio)

setwd("/home/jorge/Documents/master/sixth-quartile/statistics-big-data")
dupuytrenData = read.sas7bdat("rcode/assignment_dd.sas7bdat")
dupuytrenData$AGE <- dupuytrenData$AGE0 + dupuytrenData$MONTH/12
uniqueSubjects <- length(unique(dupuytrenData$ID))
## We have 224 unique patients
table_df = as.data.frame.matrix(table(dupuytrenData$ID, dupuytrenData$Hand))
table_df

#dplyr::mutate(table_df, ID=row_number())
df_hand1 = subset(dupuytrenData, Hand == 0)
df_hand2 = subset(dupuytrenData, Hand == 1)

subjects = unique(dupuytrenData$ID)

#Subjects with the same amount of sequential measurements in both hands
fu_threshold = 12;
valid_subjects <- vector(mode="numeric", length=0)
for (subject in subjects){
  fus_hand1 = df_hand1[df_hand1$ID == subject, ]$FU
  fus_hand2 = df_hand2[df_hand2$ID == subject, ]$FU
  add_subject = TRUE;
  fu_counter = 0;
  if (length(fus_hand1) < fu_threshold || length(fus_hand2) < fu_threshold){
    add_subject = FALSE;
  }

  if (add_subject){
    valid_subjects <- c(valid_subjects, subject)
  }
}
length(valid_subjects)


#Subjects with the same amount of sequential measurements in at least one hand
fu_threshold = 12;
valid_subjects_one_hand <- vector(mode="numeric", length=0)
for (subject in subjects){
  fus_hand1 = df_hand1[df_hand1$ID == subject, ]$FU
  fus_hand2 = df_hand2[df_hand2$ID == subject, ]$FU
  add_subject = TRUE;
  fu_counter = 0;
  if (length(fus_hand1) < fu_threshold && length(fus_hand2) < fu_threshold){
    add_subject = FALSE;
  }
  
  if (add_subject){
    valid_subjects_one_hand <- c(valid_subjects_one_hand, subject)
  }
}
length(valid_subjects_one_hand)



#Subjects with the same amount of sequential measurements in both hands
fu_threshold = 1;
valid_consistent_subjects = vector(mode="numeric", length=0)
for (subject in subjects){
  fus_hand1 = df_hand1[df_hand1$ID == subject, ]$FU
  fus_hand2 = df_hand2[df_hand2$ID == subject, ]$FU
  add_subject = TRUE;
  fu_counter = 0;
  while (fu_counter < fu_threshold){
    if (!is.element(fu_counter, fus_hand1) || !is.element(fu_counter, fus_hand2)){
      add_subject = FALSE;
      break;
    }
    fu_counter = fu_counter + 1;
  }
  
  if (add_subject){
    valid_consistent_subjects <- c(valid_consistent_subjects, subject)
  }
}
length(valid_consistent_subjects)


fu_threshold = 12;
valid_consistent_subjects_one_hand = vector(mode="numeric", length=0)
for (subject in subjects){
  fus_hand1 = df_hand1[df_hand1$ID == subject, ]$FU
  fus_hand2 = df_hand2[df_hand2$ID == subject, ]$FU
  add_subject_hand1 = TRUE;
  add_subject_hand2 = TRUE;
  fu_counter = 0;
  while (fu_counter < fu_threshold){
    if (!is.element(fu_counter, fus_hand1)){
      add_subject_hand1 = FALSE;
      break;
    }
    fu_counter = fu_counter + 1;
  }
  
  fu_counter = 0;
  while (fu_counter < fu_threshold){
    if (!is.element(fu_counter, fus_hand2)){
      add_subject_hand2 = FALSE;
      break;
    }
    fu_counter = fu_counter + 1;
  }
  
  if (add_subject_hand1 || add_subject_hand2){
    valid_consistent_subjects_one_hand <- c(valid_consistent_subjects_one_hand, subject)
  }
}
length(valid_consistent_subjects_one_hand)


dupuytrenBothHands = dupuytrenData[dupuytrenData$ID %in% valid_subjects, ]
dupuytrenBothHandsConsistent = dupuytrenData[dupuytrenData$ID %in% valid_consistent_subjects, ]

followups <- seq(1, 12, by=1)
consistentBothHands <- c(148, 140, 131, 118, 110, 101, 92, 89, 79, 62, 3, 1);
consistentOneHand <- c(224, 218, 210, 200, 188, 173, 159, 153, 139, 113, 10, 2);
followUpsDF <- data.frame("followUps" = followups, "BothHands" = consistentBothHands, "AtLeastOneHand" = consistentOneHand);

plot(followUpsDF$followUps, followUpsDF$AtLeastOneHand,  type="o", col="blue", xlab="Follow ups", ylab="# subjects with measures")
lines( followUpsDF$followUps, followUpsDF$BothHands, lty=2, pch="+", col="red")
legend("topright", legend=c("At least one hand","Both hands"), col=c("blue","red"), lty=c(1,2), pch=c("o", ""))


## compute correlation values for both hands for each follow up
#df_hand1 = subset(dupuytrenData, Hand == 0)
#df_hand2 = subset(dupuytrenData, Hand == 1)


#Subjects with the same amount of sequential measurements in both hands
followUpId = 11
fu_threshold = followUpId + 1;
valid_consistent_subjects = vector(mode="numeric", length=0)
for (subject in subjects){
  fus_hand1 = df_hand1[df_hand1$ID == subject, ]$FU
  fus_hand2 = df_hand2[df_hand2$ID == subject, ]$FU
  add_subject = TRUE;
  fu_counter = 0;
  while (fu_counter < fu_threshold){
    if (!is.element(fu_counter, fus_hand1) || !is.element(fu_counter, fus_hand2)){
      add_subject = FALSE;
      break;
    }
    fu_counter = fu_counter + 1;
  }
  
  if (add_subject){
    valid_consistent_subjects <- c(valid_consistent_subjects, subject)
  }
}
length(valid_consistent_subjects)
dupuytrenBothHandsConsistent = dupuytrenData[dupuytrenData$ID %in% valid_consistent_subjects, ]

consistentDFHand1 = subset(dupuytrenBothHandsConsistent, Hand == 0)
consistentDFHand2 = subset(dupuytrenBothHandsConsistent, Hand == 1)

corDFHand1 = consistentDFHand1[consistentDFHand1$FU == followUpId, ]
corDFHand2 = consistentDFHand2[consistentDFHand2$FU == followUpId, ]
cor(corDFHand1$AREA, corDFHand2$AREA)
followUpId