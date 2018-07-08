library(sas7bdat)
library(dplyr)
#library(foreign)
#library(rio)


setwd("/home/jorge/Documents/master/sixth-quartile/statistics-big-data")

dupuytrenData = read.sas7bdat("rcode/assignment_dd.sas7bdat")

#
dupuytrenData$AGE <- dupuytrenData$AGE0 + dupuytrenData$MONTH/12;
dupuytrenData$CAGE <- 0;
dupuytrenData$CAGE[dupuytrenData$AGE0 > 50] <- 1;
dupuytrenData$CAGE[dupuytrenData$AGE0 > 70] <- 2;



length(unique(dupuytrenData$ID))

## We have 224 unique patients
table_df = as.data.frame.matrix(table(dupuytrenData$ID, dupuytrenData$Hand))
#dplyr::mutate(table_df, ID=row_number())

df_hand1 = subset(dupuytrenData, Hand == 0)
df_hand2 = subset(dupuytrenData, Hand == 1)

fu_threshold = 8;
valid_subjects_hand1 = vector(mode="numeric", length=0)
subject_ids_hand1 = unique(df_hand1$ID)
for (subject in subject_ids_hand1){
  fus_hand1 = df_hand1[df_hand1$ID == subject, ]$FU
  add_subject_hand = TRUE;
  fu_counter = 0;
  while (fu_counter < fu_threshold){
    if (!is.element(fu_counter, fus_hand1)){
      add_subject_hand = FALSE;
      break;
    }
    fu_counter = fu_counter + 1;
  }

  if (add_subject_hand){
    valid_subjects_hand1 <- c(valid_subjects_hand1, subject)
  }
}
length(valid_subjects_hand1)

valid_subjects_hand2 = vector(mode="numeric", length=0)
subject_ids_hand2 = unique(df_hand2$ID)
for (subject in subject_ids_hand2){
  fus_hand2 = df_hand2[df_hand2$ID == subject, ]$FU
  add_subject_hand = TRUE;
  fu_counter = 0;
  while (fu_counter < fu_threshold){
    if (!is.element(fu_counter, fus_hand2)){
      add_subject_hand = FALSE;
      break;
    }
    fu_counter = fu_counter + 1;
  }
  
  if (add_subject_hand){
    valid_subjects_hand2 <- c(valid_subjects_hand2, subject)
  }
}
length(valid_subjects_hand2)

dupuytrenHand1 = df_hand1[df_hand1$ID %in% valid_subjects_hand1, ]
dupuytrenHand2 = df_hand2[df_hand2$ID %in% valid_subjects_hand2, ]


## month normalization
dupuytrenHand1$NORMONTH <- dupuytrenHand1$MONTH/6;
dupuytrenHand2$NORMONTH <- dupuytrenHand2$MONTH/6;

write.csv(dupuytrenHand1, "dp_hand1_8fu.csv")
write.csv(dupuytrenHand2, "dp_hand2_8fu.csv")

hist(dupuytrenHand1$AREA, xlab="AREA", main="AREA for subjects with measurements in hand 1")

