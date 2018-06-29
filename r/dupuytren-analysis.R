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
length(unique(dupuytrenData$ID))

## We have 224 unique patients
#table_df = as.data.frame.matrix(table(dupuytrenData$ID, dupuytrenData$Hand))
#dplyr::mutate(table_df, ID=row_number())

df_hand1 = subset(dupuytrenData, Hand == 0)
df_hand2 = subset(dupuytrenData, Hand == 1)

valid_subjects <- vector(mode="numeric", length=0)
subjects = unique(dupuytrenData$ID)


for (subject in subjects){
  fus_hand1 = df_hand1[df_hand1$ID == subject, ]$FU
  fus_hand2 = df_hand2[df_hand2$ID == subject, ]$FU
  add_subject = TRUE;
  if (length(fus_hand1) == length(fus_hand2)){
    for (fu in fus_hand1){
      if (!is.element(fu, fus_hand2)){
        add_subject = FALSE;
        break;
      }
    }
  } else {
    add_subject = FALSE;
  }
    
  if (add_subject){
    valid_subjects <- c(valid_subjects, subject)
  }
}

length(valid_subjects)
dupuytrenBothHands = dupuytrenData[dupuytrenData$ID %in% valid_subjects, ]

dupuytrenBothHands %>% distinct('ID', 'AGE0')

## Confirm visually that both hands have the same amount of Follow up measurements
#table(dupuytrenBothHands$ID, dupuytrenBothHands$Hand)

write.csv(df_hand1, "dp_hand1.csv")
write.csv(df_hand2, "dp_hand2.csv")

write.csv(dupuytrenBothHands, "dp_both_hands.csv")
export(dupuytrenBothHands, "dp_both_hands.sas7bdat")

write.foreign(mydata, "c:/mydata.txt", "c:/mydata.sas", package="SAS")

