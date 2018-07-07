setwd("/home/jorge/Documents/master/sixth-quartile/statistics-big-data")

dp_hand1_filename = "dp_hand1_8fu.csv";
dp_hand2_filename = "dp_hand2_8fu.csv";

dp_hand1 <- read.csv(dp_hand1_filename);
dp_hand2 <- read.csv(dp_hand2_filename);

# count number of users per sex
subject_ids_hand1 = unique(dp_hand1$ID);
subject_ids_hand2 = unique(dp_hand2$ID);

sex0_hand1 = 0;
sex1_hand1 = 0;
sex0_hand2 = 0;
sex1_hand2 = 0;

for (subject_id in subject_ids_hand1){
  sex_vector = dp_hand1[dp_hand1$ID == subject_id, ]$SEX;
  if (sex == 0){
    print("sex == 0");
    sex0_hand1 = sex0_hand1 + 1;
  } else {
    print("sex == 1");
    sex1_hand1 = sex1_hand1 + 1;
  }
}

for (subject_id in subject_ids_hand2){
  sex_vector = dp_hand2[dp_hand2$ID == subject_id, ]$SEX;
  sex = sex_vector[1];
  if (sex == 0){
    sex0_hand2 = sex0_hand2 + 1;
  } else {
    sex1_hand2 = sex1_hand2 + 1;
  }
}

print(paste("Subjects with sex=0 for hand 1: ", sex0_hand1));
print(paste("Subjects with sex=1 for hand 1: ", sex1_hand1));
print(paste("Subjects with sex=0 for hand 2: ", sex0_hand2));
print(paste("Subjects with sex=1 for hand 2: ", sex1_hand2));


hist(dp_hand1$AGE0, xlab="Age", main="Age for subjects with measurements in hand 1")
hist(dp_hand2$AGE0, xlab="Age", main="Age for subjects with measurements in hand 2")
summary(dp_hand1$AGE0)
summary(dp_hand2$AGE0)
