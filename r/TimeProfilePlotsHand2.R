setwd("/home/jorge/Documents/master/sixth-quartile/statistics-big-data")

dp_hand2_filename = "dp_hand2_8fu.csv";
dp_hand2 <- read.csv(dp_hand2_filename);


# parameters for model of hand2
intercept = 3.3908;
lambda_cage_0 = 0;
lambda_cage_1 = -0.6471;
lambda_cage_2 = -0.9525;
lambda_cage_3 = -0.7356;
lambda_month = 0.03864;
lambda_sex_0 = 0;
lambda_sex_1 = -0.8418;
var_random_intercept = 4.5045;
var_random_month = 0.002402;
var_ar1 = 0.1958;
var_residual = 0.8018;

time_points = 9;
# profiles for ids: 36, 95, 14, 132, 134, 40
#subjects_to_plot <- c(36, 95, 14, 132, 134, 40);
#summary(dp_hand2$MONTH)
t_values <- seq(0, 56, length = time_points);

subject_sex0_cage0 = 33;
subject_sex0_cage1 = 95;
subject_sex0_cage2 = 54;
subject_sex0_cage3 = 2;
subject_sex1_cage0 = 173;
subject_sex1_cage1 = 7;
subject_sex1_cage2 = 4;
subject_sex1_cage3 = 28;

y_subject_sex0_cage0 <- numeric(time_points);
y_subject_sex0_cage1 <- numeric(time_points);
y_subject_sex0_cage2 <- numeric(time_points);
y_subject_sex0_cage3 <- numeric(time_points);
y_subject_sex1_cage0 <- numeric(time_points);
y_subject_sex1_cage1 <- numeric(time_points);
y_subject_sex1_cage2 <- numeric(time_points);
y_subject_sex1_cage3 <- numeric(time_points);
y_avg <- numeric(time_points); 

subjects <- unique(dp_hand2$ID);
number_subjects <- length(subjects);

random_intercepts <- rnorm(number_subjects, 
                           intercept,
                           var_random_intercept);
random_slopes <- rnorm(number_subjects,
                       lambda_month,
                       var_random_month);
random_residuals <- rnorm(number_subjects,
                          0, var_residual);


for (i in 1:length(subjects)){
  subject_id = subjects[i];
  subject_sex <- dp_hand2[dp_hand2$ID == subject_id, ]$SEX[1];
  #print(subject_sex);
  lambda_sex = lambda_sex_0;
  if (subject_sex == 1){
    lambda_sex = lambda_sex_1;  
  }
  
  subject_cage <- dp_hand2[dp_hand2$ID == subject_id, ]$CAGE[1];
  lambda_cage = lambda_cage_0;
  #print(subject_cage);
  if (subject_cage == 1){
    lambda_cage = lambda_cage_1;
  } else if (subject_cage == 2){
    lambda_cage = lambda_cage_2;
  } else if (subject_cage == 3){
    lambda_cage = lambda_cage_3;
  }
  
  random_intercept = random_intercepts[i];
  random_slope = random_slopes[i];
  residual = var_ar1 + random_residuals[i];
  # compute time for 
  
  y_i <- numeric(time_points);
  for (j in 1:length(t_values)){
    t = t_values[j];
    yij = lambda_cage + lambda_sex + (lambda_month*t) + intercept 
    + random_slope*t + residual;
    y_i[j] = yij;
    y_avg[j] = y_avg[j] + yij;
  }
  
  if (subject_id == subject_sex0_cage0){
    y_subject_sex0_cage0 <- y_i;
  } else if (subject_id == subject_sex0_cage1){
    y_subject_sex0_cage1 <- y_i;
  } else if (subject_id == subject_sex0_cage2){
    y_subject_sex0_cage2 <- y_i;
  } else if (subject_id == subject_sex0_cage3){
    y_subject_sex0_cage3 <- y_i;
  }else if (subject_id == subject_sex1_cage0){
    y_subject_sex1_cage0 <- y_i;
  } else if (subject_id == subject_sex1_cage1){
    y_subject_sex1_cage1 <- y_i;
  } else if (subject_id == subject_sex1_cage2){
    y_subject_sex1_cage2 <- y_i;
  } else if (subject_id == subject_sex1_cage3){
    y_subject_sex1_cage3 <- y_i;
  }
}

for (j in 1:length(t_values)){
  y_avg[j] = y_avg[j]/number_subjects;
}


# average line
plot(t_values, y_avg, ylim=c(0, 10), type="b",
     col="black", lwd=2, pch=15, xlab="Follow up (months)", ylab="Area", main="Second hand time profiles");
# lines for subjects with sex0
lines(t_values, y_subject_sex0_cage0, type="b", col="red", lwd=1, pch=3);
lines(t_values, y_subject_sex0_cage1, type="b", col="red", lwd=1, pch=3);
lines(t_values, y_subject_sex0_cage2, type="b", col="red", lwd=1, pch=3);
lines(t_values, y_subject_sex0_cage3, type="b", col="red", lwd=1, pch=3);

# lines for subjects with sex1
lines(t_values, y_subject_sex1_cage0, type="b", col="blue", lwd=1, pch=3);
lines(t_values, y_subject_sex1_cage1, type="b", col="blue", lwd=1, pch=3);
lines(t_values, y_subject_sex1_cage2, type="b", col="blue", lwd=1, pch=3);
lines(t_values, y_subject_sex1_cage3, type="b", col="blue", lwd=1, pch=3);

first_subject_label = "Sex 0";
second_subject_label = "Sex 1";
legend("topright", legend=c("Average", first_subject_label, second_subject_label),
       col=c("black","red", "blue"), lty=c(2, 2, 2), pch=c("o", "+", "+"))


# plots for second model
