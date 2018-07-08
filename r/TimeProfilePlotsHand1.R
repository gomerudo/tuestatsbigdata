setwd("/home/jorge/Documents/master/sixth-quartile/statistics-big-data/rcode")

dp_hand1_filename = "dp_hand1_8fu.csv";

dp_hand1 <- read.csv(dp_hand1_filename);


# parameters for model of hand1
intercept = 2.9420;
lambda_cage_0 = 0;
lambda_cage_1 = -0.05411;
lambda_cage_2 = -0.5040;
lambda_cage_3 = -0.1966;
lambda_month = 0.04751;
lambda_sex_0 = 0;
lambda_sex_1 = -1.4448;
var_random_intercept = 4.6798;
var_random_month = 0.003798;
var_ar1 = 0.4541;
var_residual = 0.7330;

time_points = 9;
t_values <- seq(0, 56, length = time_points);

subject_sex0_cage0 = 33;
subject_sex0_cage1 = 85;
subject_sex0_cage2 = 46;
subject_sex0_cage3 = 2;
subject_sex1_cage0 = 173;
subject_sex1_cage1 = 35;
subject_sex1_cage2 = 4;
subject_sex1_cage3 = 51;


y_subject_sex0_cage0 <- numeric(time_points);
y_subject_sex0_cage1 <- numeric(time_points);
y_subject_sex0_cage2 <- numeric(time_points);
y_subject_sex0_cage3 <- numeric(time_points);
y_subject_sex1_cage0 <- numeric(time_points);
y_subject_sex1_cage1 <- numeric(time_points);
y_subject_sex1_cage2 <- numeric(time_points);
y_subject_sex1_cage3 <- numeric(time_points);
y_avg <- numeric(time_points); 

subjects <- unique(dp_hand1$ID);
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
  subject_sex <- dp_hand1[dp_hand1$ID == subject_id, ]$SEX[1];
  #print(subject_sex);
  lambda_sex = lambda_sex_0;
  if (subject_sex == 1){
    lambda_sex = lambda_sex_1;  
  }

  subject_cage <- dp_hand1[dp_hand1$ID == subject_id, ]$CAGE[1];
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
  } else if (subject_id == subject_sex1_cage0){
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
     col="black", lwd=2, pch=15, xlab="Follow up (months)", ylab="Area", main="First hand time profiles");
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

