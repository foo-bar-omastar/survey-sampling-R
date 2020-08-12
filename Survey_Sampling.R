#We load the dataset using the 'import dataset' option in RStudio and then perform 
#all our calculations and solutions

families <- read.csv("families.txt")

#---------------------------------------------------------------------------------

# SCENARIO 1
#-------------
# Take a simple sample of 600 families. Estimate the following population
# parameters, calculate the estimated standard error of these estimates and form
# 95% confidence intervals:

#First we create a sample of 600 from given data and store it in an object.

sample_ex_a <- families[sample(1:nrow(families), 600), ]
N <- nrow(families) #Number of total rows in dataset
n <- 600  #Size of the sample



#-----------------------------------------------------------
#Part (i) - The proportion of husband-wife family (family type = 1 in the data)
#-----------------------------------------------------------

p <- nrow(sample_ex_a[sample_ex_a$X.TYPE.== 1, ])  #No. of husband-wife families

#Calculating the estimate of p
p_hat <- p/n    

#Calculating the estimated standard error of p_hat
s_p_hat <- (sqrt((p_hat*(1-p_hat))/(n-1)))*(sqrt(1-(n/N)))

#Calculating the 95% Confidence Intervals
i_p_plus = p_hat + (1.96*s_p_hat)
i_p_minus = p_hat - (1.96*s_p_hat)





#-----------------------------------------------------------
#Part (ii) - The average number of children per family
#-----------------------------------------------------------

#Calculating the estimate of mu_c:
#Taking the sum of elements in the children column of the sample
c_bar <- mean(sample_ex_a$X.CHILDREN.)


#Calculating the sample variance
sum_ci_cbar <- 0  #Setting the sum variable to zero for calculation

#Loop to calculate value of (c_i - c_bar)^2
for(i in 1:nrow(sample_ex_a)) {
  sum_ci_cbar = sum_ci_cbar + (sample_ex_a$X.CHILDREN.[i] - c_bar)^2
}

S_var_c <- (1/(n-1))*sum_ci_cbar

#can use the below formula to calculate variance too, if required
#S_var_c <- var(sample_ex_a$X.CHILDREN.)

#Calculating the estimated standard error of c_bar
s_c_bar <- (sqrt(((S_var_c))/(n)))*(sqrt(1-(n/N)))

#Calculating the 95% Confidence Intervals
i_c_plus = c_bar + (1.96*s_c_bar)
i_c_minus = c_bar - (1.96*s_c_bar)





#-----------------------------------------------------------
#Part (iii) - The average number of persons per family
#-----------------------------------------------------------

#Calculating the estimate of mu_p:
#Taking the sum of elements in the persons column of the sample
p_bar <- mean(sample_ex_a$X.PERSONS.)    

#Calculating the sample variance
sum_pi_pbar <- 0  #Setting the sum variable to zero for calculation

#Loop to calculate value of (p_i - p_bar)^2
for(i in 1:nrow(sample_ex_a)) {
  sum_pi_pbar = sum_pi_pbar + (sample_ex_a$X.PERSONS.[i] - p_bar)^2
}

S_var_p <- (1/(n-1))*sum_pi_pbar

#can use the below formula to calculate variance too, if required
#S_var_p <- var(sample_ex_a$X.PERSONS.)

#Calculating the estimated standard error of p_bar
s_p_bar <- (sqrt(((S_var_p))/(n)))*(sqrt(1-(n/N)))

#Calculating the 95% Confidence Intervals
i_pr_plus = p_bar + (1.96*s_p_bar)
i_pr_minus = p_bar - (1.96*s_p_bar)


#---------------------------------------------------------------------------------

# SCENARIO 2
#-------------
#Take 100 samples of size 400.



#-----------------------------------------------------------
#Part (i) - For each sample, find the average family income.
#-----------------------------------------------------------
#We first create 100 samples of size 400 each using a loop and save the
#average family income of each sample in an array. 
#Creating an empty array to store average family incomes of 100 samples
#Creating a similar empty array to store the variance of each sample
#Creating an empty array to store the estimated standard error for
#each of the 100 samples as well.
#we are calculating these values before hand to be have all information 
#handy to answer question part(i) to (iv)

n <- 400  #Initialising n=400 for size of each sample
avg_income_per_sample <- rep(0,100)
var_income_per_sample <- rep(0,100)
est_std_err_per_sample <- rep(0,100)

#A for loop to create 100 samples of size 400 for family income 
#and store average values simultaneously
for(i in 1:100) {
  sample_ex_b = sample(families$X.INCOME., 400) #sample of 400 per iteration
  avg_income_per_sample[i] <- mean(sample_ex_b) #sample mean per iteration
  var_income_per_sample[i] <- var(sample_ex_b)  #sample variance per iteration
  est_std_err_per_sample[i] <- (sqrt(((var_income_per_sample[i]))/(n)))*(sqrt(1-(n/N)))  
}


#Display the average sample incomes of all teh 100 samples
avg_income_per_sample
#Storing the averages across samples into another variable that would classify 
#the averages into 10 different intervals for easier reference in this case
income_interval <- cut(avg_income_per_sample,10,NULL,TRUE,FALSE)

#Below command displays a table with intervals and frequency of income at level
#The average income of families in every sample will be represented in this.
table(income_interval)




#-----------------------------------------------------------
#Part (ii) - Find the average and standard deviation of these 100 estimates 
#and make a histogram of the estimates.
#-----------------------------------------------------------


#To find the average of these 100 estimates, we use mean() function directly
ex_b_sample_mean <- mean(avg_income_per_sample)

#To find the standard deviation, we use the sd() function directly
ex_b_sample_sd <- sd(avg_income_per_sample)

#To plot the estimates, we use the hist() function as below
#Frequency Histogram of the estimates
hist(avg_income_per_sample,
     main="Frequency Histogram: Sample Mean Family Incomes",
     xlab="Sample Mean Family Incomes",
     col="skyblue3")
#Plotting the mean of all the 100 estimates
abline(v=ex_b_sample_mean,col="green", lwd=2)
#Density Histogram of the estimates
hist(avg_income_per_sample,
     main="Density Histogram: Sample Mean Family Incomes",
     xlab="Sample Mean Family Incomes",
     col="skyblue3",
     freq=FALSE, right=FALSE)
#Plotting the mean of all the 100 estimates
abline(v=ex_b_sample_mean,col="green", lwd=2)




#-----------------------------------------------------------
#Part (iii) - Superimpose a plot of a normal density with that mean and 
#standard deviation of the histogram and comment on how well it fits.
#-----------------------------------------------------------

#Creating the histogram
h <- hist(avg_income_per_sample, breaks = 10, density = 10,
          col = "skyblue3", xlab = "Sample Mean Family Incomes", main = "Histogram and Density Plot") 
xfit <- seq(min(avg_income_per_sample), max(avg_income_per_sample), length = 4000) 
yfit <- dnorm(xfit, mean = mean(avg_income_per_sample), sd = sd(avg_income_per_sample)) 
yfit <- yfit * diff(h$mids[1:2]) * length(avg_income_per_sample) 

#Adding the Density Plot
lines(xfit, yfit, col = "black", lwd = 2)

#Adding the mean of the samples
abline(v=ex_b_sample_mean,col="green", lwd=2)


#-----------------------------------------------------------
#Part (iv) - For each of the 100 samples, find a 95% confidence interval
#for the population average income. How many of those intervals actually
#contain the population target.
#-----------------------------------------------------------

#Using the values calculated above, we can calculate 95% confidence
#intervals for each of the samples by declaring empty arrays to store
#the upper and lower bound of the intervals for each sample.
#The estimated standard error for each sample has already been 
#calculated in part(i) of the exercise for ease of calculation.
#We will only directly call it here.

LB_CI_ex_b <- rep(0,100)
UB_CI_ex_b <- rep(0,100)
count <- 0

for(i in 1:100){
  #Calculating the Lower and Upper Bound of the Confidence Intervals
  LB_CI_ex_b[i] <- avg_income_per_sample[i] - (1.96*est_std_err_per_sample[i])
  UB_CI_ex_b[i] <- avg_income_per_sample[i] + (1.96*est_std_err_per_sample[i])
  
  #Checking if the population mean lies outside the confidence interval
  #count increased by 1 everytime the population mean is out of bounds
  if (mean(families$X.INCOME.) < LB_CI_ex_b[i]) 
    count = count + 1
  else if (mean(families$X.INCOME.) > UB_CI_ex_b[i]) 
    count = count + 1
}

#Print the counter to see how many intervals did not contain population mean 
count

#Creating a graphical representation of the confidence intervals to verify if 
#any of the samples exist that do not contain the population mean
plotCI(y=1:100, x=avg_income_per_sample,
       li=LB_CI_ex_b, ui=UB_CI_ex_b,
       sfrac=0.001, err="x",
       xlab="Confidence Interval of Mean Family Income",
       ylab="Sample No.",
       main="Confidence Interval vs Population Mean")
abline(v=mean(families$X.INCOME.), col="tomato2")

mean(LB_CI_ex_b)
mean(UB_CI_ex_b)


#-----------------------------------------------------------
#Part (v) - Take 100 samples of size 100. Compare the averages, standard 
# deviations and histograms to those obtained for a sample of size 400 and
#explain how the theory of simple random sampling relates to the comparisons.
#-----------------------------------------------------------

#We leverage the same code that we used to create samples earlier
#But we change the values to create 100 samples of size 100

avg_income_per_sample_2 <- rep(0,100)

#A for loop to create 100 samples of size 100 for family income 
#and store average values simultaneously
for(i in 1:100) {
  sample_ex_b_2 = sample(families$X.INCOME., 100) #sample of 100 per iteration
  avg_income_per_sample_2[i] <- mean(sample_ex_b_2) #sample mean per iteration
}


#To find the average of these 100 estimates, we use mean() function directly
ex_b_sample_mean_2 <- mean(avg_income_per_sample_2)

#To find the standard deviation, we use the sd() function directly
ex_b_sample_sd_2 <- sd(avg_income_per_sample_2)

#To plot the estimates, we use the hist() function as below
#Frequency Histogram of the estimates
hist(avg_income_per_sample_2,
     main="Frequency Histogram: Sample Mean Family Incomes",
     xlab="Sample Mean Family Incomes",
     col="skyblue3")
abline(v=ex_b_sample_mean_2,col="green", lwd=2)





#---------------------------------------------------------------------------------

# SCENARIO 3
#-------------
#Stratify the families into four strata by region (North, East, South and West).



#-----------------------------------------------------------
#Part (i) - What are the sampling fractions for proportional allocation and
#optimal allocation?
#-----------------------------------------------------------


#We have 4 Strata - North, South, East, West
#The population total is N as defined at the beginning of the program

#Let us first create the four Strata as asked
s1_north <- families[families$X.REGION.==1,]
s2_east <- families[families$X.REGION.==2,]
s3_south <- families[families$X.REGION.==3,]
s4_west <- families[families$X.REGION.==4,]
  

#FOR PROPORTIONAL ALLOCATION - 
#We will now calculate the sampling fractions w:
#We will be re-using these values of w for optimal allocation as well

w_north <- nrow(s1_north)/N
w_east <- nrow(s2_east)/N
w_south <- nrow(s3_south)/N
w_west <- nrow(s4_west)/N


#FOR OPTIMAL ALLOCATION - 
#We calculate the sampling fractions by the below formulae:

#The denominator of the optimal allocation formula is calculated by:
opt_alloc_denom <- 
  ((w_north)*(sd(s1_north$X.INCOME.)))+ 
  ((w_east)*(sd(s2_east$X.INCOME.))) + 
  ((w_south)*(sd(s3_south$X.INCOME.))) + 
  ((w_west)*(sd(s4_west$X.INCOME.)))

#Putting it in the below, we get the optimal allocation proportions:
opt_w_north <- ((w_north)*(sd(s1_north$X.INCOME.)))/opt_alloc_denom
opt_w_east <- ((w_east)*(sd(s2_east$X.INCOME.)))/opt_alloc_denom
opt_w_south <- ((w_south)*(sd(s3_south$X.INCOME.)))/opt_alloc_denom
opt_w_west <- ((w_west)*(sd(s4_west$X.INCOME.)))/opt_alloc_denom





#-----------------------------------------------------------
#Part (ii) - Allocate 500 observations proportionally to the four regions
# and estimate the average income from the stratified sample. Estimate the 
# standard error and form a 95% confidence interval. Compare your results 
# to the results of the simple random sample.
#-----------------------------------------------------------



#For a sample size of n = 500, we need to first calculate the proportional 
#sample size of each strata using the proportional allocation formula and
#variables used above to create the samples based on that. 

n <- 500
sample_size_north <- round(n*w_north)
sample_size_east <- round(n*w_east)
sample_size_south <- round(n*w_south)
sample_size_west <- round(n*w_west)

#Now we use the above proportional size to sample data from the population
strata_1_north <- sample(s1_north$X.INCOME., sample_size_north)
strata_2_east <- sample(s2_east$X.INCOME., sample_size_east)
strata_3_south <- sample(s3_south$X.INCOME., sample_size_south)
strata_4_west <- sample(s4_west$X.INCOME., sample_size_west)

#We calculate the stratified sample mean by summing up the weighted means 
#of each strata together as below:

stratified_sample_mean <- 
  (w_north*mean(strata_1_north)) + 
  (w_east*mean(strata_2_east)) + 
  (w_south*mean(strata_3_south)) + 
  (w_west*mean(strata_4_west))

#We calculate the stratified standard error by the below formula:
stratified_std_error <- sqrt(((w_north^2)*var(strata_1_north)/sample_size_north) + 
                           ((w_east^2)*var(strata_2_east)/sample_size_east) + 
                           ((w_south^2)*var(strata_3_south)/sample_size_south) + 
                           ((w_west^2)*var(strata_4_west)/sample_size_west))


#Calculating the 95% Confidence Intervals
stratified_ci_plus = stratified_sample_mean + (1.96*stratified_std_error)
stratified_ci_minus = stratified_sample_mean - (1.96*stratified_std_error)


#---------------------------------------------------------------------------------