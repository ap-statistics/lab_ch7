# Activity 7.1B

# Generate a population of 60 test scores from normal distribution (mean=70, sd=2.5)
x <- rnorm(60,70,2.5)

# Take 4 random samples of size 5 and record the sample means
y <- replicate(4, sample(x, size=5, replace=FALSE))
apply(y, 2, mean)

# 1. 
## Simulate 5000 samples of 5 students and record the sample means
sp <- replicate(5000,sample(x, size=5, replace=FALSE))
spmeans <- apply(sp,2,mean)

## Plot the histogram of the 5000 sample means. The area of each bar in the histogram represents the percent of simulated samples whose sample means are within a particular class.
# Uncomment the line below if you want to place juxtapose two histograms in a 2 by 1 layout
# par(mfrow=c(2,1))
hist(spmeans, probability = TRUE, breaks=seq(65,75,0.5),ylim=c(0,0.8),
     main = "Approximate Sampling Distribution \nof Sample Mean Test Scores (n=5)", xlab="Sample Mean Test Scores")
## Add a vertical red line to represent the true pop. mean.
abline(v=mean(x), col="red")

### define function for population standard deviation
sdp <- function(x){
  n <- length(x)
  a <- sd(x)*sqrt((n-1)/n)
  return(a)
}

## Superimpose normal curve that approximates the sampling distribution of the sample mean test scores
v <- seq(66,74,length=200)
lines(v,dnorm(v,mean=mean(x), sd=sdp(x)/sqrt(5)))

# 5.
## Find the percentage of simulated samples whose sample means of 5 tests are <= 68.
sum(spmeans < 68)/length(spmeans)

# Extra:
## Show that increasing the sample size decreases the variability of the statistic
## Simulate 5000 samples of 5 students and record the sample means
sp20 <- replicate(5000,sample(x, size=20, replace=TRUE))
spmeans20 <- apply(sp20,2,mean)
hist(spmeans20, probability = TRUE, breaks=seq(65,75,0.25), ylim=c(0,0.8),
     main = "Approximate Sampling Distribution \nof Sample Mean Test Scores (n=20)", xlab="Sample Mean Test Scores")
## Red vertical line = true pop. mean
abline(v=mean(x), col="red")
## Superimpose normal curve that approximates the sampling distribution of the sample mean test scores
v <- seq(66,74,length=200)
lines(v,dnorm(v,mean=mean(x), sd=sdp(x)/sqrt(20)))

# Notice that replace=TRUE in the sample() function above, since n=20 is greater than 10% of the population size (N=60).
# Find out what happens if you change replace to FALSE.
