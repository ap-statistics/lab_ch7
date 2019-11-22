#Activity 7.2

# Create population of 1000 Reese's pieces.
# 40% of the pieces are orange (labeled as 1)
x <- c(rep(1,400), rep(0,600))

# 2.
# Draw a sample of 50 Reese's pieces. Repeat 5 times.
y <- replicate(5, sample(x, size=50, replace=FALSE))
apply(y, 2, sum)

# Repeated samples of size n=50. 
# Calculate the sample mean for each sample.
s <- replicate(10000, sample(x,size=50, replace=FALSE))
d <- apply(s,2,sum)

# 3 - 6.
# Approximate sampling distribution of X (number of orange candies in sample)
# Superimpose normal density curve
hist(d, prob=TRUE, breaks=seq(0.5,35.5,1), 
     main="Approximate Sampling Distribution \nof the Number of Orange Candies (n=50)", xlab = "Number of Orange Candies")
v <- seq(10,30,length=200)
lines(v,dnorm(v,mean=20, sd=sqrt(50*0.4*0.6)))

# 7. 
## Use the samples from 2. to find the sample proportions.
apply(y, 2, mean)

# 8 - 11.
## Approx sampling distribution of X/n (proportion of orange candies in sample)

d2 <- apply(s,2,mean)
hist(d2, prob=TRUE,breaks=seq(0.05,0.85,0.02),
     main = "Approximate Sampling Distribution \nof the Proportion of Orange Candies (n=50)", xlab="Proportion of Orange Candies")
v <- seq(0,1,length=200)
lines(v,dnorm(v,mean=0.4, sd=sqrt(0.4*0.6/50)))

# 12.
## What is the probability that the sample proportion of a random sample of 50 candies will have 45% of more orange candies?

# using z scores
z <- (0.45 - 0.4)/sqrt(0.4*0.6/50)
# pnorm: normalcdf in your calculator
1-pnorm(z)
# calculate area of the standard normal curve to the right of the z score 
pnorm(z, lower.tail = FALSE)

# using proportion directly
1-pnorm(0.45, mean = 0.4, sd = sqrt(0.4*0.6/50))

