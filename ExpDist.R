## Cousera Statistical Inference Project Part 1 - Allen Seol
## Title: An Exploration of the Expotential Distribution and the Central Limit Theorem

#Parameters and setting seed
set.seed(1337)
lambda = .2
n = 40
sims = NULL ##Matrix with simulations 
avg = NULL ##vector with Averages

#1000 Simluations of 40 simulations of expotential distribution
for(i in 1:1000) {
  sims <- rbind(sims, rexp(n,.2))
}

#Run Apply Function to get mean of each simluations of 40 samples
avg = apply(sims,1,mean)
  
#mean of the 1000 simulations
mean_avg <- mean(avg)
print("Mean of 1000 simluations:")
print(mean_avg)
print("Theoretical mean is 5")

#variance of the 1000 simluations
var_avg <- mean(apply(sims,1,var))
print("Variance of 1000 simluations:")
print(var_avg)
print("Theoretical Variance is 25")

#Histogram of means of 40 samples x 1000 times
hist(avg)
abline(v=5,col=3) ##Green line Shows Theoretical Mean of 5
abline(v=mean_avg,col=4) ##Blue Line shows Actual Mean Calculated

#qqplot of values
qqnorm(avg)
qqline(avg,col=2)


