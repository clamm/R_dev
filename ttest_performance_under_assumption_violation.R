
# generate a small sample of a skewed distribution
n_x <- 20
x <- rgamma(n_x,20,0.1)
mean(x)
plot(x)
plot(density(x))


# generate a x-times larger sample with the same mean than the first sample
y <- rchisq(3*n_x,mean(x))
mean(y)
# plot(density(y))

#cor.test(x,y)

# Can the t.test confirm that these 2 samples have the same mean?
t.test(x,y)


sampleGammaDistribution <- function(n) {
  rgamma(n,9,9)
}
sampleGammaDistribution(n_x)


sampleChisqDistribution <- function(n,mu) {
  rchisq(3*n,mean(mu))
}
sampleChisqDistribution(n_x, 2)


createSamplesAndRunTTest <- function(n_x) {
  # create samples with the same mean and of size n_x and 3*n_x and run the t.test
  sampleX <- sampleGammaDistribution(n_x)
  sampleY <- sampleChisqDistribution(n_x*3, mean(sampleX))
  t <- t.test(sampleX,sampleY)
  return(t)
}
createSamplesAndRunTTest(n_x)

getTestSuccessRate <- function(n_x) {
  # param: n_x - sample size for smaller sample
  # returns fraction of test accepting "H0: means are equal"
  m <- 1000
  mean(replicate(m, createSamplesAndRunTTest(n_x)$p.value > 0.05))
}
getTestSuccessRate(25)
# -> for samples of size 25 and 75 the t.test detects in about xx% of the case 
#    that the means are equal

# How does the success rate of the test depend on the sample sizes?
set.seed(911911)
sampleSizes <- seq(n_x,to=100,by=5)
successRates <- sapply(sampleSizes, getTestSuccessRate)
plot(sampleSizes, successRates)
# --> apparently it doesn't ...
