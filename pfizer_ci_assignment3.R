#####################
### STAT 305 HW 3 ###
#####################

## Perform a simplified analysis of the
## Pfizer-Biontech vaccine trial results
## for data, see: 
## https://www.pfizer.com/news/press-release/press-release-detail/pfizer-and-biontech-conclude-phase-3-study-covid-19-vaccine

### Data ------
# total COVID cases
n <- 170
# COVID cases in vaccine arm
y <- 8


### Estimate prob vaccine arm (conditional on COVID cases) - normal approx ------

pi.hat <-y/n # (estimate of pi)
pi.hat

se.pi.hat <- sqrt((pi.hat)*(1-pi.hat)/n) #  (se of pi.hat)
se.pi.hat

# calculate 95% confidence interval based on normal approximation
z <- qnorm(0.975, mean = 0, sd = 1) # (normal quantile to be used in 95% CI)
CI <-c(pi.hat-z*se.pi.hat,pi.hat+z*se.pi.hat)##  (95% CI)
print(CI)

# 
## Simulate sampling distribution (i.e., repeated experiments) of pi estimator ------
n.simul <- 1000
set.seed(3527) # THE LAST FOUR DIGITS OF STUDENT NUMBER

y.simul <- rbinom(n.simul, size = n, prob = pi.hat)
pi.hat.simul <- y.simul / n

# Report empirical mean and sd as estimates of properties of estimators
mean(pi.hat.simul)
sd(pi.hat.simul)
# Agree with theoretical properties of binomial?

# Calculate simulated empirical confidence interval
ci.sim <- quantile(pi.hat.simul, c(0.025, 0.975))
ci.sim

# plot for comparison
xx <- seq(from = 0.0, to = 0.15, by = 0.001)
hist(pi.hat.simul, freq = FALSE)
abline(v = pi.hat, lwd = 2.5) # this is a vertical line at pi.hat
abline(v = ci.sim) # vertical lines at simulated CI limits
lines(xx, dnorm(xx, mean = pi.hat, sd = se.pi.hat), col = "blue") # normal density
abline(v = CI, col = "blue") # blue vertical lines at normal CI limits


### In the study, these proportions were converted to vaccine effectiveness ----

# Convert pi.hat to vaccine effectiveness
rr.hat <- pi.hat / (1 - pi.hat)
ve.hat <- 100 * (1 - rr.hat)

# Convert normal CI limits to vaccine effectiveness
# Rate ratio
rr <- CI / (1 - CI)
# Vaccine effectiveness
ve <- rev(100 * (1 - rr))
print(ve)

# Convert simulation CI limits to vaccine effectiveness
rr.sim <- ci.sim / (1 - ci.sim)
ve.sim <- rev(100 * (1 - rr.sim))
print(ve.sim)
# plot for comparison
ve.hat.sim <- 100 * (1 - (pi.hat.simul / (1 - pi.hat.simul)))
ci.ve.hat.sim <- quantile(ve.hat.sim, c(0.025, 0.975))
hist(ve.hat.sim, freq = FALSE)
abline(v = mean(ve.hat.sim), lwd = 2.5) # this is a vertical line at pi.hat
abline(v = ve.sim) # vertical lines at simulated CI limits
abline(v = ve, col = "blue") # blue vertical lines at normal CI limits
abline(v = ci.ve.hat.sim, col = "red")

