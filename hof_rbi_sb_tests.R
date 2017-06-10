#### Career statistics and Hall of Fame membership for 1,780 active and retired Major 
#### League Baseball players (pitchers excluded) through 2014.
#### Data Provided By baseball-reference.com

dta <- read.csv("hof.csv")
n <- nrow(dta)

## Use bootstrap method to construct an 80% confidence interval for mean RBI among those in 
## the HOF.

## Set aside only those players in the HOF.
dta_HOF <- with(dta, dta[HOF == 1, ])

## Generate Hitogram as Visual Aid
hist(dta_HOF$RBI)
mean(dta_HOF$RBI)

## Run bootstrap
B <- 500
mean_RBI_b <- numeric(B)
for(b in 1:B) {
  y <- with(dta_HOF, sample(RBI, replace = TRUE))
  mean_RBI_b[b] <- mean(y)
}

## Get histogram and calculate CI and print
hist(mean_RBI_b)
ci_boot <- quantile(mean_RBI_b, c(0.1, 0.9))

## Use the bootstrap to test H_0: mean RBI = 1400 vs. H_a: mean RBI < 1400, among players 
## in the HOF.

## Transform to force H_0 to be true.
RBI_0 <- with(dta_HOF, mean(RBI))
RBI_trans <- with(dta_HOF, RBI - mean(RBI) + 1400)


## Run Bootstrap with RBI_trans to calculate p
B <- 500
mean_RBI_b <- numeric(B)
for(b in 1:B) {
  y <- sample(RBI_trans, replace = TRUE)
  mean_RBI_b[b] <- mean(y)
}
p_value_boot <- mean(mean_RBI_b < RBI_0)

## Among those players who are currently eligible to be in the HOF, I am going
## to see if the standard deviation for the number of stolen bases (SB) for 
## those in the HOF is twice (or more) that for those who are not in the HOF.

## Set aside only the players currently eligible to be in the HOF.
dta_elig <- with(dta, dta[(SP >= 10) & ((2014 - LY) >= 5), ])

## Hypothesis test at significance level 0.05
## Force H_0 to be true and then run bootstrap

sd_ratio_0 <- with(dta_elig, sqrt(var(SB[HOF == 1])) / sqrt(var(SB[HOF == 0])))
dta_elig_scaled <- dta_elig
dta_elig_scaled$SB[dta_elig_scaled$HOF == 1] <- 
  with(dta_elig, SB[HOF == 1] / sqrt(var(SB[HOF == 1]))) * 2
dta_elig_scaled$SB[dta_elig_scaled$HOF == 0] <- 
  with(dta_elig, SB[HOF == 0] / sqrt(var(SB[HOF == 0])))

B <- 500
sd_ratio_b <- numeric(B)
for(b in 1:B) {
  y_hof_0 <- with(dta_elig_scaled, sample(SB[HOF == 0], replace = TRUE))
  y_hof_1 <- with(dta_elig_scaled, sample(SB[HOF == 1], replace = TRUE))
  sd_ratio_b[b] <- sqrt(var(y_hof_1)) / sqrt(var(y_hof_0))
}
hist(sd_ratio_b)
p_value <- sum(sd_ratio_b >= sd_ratio_0) / B
