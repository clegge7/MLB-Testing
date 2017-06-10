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
ci_boot

## Now going to cocmpare BA of 2B, SS, and C to all other positions
## POS is used to signify if they are those positions or not

hist(dta_HOF$AVG)
med_diff_0 <- with(dta_HOF, median(AVG[POS == 1]) - median(AVG[POS == 0]))
med_diff_0


## 95% confidence interval for median difference in batting average. 

## Run Bootstrap
B <- 500
med_diff_b <- numeric(B)
for(b in 1:B) {
  y_pos_0 <- with(dta_HOF, sample(AVG[POS == 0], replace = TRUE))
  y_pos_1 <- with(dta_HOF, sample(AVG[POS == 1], replace = TRUE))
  med_diff_b[b] <- median(y_pos_1) - median(y_pos_0)
}

## Get histogram and calculate CI and print
## Calculates difference in BA
hist(med_diff_b)
ci_boot95 <- quantile(med_diff_b, c(0.025, 0.975))
ci_boot95
