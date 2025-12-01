######Zoo800 Homework 13 - Maddy Teter - November 25, 2025

#Objective 1
dragon_data <- read.csv("dragon_data.csv")

#visualize data
plot(dragon_data)

#estimate parameters of linear regression using analytical solution equation: 
#b <- solve(t(X) %*% X) %*% (t(X) %*% y)

X <- cbind(1, dragon_data$size) #combine columns with intercept (1) for each dragon size row
y <- dragon_data$acres_on_fire #response variable

b <- solve(t(X) %*% X) %*% (t(X) %*% y) #analytical solution equation
b #view results
#intercept = -1.38, slope = 1.35

lm(acres_on_fire ~ size, data = dragon_data) #double check values by using linear model - they are the same

#Objective 2: Estimate the parameters of the linear regression of area burned on dragon size using 
#OLS using grid search and optim()

#part a
x <- dragon_data$size
y <- dragon_data$acres_on_fire

intercepts <- seq(-5, 5, by = 0.01) #intercepts from -5 to 5 by 0.01 intervals
slopes <- seq(-5, 5, by = 0.01) #slopes from -5 to 5 by 0.01 intervals

dragon_grid <- matrix(NA, nrow = length(intercepts), ncol = length(slopes)) #create empty matrix to 
#store values

#run for loop to see all combinations of 
for(i in seq_along(intercepts)) {
  for (j in seq_along(slopes)) {
    prediction <- intercepts[i] + slopes[j] * x
    dragon_grid[i, j] <- sum((y - prediction)^2)
  }
}

#identify the minimum sse from dragon grid values
minimum_error <- which(dragon_grid == min(dragon_grid), arr.ind = TRUE)

grid_intercept <- intercepts[minimum_error[1]]
grid_slope <- slopes[minimum_error[2]]

grid_intercept #-1.49 - slightly off from lm (-1.38), but close
grid_slope #1.35

#part b - use optim function to find ideal parameters

#create function to find sse
dragon_sse <- function(parameters) {
  b0 <- parameters[1]
  b1 <- parameters[2]
  sum((y - (b0 + b1 * x))^2)
}

#use optim function 
dragon_sse_results <- optim(par = c(0, 0), fn = dragon_sse)

dragon_sse_results$par #parameters = -1.37, 1.35
dragon_sse_results$convergence #convergence = 0


#part c -check convergence when using different starting points

starting_values <- list(
  c(0, 0),
  c(80, -10),
  c(-50, 20),
  c(15, 15),
  c(-15, -15)
)

convergence_results <- lapply(starting_values, function(s) {
  optim(par = s, fn = dragon_sse)
})

convergence_results #convergence = 0 even with different starting numbers, intercept and slope 
#values are -1.42, 1.35 (very close to other values)


#Objective 3 - Repeat objective 2 but use maximum likelihood 
#part a

#Use same parameters from before for grid search (already loaded in environment)
intercepts <- seq(-5, 5, by = 0.01) #intercepts from -5 to 5 by 0.01 intervals
slopes <- seq(-5, 5, by = 0.01) #slopes from -5 to 5 by 0.01 intervals

n <- length(y)

dragon_grid_nll <- matrix(NA, length(intercepts), length(slopes)) #empty matrix to store values

#run for loop to see all combinations of b0 and b1 in grid
for (i in seq_along(intercepts)) {
  for (j in seq_along(slopes)) {
    pred <- intercepts[i] + slopes[j] * x
    sigma2 <- mean((y - pred)^2)
    dragon_grid_nll[i, j] <- (n/2)*log(2*pi*sigma2) + (n/2)
  }
} 

#identify minimum nll values from grid 
nll_minimum <- which(dragon_grid_nll == min(dragon_grid_nll), arr.ind = TRUE)

#pull minimum intercept and slope values
mle_grid_intercept <- intercepts[nll_minimum[1]]
mle_grid_slope     <- slopes[nll_minimum[2]]

mle_grid_intercept #-1.49
mle_grid_slope # 1.35

#part b
mle_nll <- function(par) {
  b0 <- par[1]
  b1 <- par[2]
  sigma <- exp(par[3]) #makes sigma positive only
  mu <- b0 + b1 * x
  -sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
}

#Run lme_nll function with optim 
mle_optim <- optim(par = c(0, 0, log(sd(y))), 
                fn = mle_nll, 
                method = "BFGS")
mle_optim$par #parameters = -1.38, 1.35, 1.52

#part c

#Check convergence with different starting values - need to include sigma in starting values for MLE
starts_mle <- list(
  c(0, 0, log(1)),
  c(80, -10, log(1)),
  c(-50, 20, log(2)),
  c(15, 15, log(3)),
  c(-15, -15, log(4))
)

multi_fits <- lapply(starts_mle, optim, fn = mle_nll, method = "BFGS")

multi_fits #parameters = -1.39, 1.35, 1.5, convergence = 0


#Objective 4
#The intercept and slope values from each of the three approaches are very close to each other. The true
#intercept and slope values from lm() are -1.38 and 1.35 - all methods were very close to the true values
#but the grid search for the intercept was ~ -1.49 which was the furthest from the true value of all of the
#methods. 

#Plot fits from different methods together

#extract lm values
fit_lm <- lm(y ~ x)
true_b0 <- coef(fit_lm)[1]
true_b1 <- coef(fit_lm)[2]

#extract OLS values from grid
ols_grid_b0 <- grid_intercept
ols_grid_b1 <- grid_slope

#extract MLE values from grid
mle_grid_b0 <- mle_grid_intercept
mle_grid_b1 <- mle_grid_slope

#Plot raw data points
plot(x, y,
     pch = 19, cex = 1,
     xlab = "Dragon size",
     ylab = "Acres burned",
     main = "True Intercept and Slope Values (lm) vs Predicted (ols and mle")
abline(true_b0, true_b1, col = "red", lwd = 2)
abline(ols_grid_b0, ols_grid_b1, col = "green", lwd = 2)
abline(mle_grid_b0, mle_grid_b1, col = "blue", lwd = 2)
#Lines all fall in line with eachother - predicted values match true values







