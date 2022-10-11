setwd("C:/Users/2baja/OneDrive/Desktop/STAT 341/A2")
full_nba_player_boxscore <- read.csv("NBA_Player_Boxscore_2021-22.csv")

#1 a)

# hoopR - Access Men's Basketball Play by Play Data
# wehoop - Access Women's Basketball Play by Play Data

#1 b) 

#https://hoopr.sportsdataverse.org/reference/load_nba_player_box.html

#1 c) 

team_frequency_table <- table(full_nba_player_boxscore$team_abbreviation)

par(mar=c(5.1,4.1,4.1,2.1))
hist(team_frequency_table)

# most teams are greater than 800 
# mean(team_frequency_table) = 868.8125
# median(team_frequency_table) = 895.5
# outlier - Team Durant and Team LeBron which are 12 and 11 respectively

#1 d)

nba_player_boxscore <- subset(full_nba_player_boxscore, team_abbreviation != "LEB" & team_abbreviation != "DUR")

#2 a)
par(mfrow=c(1,3))

boxplot(nba_player_boxscore$reb)
hist(nba_player_boxscore$reb)

# have to manually build a quantile plot

y <- sort(nba_player_boxscore$reb)
x <- (1:length(y)) / length(y)

plot(x, y)

#2 b) Yes. The box plot shows the right skew, the location of the median, and the outliers / spread in the population

#2 c) mean(nba_player_boxscore[which(nba_player_boxscore$athlete_position_name == "Small Forward"), "reb"])
by(nba_player_boxscore$reb, nba_player_boxscore$athlete_position_name, mean)

#2 d) less skewness compared to the individual box plots
par(mfrow=c(1,7))
by(nba_player_boxscore$reb, nba_player_boxscore$athlete_position_name, boxplot)

#2 e) Yes. Because the represent the outliers, skewness, and central location

#2 f) 
par(mfrow=c(1,7))
by(jitter(nba_player_boxscore$reb), nba_player_boxscore$athlete_position_name, boxplot)

#2 g)
powerfun <- function(y, alpha) {
  if(sum(y <= 0) > 0) stop("y must be positive")
  if (alpha == 0)
    log(y)
  else if (alpha > 0) {
    y^alpha
  } else -(y^alpha)
}

to_be_optimized <- function(alpha) {
  return(abs(skewness(powerfun(nba_player_boxscore$reb + 1, alpha))))
}

nlminb(start=1, objective=to_be_optimized)
hist(powerfun(nba_player_boxscore$reb + 1, 0.24446))
hist(powerfun(nba_player_boxscore$reb + 1, 0.2))
hist(powerfun(nba_player_boxscore$reb + 1, 0))

#3 a)
raptors = subset(nba_player_boxscore, team_abbreviation %in% c("TOR"))
warriors = subset(nba_player_boxscore, team_abbreviation %in% c("GS"))

par(mar=c(5.1,4.1,4.1,2.1))
plot(raptors$reb, raptors$pts, pch = 1, col=adjustcolor("red", alpha = 0.8))
points(warriors$reb, warriors$pts, pch = 1, col=adjustcolor("blue", alpha = 0.8))

warriors_regression = lm(warriors$pts ~ warriors$reb)
raptors_regression = lm(raptors$pts ~ raptors$reb)

abline(raptors_regression, col = "red")
abline(warriors_regression, col = "blue")

#3 b)
N = nrow(raptors)
delta = matrix(0, nrow = N, ncol = 2)
for (i in 1:N) {
  fit.no.i = lm(pts ~ reb, data = raptors[-i, ])
  delta[i, ] = (raptors_regression$coef - fit.no.i$coef)
}

par(mfrow = c(1, 2))

plot(delta[, 1], ylab = bquote(Delta[alpha]), main = bquote("Influence on" ~
                                                              alpha), pch = 19, col = adjustcolor("grey", 0.6))

plot(delta[, 2], ylab = bquote(Delta[beta]), main = bquote("Influence on" ~
                                                             beta), pch = 19, col = adjustcolor("grey", 0.6))

delta2 = apply(X = delta, MARGIN = 1, FUN = function(z) {
  sqrt(sum(z^2))
})

plot(delta2, main = bquote("Influence on" ~ theta), ylab = bquote(Delta), pch = 19,
     col = adjustcolor("grey", 0.6))

raptors[delta2 > 0.06, ]

# large influence by Gary Trent Jr. [high points / low rebounds] / Fred VanVleet [high pts / low rebounds] / Precious Achiuwa [high rebounds / low points]

#3 c)
average_influence_on_slope_per_raptor = by(delta[,2], raptors$athlete_display_name, mean)

#  which.max(average_influence_on_slope_per_raptor) -> David Johnson
#  which.min(average_influence_on_sloper_per_raptor) -> Gary Trent Jr.

#3 d)
gradientDescent <- function(theta = 0,
                            rhoFn, gradientFn, lineSearchFn, testConvergenceFn,
                            maxIterations = 100,
                            tolerance = 1E-6, relative = FALSE,
                            lambdaStepsize = 0.01, lambdaMax = 0.5 ) {
  converged <- FALSE
  i <- 0
  while (!converged & i <= maxIterations) {
    g <- gradientFn(theta) ## gradient
    glength <- sqrt(sum(g^2)) ## gradient direction
    if (glength > 0) d <- g /glength
    lambda <- lineSearchFn(theta, rhoFn, d,
                           lambdaStepsize = lambdaStepsize, lambdaMax = lambdaMax)
    thetaNew <- theta - lambda * d
    converged <- testConvergenceFn(thetaNew, theta,
                                   tolerance = tolerance,
                                   relative = relative)
    theta <- thetaNew
    i <- i + 1
  }
  ## Return last value and whether converged or not
  list(theta = theta, converged = converged, iteration = i, fnValue = rhoFn(theta))
}

createRobustHuberRho <- function(x, y, kval) {
  ## local variable
  xbar <- mean(x)
  ## Return this function
  function(theta) {
    alpha <- theta[1]
    beta <- theta[2]
    sum( huber.fn(y - alpha - beta * (x - xbar), k = kval ) )
  }
}

huber.fn <- function(r, k) {
  val = r^2/2
  subr = abs(r) > k
  val[ subr ] = k*(abs(r[subr]) - k/2)
  return(val)
}

createRobustHuberGradient <- function(x, y, kval) {
  ## local variables
  xbar <- mean(x)
  ybar <- mean(y)
  function(theta) {
    alpha <- theta[1]
    beta <- theta[2]
    ru = y - alpha - beta*(x - xbar)
    rhok = huber.fn.prime(ru, k=kval)
    -1*c( sum(rhok*1), sum(rhok*(x - xbar)) )
  }
}

huber.fn.prime <- function(r, k) {
  val = r
  subr = abs(r) > k
  val[ subr ] = k*sign(r[subr])
  return(val)
}

gridLineSearch <- function(theta, rhoFn, d,
                           lambdaStepsize = 0.01,
                           lambdaMax = 1) {
  ## grid of lambda values to search
  lambdas <- seq(from = 0, by = lambdaStepsize, to = lambdaMax)
  ## line search
  rhoVals <- sapply(lambdas, function(lambda) {rhoFn(theta - lambda * d)})
  ## Return the lambda that gave the minimum
  lambdas[which.min(rhoVals)]
}

testConvergence <- function(thetaNew, thetaOld, tolerance = 1E-10, relative=FALSE) {
  sum(abs(thetaNew - thetaOld)) < if (relative) tolerance * sum(abs(thetaOld)) else tolerance
}

objective_func <- createRobustHuberRho(x = raptors$reb, y = raptors$pts, kval = 1.345)
gradient_func <- createRobustHuberGradient(x = raptors$reb, y = raptors$pts, kval = 1.345)

result <- gradientDescent(theta = c(0,0), rhoFn = objective_func, gradientFn = gradient_func,
                          lineSearchFn = gridLineSearch, testConvergenceFn = testConvergence)

library(MASS)
temp = rlm(raptors$pts ~ I(raptors$reb - mean(raptors$reb)), psi="psi.huber")
temp$coef

#3 e
par(mfrow = c(1, 1))
plot(raptors$reb, raptors$pts)
abline(lm(raptors$pts ~ raptors$reb), col = "red")
abline(temp, col = "blue")

# huber's loss function gives a model with a greater positive intercept and a greater positive slope because 
# it is less sensitive to outliers that are high in reb and low in points 

#4a 

rho <- function(theta){
  theta_1 = theta[1]
  theta_2 = theta[2]
  
  return (1/2 * (theta_1^4 - 16 * (theta_1)^2 + 5 * (theta_1) + theta_2^4 - 16 * (theta_2)^2 + 5* (theta_2)))
}

gradient <- function(theta){
  theta_1 = theta[1]
  theta_2 = theta[2]
  
  return (c(4 * (theta_1)^3 - 32 * (theta_1) + 5, 4 * (theta_2)^3 - 32 * (theta_2) + 5))
}

#4 b)

result_1 <- gradientDescent(theta = c(0,0), rhoFn = rho, gradientFn = gradient,
                          lineSearchFn = gridLineSearch, testConvergenceFn = testConvergence)

result_2 <- gradientDescent(theta = c(3,3), rhoFn = rho, gradientFn = gradient,
                              lineSearchFn = gridLineSearch, testConvergenceFn = testConvergence)

result_3 <- gradientDescent(theta = c(-3,3), rhoFn = rho, gradientFn = gradient,
                            lineSearchFn = gridLineSearch, testConvergenceFn = testConvergence)

result_4 <- gradientDescent(theta = c(3,-3), rhoFn = rho, gradientFn = gradient,
                            lineSearchFn = gridLineSearch, testConvergenceFn = testConvergence)

result_5 <- gradientDescent(theta = c(-3,-3), rhoFn = rho, gradientFn = gradient,
                            lineSearchFn = gridLineSearch, testConvergenceFn = testConvergence)

#4 c)

rho <- function(theta_1, theta_2){
  return (1/2 * (theta_1^4 - 16 * (theta_1)^2 + 5 * (theta_1) + theta_2^4 - 16 * (theta_2)^2 + 5* (theta_2)))
}

theta1 <- seq(-5, 5, length = 500)
theta2 <- seq(-5, 5, length = 500)
Rho <- outer(theta1, theta2, "rho")
image(theta1, theta2, Rho, col = heat.colors(1000), xlab = bquote(theta[1]),
      ylab = bquote(theta[2]), main = "")
contour(theta1, theta2, Rho, add = T, levels = c(100, 40, -40, 20, 80, -20, -60))

#4 d) If starting value is close to a local minima that is not a global minima 
#     it is probable that the gradient descent algorithm will converge and get stuck there 
#     instead of reaching the global minima 

#4 e)

optim_1 <- optim(par = c(0,0), fn = rho)
optim_2 <- optim(par=c(3,3), fn = rho)
optim_3 <- optim(par=c(-3,3), fn = rho)
optim_4 <- optim(par=c(3, -3), fn = rho)
optim_5 <- optim(par=c(-3, -3), fn = rho)

# all look similar apart from at (0,0) where the gradient descent algo gives a better approximation than the generalized optim one