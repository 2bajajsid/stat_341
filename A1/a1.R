# Question 1

3^4 # 1.a) 

log(100, base = 7) # 1.b)

x <- seq(1, 100) # 1.c)
sum(sapply(x, function(x) {1/(x^2)}))

100 %% 7 # 1.d)

# 1.e) 
integrate(sin, 0, pi / 2) 
dx_steps <- 0.001
x_val <- seq(0, pi/2, by = dx_steps)
sum(sapply(x_val, function(x){ sin(x) * dx_steps }))


# 1.f) 
f <- function(x) {
  return (dexp(x, rate = 1/2))
}
integrate(f, 0, 3)

dx_steps <- 0.001
x_val <- seq(0, 3, by = dx_steps)
sum(sapply(x_val, function(x){ dexp(x, rate = 1/2) * dx_steps }))

# 1.g) 
f <- function(x) {
  return (x^2 + 3)
}
integrate(f, -2, 2)

dx_steps <- 0.0001
x_val <- seq(-2, 2, by = dx_steps)
sum(sapply(x_val, function(x){ f(x) * dx_steps }))

# Question 2

SD <- function(y) {
  return (sqrt(sum((y - mean(y))^2) / length(y)))
}

MAD <- function(y) {
  return (median(y - median(y)))
}

sc <- function(pop, y, attr){
  N <- length(pop) + 1
  return (N * (attr(c(pop, y)) - attr(pop)))
}

set.seed(341)
pop = rexp(1000)

y_val <- seq(-1, 4, by=0.1)

delta_sd <- sapply(y_val, function(y) { sc(pop, y, SD) })
delta_mad <- sapply(y_val, function(y) { sc(pop, y, MAD) })

plot(y_val, delta_sd, type="l", lwd = 2, 
     main="SC for std deviation and absolute deviation", ylab="sensitivity", xlab="y", 
     xlim=c(-1,4), ylim=c(-1, 1), col="blue")
lines(y_val, delta_mad, type="l", lwd = 2, main="Sensitivity curve for the median absolute deviation", col="red")

legend(x = "topright",          # Position
       legend = c("std deviation", "median absolution deviation"),  # Legend texts
       col = c("blue", "red"),           # Line colors
       lwd = 2)   

# Question 3

rounded.barplot <- function(x, xlab){
  table_x <- table(x)
  categories <- names(table_x)
  categories_frequencies <- as.numeric(table_x)
  
  plot.new()
  plot(NULL, type="n", xlim=c(0, 10*length(categories_frequencies)), ylim=c(0, max(categories_frequencies)), axes=FALSE, ann=FALSE)
  
  axis(2, at=seq(from=0, to=max(categories_frequencies), by=10))
  mtext(xlab, side=1, line=2)
  mtext("Frequency", side=2, line=3)
  
  x_semi <- seq(-4.5, 4.5, by=0.01)
  y_semi <- sqrt(20.25-x_semi^2)
  
  for (i in c(1: length(categories_frequencies))){
    rect(10*(i-1), 0, 10*i-1, categories_frequencies[i], col = "gray", border = "black")
    mtext(categories[i], 1, at=10*i-5, cex=0.85)
    polygon(x_semi + 4.5 + 10*(i-1), y_semi + categories_frequencies[i], col = "gray")
  }
}

plot(x, y, type="n", axes=FALSE, ann=FALSE)

set.seed(12345)
flavours = c("Mango","Papaya","Banana","Coconut","Guava","Guarana","Durian","Cashew")
candies = sample(flavours, size=300, prob=(1:8)/sum(1:8), replace=TRUE)
barplot(table(candies), xlab="Candy Flavour", ylab="Frequency")

rounded.barplot(candies, xlab="Candy Flavour")

flavours_2 <- c("Mango","Papaya","Banana")
candies_2 <- sample(flavours_2, size=50, prob=(1:length(flavours_2))/sum(1:length(flavours_2)), replace=TRUE)
rounded.barplot(candies_2, xlab="Flavours")