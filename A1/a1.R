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

# Question 4 

setwd("C:/Users/2baja/OneDrive/Desktop/STAT 341/A1")
apartment_eval <- read.csv("Apartment_Building_Evaluation.csv")

# q4 a) 
score_90 <- apartment_eval[,"SCORE"] >= 90
sum_score_90 <- sum(score_90)

# q4 b)
davenport <- which(apartment_eval[,"WARDNAME"] == "Davenport")
davenport_apartments <- apartment_eval[davenport,]
davenport_apartments_sorted_addresses <- davenport_apartments[order(-davenport_apartments$SCORE),"SITE_ADDRESS"]
top_5_addresses <- davenport_apartments_sorted_addresses[c(1:5)]

# q4 c)

unique_wardnames <- unique(apartment_eval[,"WARDNAME"])
sapply(unique_wardnames, function(name) { mean(apartment_eval[which(apartment_eval$WARDNAME == name), "SCORE"]) })

# q4 d)

plot(apartment_eval$YEAR_BUILT, apartment_eval$SCORE, pch = 1, col=adjustcolor("black", alpha = 0.8), xlab="Year built", ylab="Apartment Score")

unique_years <- unique(apartment_eval[,"YEAR_BUILT"])
average_score_by_year <- sapply(unique_years, function(year_built) { mean(apartment_eval[which(apartment_eval$YEAR_BUILT == year_built), "SCORE"]) })

lines(unique_years, average_score_by_year, pch = 18, col="red", type="b")

legend(x = "bottomleft",          # Position
       legend = c("building score", "average building score per year"),  # Legend texts
       col = c("black", "red"),           # Line colors
       cex = 0.75,
       pch = c(1, 19)) 

# q4 e)

influence_values <- function(pop, attribute){
  N <- length(pop)
  attribute_total_pop <- attribute(pop)
  
  return (sapply(1:N, function(x) { abs(attribute_total_pop - attribute(pop[-x])) }))
}

mean_influence <- influence_values(apartment_eval$SCORE, mean)

plot(1:length(apartment_eval$SCORE), mean_influence, xlab = "Observation Number", ylab = "Influence")