main <- function() {
	set.seed(0)

	lambda <- 0.2
	nosim <- 1000
	n <- 40

	data <- generate(nosim, n, lambda)

## MEAN
	mu <- 1/lambda
	X <- mean(data)

	##TABLE OF MEANS

	hist(data, 
		breaks=75,
		main= "Histogram of Exponential Distibutions",
		xlab = "Distribution"
	)
	abline(v=X, col="green")
	abline(v=mu, col="red")
	legend("topright", 
		c("Sample Mean", "Population Mean"),
		lty=c(1,1),
		col = c("green", "red")
	)
	
## VARIANCE

	sigma <- 1/lambda
	var_pop <- sigma^2/n #theoretical variance

	var_s <- var(data) #variance of means

	##TABLE OF VARIANCE

	hist(data,
		breaks=75,
		prob=TRUE,
		main="Exponential Distibutions, Population and Sample Values",
		xlab="Distribution"
	)
	lines(density(data), lwd=2, col="red")
	xfit <- seq(min(data), max(data), length = 100)
	yfit <- dnorm(xfit, mean = 1/lambda, sd = (1/lambda/sqrt(n)))	
	lines(xfit, yfit, lwd=3, col="green", lty=5)
	legend("topright", 
		c("Population Values", "Sample Values"),
		lty=c(5,1),
		col = c("green", "red")
	)

##APPROXIMATE NORMAL

	data <- generate(100000, n, lambda)

	hist(data,
		breaks=75,
		prob=TRUE,
		main="Exponential Distibutions, Population and Sample Values",
		xlab="Distribution"
	)
	lines(density(data), lwd=2, col="red")
	xfit <- seq(min(data), max(data), length = 100)
	yfit <- dnorm(xfit, mean = 1/lambda, sd = (1/lambda/sqrt(n)))	
	lines(xfit, yfit, lwd=3, col="green", lty=5)
	legend("topright", 
		c("Population Values", "Sample Values"),
		lty=c(5,1),
		col = c("green", "red")
	)
}

generate <- function(iter, avgs, lambda) {
	mns = NULL
	for(i in 1:iter) {mns = c(mns, mean(rexp(avgs, lambda)))}
	mns
}

part2 <- function() {
	data("ToothGrowth")
	str(ToothGrowth)
	summary(ToothGrowth)

	boxplot(len ~ supp*dose, data=ToothGrowth)
	##t.test(len~supp, data=ToothGrowth)

	##dose_low <- subset(ToothGrowth, ToothGrowth$dose == 0.5)
	##dose_mid <- subset(ToothGrowth, ToothGrowth$dose == 1)
	##dose_high <- subset(ToothGrowth, ToothGrowth$dose == 2)

	##t.test(dose_low$len, dose_low$dose)

	##dose_low_oj <- subset(dose_low, dose_low$supp == "OJ")
	##dose_low_vc <- subset(dose_low, dose_low$supp == "VC")

	##dose_mid_oj <- subset(dose_mid, dose_mid$supp == "OJ")
	##dose_mid_vc <- subset(dose_mid, dose_mid$supp == "VC")

	##dose_high_oj <- subset(dose_high, dose_high$supp == "OJ")
	##dose_high_vc <- subset(dose_high, dose_high$supp == "VC")
	
	##t.test(dose_low_oj$len, dose_low_vc$len)
	##t.test(dose_mid_oj$len, dose_mid_vc$len)
	##t.test(dose_high_oj$len, dose_high_vc$len)
}