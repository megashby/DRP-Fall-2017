sum = 0
finalvalue = 0
N = 100
keep <-c()
final <-c()
for (j in 1:1000){
	for (n in 1:N){
		currentx <-runif(1, 0, 1)
		currenthx <- dunif(currentx, min = 0, max = 1)
		currentgx <- dbeta(currentx, shape1 = 5, shape2 = 3)
		curxgxhx <- currentx*currentgx/currenthx
		keep <- c(keep, curxgxhx)
		sum <- sum +curxgxhx
		}

	expected <-sum/N
	expected
	final <-c(final, expected)
	sum <- 0
	finalvalue <-finalvalue+expected
	}
final
hist(final,main = 'blah', xlab = '', labels = TRUE)
finalvalue/100

sum2 = 0
N2 = 1000
keep2 <-c()
for(n in 1:N2){
	currentx <-rbeta(1, shape1 =2, shape2 = 2)
	currenthx <- dbeta(currentx, shape1 = 2, shape2 = 2)
	currentgx <- dbeta(currentx, shape1 = 5, shape2 = 3)
	curxgxhx <- currentx*currentgx/currenthx
	keep2 <-c(keep2, curxgxhx)
	sum2 <-sum2 + curxgxhx
	}
hist(keep2,main = 'blah2', xlab = '', labels = TRUE)

expected2 <-sum2/N2
expected2

sum3 = 0
N3 = 1000
keep3 <-c()
other3 <-c()
for(n in 1:N3){
	currentx <-rbeta(1, shape1 =6, shape2 = 3.5)
	currenthx <- dbeta(currentx, shape1 = 6, shape2 = 3.5)
	currentgx <- dbeta(currentx, shape1 = 5, shape2 = 3)
	curxgxhx <- currentx*currentgx/currenthx
	other3 <-c(other3, currentgx/currenthx)
	keep3 <-c(keep3, curxgxhx)
	sum3 <-sum3 + curxgxhx
	}
hist(keep3, main = 'blah3', xlab = '', labels = TRUE)
hist(other3, main = "other3", labels = 
expected3 <-sum3/N3
expected3

#what is going on with this???
sum4 = 0
N4 = 1000
keep4 <-c()
for(n in 1:N4){
	currentx <-rbeta(1, shape1 =6, shape2 = 3)
	currenthx <- dbeta(currentx, shape1 = 6, shape2 = 3)
	currentgx <- dbeta(currentx, shape1 = 5, shape2 = 3)
	curxgxhx <- currentx*currentgx/currenthx
	keep4 <-c(keep4, curxgxhx)
	sum4 <-sum4 + curxgxhx
	}
hist(keep4, main = 'blah4', xlab = '', labels = TRUE)
expected4 <-sum4/N4
expected4
keep4