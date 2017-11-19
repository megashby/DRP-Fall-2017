for (i in seq(from=0, to=1, by = 0.01)){
	print(dbeta(x = i, shape1 = 5, shape2 = 3))
	print(22*dbeta(x = i, shape1 = 6, shape2 = 4))
	print('')
} 

#xtemp = dbeta(x = .68, shape1 = 5, shape2 = 3)

#xtemp

#Since beta dist. seems to be <3 an M of 3 seems appropriate


keep <- c()
M = 3
for (n in 1:1000){
	currentu <- runif(1, 0, 1)
	currentx <- runif(1, 0, 1) 
	currentgx <- dbeta(x = currentx, shape1 = 5, shape2 = 3)
	currentMhx <- 3*dunif(currentx, min = 0, max = 1) 
	if (currentu < (currentgx/currentMhx)){
		keep <- c(keep, currentx)
	}
}
#currentu
#currentx
hist(keep,main = 'rejection sampling beta(5,3)?', xlab = '', labels = TRUE)
length(keep)

keep2 <-c()
M = 22
for (n2 in 1:100000){
	currentu <- runif(1, 0, 1)
	currentx <- rbeta(1, shape1 = 6, shape2 = 4)
	currentgx <- dbeta(x = currentx, shape1 = 5, shape2 = 3)
	currentMhx <-22*dbeta(x = currentx, shape1 = 6, shape2 = 4)
	if (currentu < (currentgx/currentMhx)){
		keep2 <- c(keep2, currentx)
	}
}
hist(keep2,main = 'rejection sampling part 2', xlab = '', labels = TRUE)
length(keep2)

