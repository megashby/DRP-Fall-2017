temp = rnorm(1000, mean = 0, sd= 4)
mean(temp)
sd(temp)

hist(temp, main = '1000 N(0,4) Draws', freq = FALSE, xlab = '')
points(seq(-4, 4, .1), dnorm(seq(-4, 4, .1),0,4), type = 'l', col = 'red')
#need to also change the points to match the mean, standard deviation, and max/min values for x

#question 2 
#continuous uniform???
#a = 10
#b = 15
temp2 = runif(100, min = 10, max = 11)
hist(temp2, main = '100 draws of a continuous uniform on (10,11)', freq = FALSE, xlab = 'temp2')


#question 3


#redo by iterating through creating values off the normal one at a time and concat. at the end of an array


temp3 = rnorm(10000, mean = 0, sd = 1)
#temp3
cutoff = -1.5

temp3 = temp3[temp3>cutoff]

while (length(temp3) < 10000){
    temp3 <- rnorm(10500)
    temp3 <- temp3[temp3 > cutoff]}


temp3 <- temp3[1:10000]

pfrow = c(2,2)
hist(temp3, main = '10000 N(0,1) Draws with truncate at -1.5', freq = FALSE, xlab = '')
points(seq(-4, 4, .1), dnorm(seq(-4, 4, .1),0,1), type = 'l', col = 2)


#RANDOM/ATTEMPTS


#temp2 = dbinom(x, size = 1000, prob = 0.25)
#hist(temp2, main = 'my try at a binomial built-in', freq = FALSE, xlab= '')
#points(seq(-4, 4, .1), dbinom(seq(-4, 4, .1),0,1), type = 'l', col = 2)

temp3 = dpois(0:5, 8)
hist(temp3, main = 'A poisson???', freq = FALSE, xlab = '')

numcases = 20
min = 1
max = 6
x = as.integer(runif(numcases, min, max +1))
par(mfrow = c(2, 1))
hist(x, main = paste(numcases, "roles of a dice"), breaks = seq(min-.5, max+.5, 1))

