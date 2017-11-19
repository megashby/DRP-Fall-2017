xtemp = rbinom(n = 100, size = 5, p =0.2)
hist(xtemp, main = '100 binomial draws with n=5, p = 0.2', xlab = '', labels = TRUE)

n = 100

xtemp2 = rbinom(n , size = 10, p =0.1)
#xtemp2
#can set breaks = very high number to make them really skinny
hist(xtemp2, main = '100 binomial draws with n= 100, p = 0.1', xlab = '', labels = TRUE)


#can also replace the xtemp2 with a sequence i.e. from 0 to 6 on intervals of
#.1 to get graphs like Ciara's
plot(xtemp2, pbinom(xtemp2, size = 10, p = 0.1))


zeros = subset(xtemp2, xtemp2 == 0)
zeros.length = length(zeros)
zeros.percent = zeros.length/n
#zeros.length

ones = subset(xtemp2, xtemp2 == 1)
ones.length = length(ones)
ones.percent = ones.length/n

twos = subset(xtemp2, xtemp2 == 2)
twos.length = length(twos)
twos.percent = twos.length/n

threes = subset(xtemp2, xtemp2 == 3)
threes.length = length(threes)
threes.percent = threes.length/n

fours = subset(xtemp2, xtemp2 == 4)
fours.length = length(fours)
fours.percent = fours.length/n

fives = subset(xtemp2, xtemp2 == 5)
fives.length = length(fives)
fives.percent = fives.length/n

percents = c(zeros.percent, ones.percent, twos.percent, threes.percent, fours.percent, fives.percent)

#sum(percents)

zeroscdf.length = zeros.length
onescdf.length = zeroscdf.length + ones.length
twoscdf.length = onescdf.length+twos.length
threescdf.length = twoscdf.length + threes.length
fourscdf.length = threescdf.length + fours.length
fivescdf.length = fourscdf.length + fives.length

#zeroscdf.length
#onescdf.length
#twoscdf.length
#threescdf.length
#fourscdf.length
#fivescdf.length

cdfvec = NULL

#trying to remember how Ciara did it and failing miserably
for (i in 1:zeroscdf.length)
{
	cdfvec <- c(cdfvec, 0)
}
#length(cdfvec)
#zeroscdf.length

for (i in 1:onescdf.length)
{
	cdfvec <- c(cdfvec, 1)
}
for (i in 1:twoscdf.length)
{
	cdfvec <- c(cdfvec, 2)
}
for (i in 1:threescdf.length)
{
	cdfvec <- c(cdfvec, 3)
}
for (i in 1:fourscdf.length)
{
	cdfvec <- c(cdfvec, 4)
}
for (i in 1:fivescdf.length)
{
	cdfvec <- c(cdfvec, 5)
}
cdfvec

hist(cdfvec, main = 'cdf???', xlab = '', labels = TRUE)
#problems: technically this shows frequencies (this is because of how I thought
#of solving the problem), also is showing values of 0 b/c this is discrete so 
#there are no points at those x-values


#this works
#cdfvec 2 = NULL
#cdfvec2 = c(cdfvec2, rep(0, zeroscdf.length))
#cdfvec2


xtemp3 = rpois(n = 1000, size =  lambda = 5)
#xtemp3
hist(xtemp3, main = '1000 binomial draws with lambda = 5', xlab = '', labels = TRUE)

xtemp4 = rpois(n = 100, lambda = 2)
xtemp4
hist(xtemp4, main = '100 binomial draws with lambda = 2', xlab = '', labels = TRUE)
#help(rpois)

#xtemp4

zeroesp = subset(xtemp4, xtemp4 == 0)
zeroesp.length = length(zeroesp)
zeroesp.percent = zeroesp.length/100

onesp = subset(xtemp4, xtemp4 == 1)
onesp.length = length(onesp)
onesp.percent = onesp.length/100

twosp = subset(xtemp4, xtemp4 == 2)
twosp.length = length(twosp)
twosp.percent = twosp.length/100

threesp = subset(xtemp4, xtemp4 == 3)
threesp.length = length(threesp)
threesp.percent = threesp.length/100

#help(ppois)

plot(xtemp4, ppois(xtemp4, 2))


xtemp5 = runif(100, 0 , 1)
xtemp5

myfunc = function(x) {
	a <- 0
	ifelse(x <= .1, a <- 1, ifelse(x<=.35, a <- 2, ifelse(x<=.5, a <- 3, ifelse(x<=
	.95, a <- 4, ifelse(x <=1, a <- 5,a <- NA)))))
	#return(a)
}
ytemp5 <- myfunc(xtemp5)
ytemp5
hist(ytemp5)



