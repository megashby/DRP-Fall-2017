#truncated normals

lowerbound <- -1.5
keep <-c()
for (i in 1:100){
	temp <-rnorm(1)
	if (temp > lowerbound){
		keep<- c(keep, temp)
	}
}


hist(keep, main = '100 N(0,1) Draws with lowerbound at -1.5', freq = FALSE, xlab = 'x', ylab = 'Density')
points(seq(-4, 4, .1), dnorm(seq(-4, 4, .1),0,1), type = 'l', col = "blue")

upperbound <-2
keep <-c()
for (i in 1:1000){
	temp<-rnorm(1)
	if (temp < upperbound){
		keep <- c(keep, temp)
	}
}
hist(keep, main = '1000 N(0,1) Draws with upperbound at 2', freq = FALSE, xlab = 'x', ylab = 'Density')
points(seq(-4, 4, .1), dnorm(seq(-4, 4, .1),0,1), type = 'l', col = "blue")



#rejection sampling of beta(5,3)

x0 <- seq(0, 1, 0.01)
y0 <- dbeta(x0, shape1 = 5, shape2 = 3)
uniform3 <- rep(3, length(x0))
	
plot(x0, y0,'l', main = 'Beta (5,3) distribution', ylim = c(0, 3.5))
lines(x0, uniform3, col = 'blue')

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

hist(keep,main = 'rejection sampling beta(5,3) with uniform(0,1)', xlab = '', labels = TRUE)
length(keep)

uniform25 <- rep(2.5, length(x0))
	
plot(x0, y0,'l', main = 'Beta (5,3) distribution', ylim = c(0, 3.5))
lines(x0, uniform25, col = 'blue')

keep <- c()
M = 2.5
for (n in 1:1000){

	currentu <- runif(1, 0, 1)
	currentx <- runif(1, 0, 1) 
	currentgx <- dbeta(x = currentx, shape1 = 5, shape2 = 3)
	currentMhx <- 2.5*dunif(currentx, min = 0, max = 1) 
	if (currentu < (currentgx/currentMhx)){
		keep <- c(keep, currentx)
	}
}

hist(keep,main = 'rejection sampling beta(5,3) with uniform(0,1)', xlab = '', labels = TRUE)
length(keep)


example <-function(x){
	5*(x-.6)^3 -2*x +2
}

plot(x0, example(x0),'l', main = '5(x-.6)^3 -2x+2', ylim = c(0, 3.5), xlim = c(0,1))
uniform15 <- rep(1.5, length(x0))
lines(x0, uniform15, col = 'blue')

keep <- c()
for (n in 1:1000){

	currentu <- runif(1, 0, 1)
	currentx <- runif(1, 0, 1) 
	currentgx <- example(currentx)
	currentMhx <- 1.5*dunif(currentx, min = 0, max = 1) 
	if (currentu < (currentgx/currentMhx)){
		keep <- c(keep, currentx)
	}
}
hist(keep,main = 'rejection sampling polynomial', xlab = '', labels = TRUE)
length(keep)

#envelope
plot.new()
plot(1, type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 2), main = '-6x^2 + 6x')
x0 <-seq(0.01, 1, .01)
y0 <- -6*(x0)^2 + 6*(x0)
lines(x0, y0, col = 'blue')

f<-function(x){
	return (-6*x^2 + 6*x)
}

deriv <-function(x){
	return(-12*x + 6)
}

tangent <-function(x1, slope, x){
	return(slope*x - slope*x1 + f(x1))	
}

tangentdiff <-function(x1, slope1, x2, slope2 ,x){
	return(slope1*x - slope1*x1 + f(x1) -(slope2*x - slope2*x2 + f(x2)))
}

temp<-seq(0,1,.1)

mat <-matrix(, nrow = 11, ncol = 7)
for (n in 1:11){
	mat[n,1] <-temp[n]
	mat[n,2] <- f(temp[n])
	mat[n,3] <- deriv(temp[n])
}

for (n in 1:11){
	lines(x0, tangent(mat[n,1], mat[n,3], x0), col = 'red')
}

mat[1,4] <- 0
mat[1,5] <-0

newarray <- tangentdiff(mat[1,1], mat[1,3], mat[2,1], mat[2, 3], x0)

keep <- 0
for (i in 1:length(newarray)){
	if (newarray[i] == 0){
		keep <- i
	}
}

for (n in 2:11){
	newarray <-tangentdiff(mat[n-1, 1], mat[n-1,3], mat[n,1], mat[n,3], x0)
	keep <- 0
	for (i in 1:length(newarray)){
		if (abs(newarray[i]- 0)< 1e-8){
			keep <-i
			xvalue <- keep*0.01
		}
	}
	mat[n,4] <-xvalue
	mat[n,5] <- tangent(mat[n,1], mat[n,3], mat[n,4])
	points(mat[n,4], mat[n,5])
}
integralval3 <-function(x2, x1, x0, slope){
	return(slope*(x2^2)/2 - slope*x0*x2 -6*(x0^2)*x2 + 6*x0*x2 - (slope*(x1^2)/2 - slope*x0*x1 -6*(x0^2)*x1 + 6*x0*x1))
}


mat[1,6] = 0
mat[1,7] = 0
for (n in 2:11){
	sumbefore <-0
	for (i in n-1){
		sumbefore <- sumbefore+ mat[i,6]
	} 
	curintegral <- integralval3(mat[n,4], mat[n-1,4], mat[n-1,1], mat[n-1,3])
	mat[n,6] <- curintegral
	mat[n,7] <- curintegral + mat[n-1,7]
	
}


lastsection <-integralval3(1, .95, 1, -6)
lastsection

totalarea <- 1.005

envelope <-function(x){
 a <- 0
	ifelse(x <= .05, a <- tangent(0, 6, x), ifelse (x<=.15, a <- tangent(.1,4.8,
	x), ifelse ( x<= .25, a<-tangent(.2, 3.6, x), ifelse(x<=.35, a<-tangent(.3, 2.4,
	x), ifelse(x<=.45, a<-tangent(.4, 1.2, x), ifelse(x<=.55, a<-tangent(.5, 0,x),
	ifelse(x<=.65, a<- tangent(.6, -1.2, x), ifelse(x<=.75, a<-tangent(.7, -2.4,x),
	ifelse(x<=.85, a<-tangent(.8, -3.6, x), ifelse(x<=.95, a<-tangent(.9, -4.8,x),
	a<- tangent(1, -6,x)))))))))))
	
}

envelopey <-envelope(x0)

points(x0, envelopey, col = 'blue')

cdf <-function(x){
 a <- 0
	ifelse(x <= .05, a <- integralval3(x,0, 0,6), ifelse (x<=.15, a <- integralval3(x,.05,.1,4.8)
	+.0075, ifelse ( x<= .25, a<-integralval3(x, .15,.2, 3.6)+0.0615, ifelse(x<=.35, a<-integralval3(x, .25, .3, 2.4
	)+.1575, ifelse(x<=.45, a<-integralval3(x,.35,.4, 1.2)+.2835, ifelse(x<=.55, a<-integralval3(x,.45,.5, 0)
	+.4275,ifelse(x<=.65, a<- integralval3(x, .55, .6, -1.2)+.5775, ifelse(x<=.75, a<-
	integralval3(x, .65,.7, -2.4)+.7215, ifelse(x<=.85, a<-integralval3(x, .75, .8, -3.6)+.8475,
	 ifelse(x<=.95, a<-integralval3(x, .85,.9, -4.8)+.9435,
	a<- integralval3(x, .95, 1, -6) + .9975))))))))))
}
plot.new()
plot(1, type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 1), main = 'CDF of Envelope')

points (x0, cdf(x0), col = 'blue')

cdfnormal <-function(x){
 a <- 0
	ifelse(x <= .05, a <- integralval3(x,0, 0,6)/1.005, ifelse (x<=.15, a <- (integralval3(x,.05,.1,4.8)
	+.0075)/1.005, ifelse ( x<= .25, a<-(integralval3(x, .15,.2, 3.6)+0.0615)/1.005, ifelse(x<=.35, a<-(integralval3(x, .25, .3, 2.4
	)+.1575)/1.005, ifelse(x<=.45, a<-(integralval3(x,.35,.4, 1.2)+.2835)/1.005, ifelse(x<=.55, a<-(integralval3(x,.45,.5, 0)
	+.4275)/1.005,ifelse(x<=.65, a<- (integralval3(x, .55, .6, -1.2)+.5775)/1.005, ifelse(x<=.75, a<-
	(integralval3(x, .65,.7, -2.4)+.7215)/1.005, ifelse(x<=.85, a<-(integralval3(x, .75, .8, -3.6)+.8475)/1.005,
	 ifelse(x<=.95, a<-(integralval3(x, .85,.9, -4.8)+.9435)/1.005,
	a<- (integralval3(x, .95, 1, -6) + .9975))/1.005)))))))))
}
plot.new()
plot(1, type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 1), main = 'Normalized CDF of Envelope')

points (x0, cdfnormal(x0))

plot.new()
plot(1, type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 1.5), main = '-6x^2 + 6x')
x0 <-seq(0.01, 1, .01)
y0 <- -6*(x0)^2 + 6*(x0)
lines(x0, y0, col = 'blue')
lines(x0, envelopey)
