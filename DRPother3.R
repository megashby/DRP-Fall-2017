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

mat <-matrix(, nrow = 11, ncol = 8)


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
mat[1,8] = 0

for (n in 2:11){
	sumbefore <-0
	for (i in n-1){
		sumbefore <- sumbefore+ mat[i,6]
	} 
	curintegral <- integralval3(mat[n,4], mat[n-1,4], mat[n-1,1], mat[n-1,3])
	mat[n,6] <- curintegral
	mat[n,7] <- curintegral + mat[n-1,7]
	mat[n,8] <-mat[n,7]/1.005
	
	
}


lastsection <-integralval3(1, .95, 1, -6)
lastsection/1.005 + mat[11,8]
lastsection + .9975

totalarea <- 1.005


envelope <-function(x){
 a <- 0
	ifelse(x <= .05, a <- tangent(0, 6, x), ifelse (x<=.15, a <- tangent(.1,4.8,
	x), ifelse ( x<= .25, a<-tangent(.2, 3.6, x), ifelse(x<=.35, a<-tangent(.3, 2.4,
	x), ifelse(x<=.45, a<-tangent(.4, 1.2, x), ifelse(x<=.55, a<-tangent(.5, 0,x),
	ifelse(x<=.65, a<- tangent(.6, -1.2, x), ifelse(x<=.75, a<-tangent(.7, -2.4,x),
	ifelse(x<=.85, a<-tangent(.8, -3.6, x), ifelse(x<=.95, a<-tangent(.9, -4.8,x),
	a<- tangent(1, -6,x)))))))))))
	return(a)
	
}

envelopey <-envelope(x0)

points(x0, envelopey)

cdf <-function(x){
 a <- 0
	ifelse(x <= .05, a <- integralval3(x,0, 0,6) + mat[1,8], ifelse (x<=.15, a <- integralval3(x,.05,.1,4.8)
	+.mat[2,8], ifelse ( x<= .25, a<-integralval3(x, .15,.2, 3.6)+mat[3,8], ifelse(x<=.35, a<-integralval3(x, .25, .3, 2.4
	)+mat[4,8], ifelse(x<=.45, a<-integralval3(x,.35,.4, 1.2)+mat[5,8], ifelse(x<=.55, a<-integralval3(x,.45,.5, 0)
	+mat[6,8],ifelse(x<=.65, a<- integralval3(x, .55, .6, -1.2)+mat[7,8], ifelse(x<=.75, a<-
	integralval3(x, .65,.7, -2.4)+mat[8,8], ifelse(x<=.85, a<-integralval3(x, .75, .8, -3.6)+mat[9,8],
	 ifelse(x<=.95, a<-integralval3(x, .85,.9, -4.8)+mat[10,8],
	a<- integralval3(x, .95, 1, -6) + mat[11,8]))))))))))
	return(a)
}
#plot.new()
#plot(1, type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 1), main = 'CDF of Envelope')


quadform<-function(a,b,c){
value1 <-(-b + sqrt(b*b -4*(a*c))) / (2*a)
value2 <-(-b - sqrt(b*b -4*(a*c))) / (2*a)
return (c(value1, value2))
#return(value1)
}

plotrealx <-function(x){
	a<-0
	ifelse(x<=0.05, a<-quadform(mat[1,3], (mat[1,2]- mat[1,1]*mat[1,3]), -x), 
	ifelse(x<=0.15,
 	a<-quadform(mat[2,3], (mat[2,2]- mat[2,1]*mat[2,3]), -x),
	ifelse(x<= .25, a<-quadform(mat[3,3], (mat[3,2]- mat[3,1]*mat[3,3]), -x),
 	ifelse(x<=.35, a<-quadform(mat[4,3], (mat[4,2]- mat[4,1]*mat[4,3]), -x), 
	ifelse(x<=.45, a<-quadform(mat[5,3], (mat[5,2]- mat[5,1]*mat[5,3]), -x),
	ifelse(x<=.55, a<-runif(1,.45,.55),
	ifelse(x<=.65, a<- quadform(mat[7,3], (mat[7,2]- mat[7,1]*mat[7,3]), -x),
	ifelse(x<=.75, a<-quadform(mat[8,3], (mat[8,2]- mat[8,1]*mat[8,3]), -x), 
	ifelse(x<=.85, a<-quadform(mat[9,3], (mat[9,2]- mat[9,1]*mat[9,3]), -x),
	ifelse(x<=.95, a<-quadform(mat[10,3], (mat[10,2]- mat[10,1]*mat[10,3]), -x),
	a<- quadform(mat[11,3], (mat[11,2]- mat[11,1]*mat[11,3]), -x)))))))))))
	return(a)	
}

#points(plotrealx(.04), envelope(plotrealx(.04)), col = 'blue')
#points(plotrealx(.34), envelope(plotrealx(.34)), col = 'blue')
#points(plotrealx(.8), envelope(plotrealx(.8)), col = 'green')



#invcdfcalc <-function(a, b, c, d, x1){
#	(quadform(a/2, (b-c*a), (-x1+(a/2)*(d^2) +(b-c*a)*d)))
#}

invcdfcalc2 <-function(a, b, c, d, x1){
	(quadform(a/2, (b-c*a), -x1-((a/2)*(d^2) + b*d - c*a*d)))
}

specialcase<-function(newx){
	return ((newx + 1.5*.45)/1.5)
}

#quadform(2.4, .54-.1*4.8, -.491)
#quadform(mat[2,3]/2, mat[2,2]-mat[2,1]*mat[2,3], -.5 + mat[2,3]/2*(mat[2,4]^2) + 
#(mat[2,2]-mat[2,1]*mat[2,3])*mat[2,4])


invcdf<-function(x){
	a<-0
	ifelse(x<=mat[2,8], a<-invcdfcalc2(mat[1,3], mat[1,2], mat[1,1], mat[1,4], x-mat[1,8]), 
	ifelse(x<=mat[3,8], a<-invcdfcalc2(mat[2,3], mat[2,2], mat[2,1], mat[2,4], x-mat[2,8]),
	ifelse(x<=mat[4,8], a<-invcdfcalc2(mat[3,3], mat[3,2], mat[3,1], mat[3,4], x-mat[3,8]),
	ifelse(x<=mat[5,8], a<-invcdfcalc2(mat[4,3], mat[4,2], mat[4,1], mat[4,4], x-mat[4,8]),
	ifelse(x<=mat[6,8], a<-invcdfcalc2(mat[5,3], mat[5,2], mat[5,1], mat[5,4], x-mat[5,8]),
	ifelse(x<=mat[7,8], a<-specialcase(x-mat[6,8]),
	ifelse(x<=mat[8,8], a<-invcdfcalc2(mat[7,3], mat[7,2], mat[7,1], mat[7,4], x-mat[7,8]),
	ifelse(x<=mat[9,8], a<-invcdfcalc2(mat[8,3], mat[8,2], mat[8,1], mat[8,4], x-mat[8,8]),
	ifelse(x<=mat[10,8], a<-invcdfcalc2(mat[9,3], mat[9,2], mat[9,1], mat[9,4], x-mat[9,8]),
	ifelse(x<=mat[11,8], a<-invcdfcalc2(mat[10,3], mat[10,2], mat[10,1], mat[10,4], x-mat[10,8]),
	 a<-invcdfcalc2(mat[11,3], mat[11,2], mat[11,1], mat[11,4], x-mat[11,8]) ))))))))))
}

keep <-c()

for(n in 1:100){
	currentu <-runif(1,0,1)
	currentx<-runif(1,0,1)
	newcurrentx<-invcdf(currentx)
	currentgx<-f(newcurrentx)
	currenthx<-envelope(newcurrentx)
	if (currenu <(currentgx/currenthx)){
		keep <-c(keep, currentx)
	}
}




