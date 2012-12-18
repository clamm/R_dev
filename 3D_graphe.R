############################   persp simple   ############################
	f<- function(x,y)
	{
	a<- x*y
	b<- (1+x^2)*(1+y^2)
	return (a/b)
	}

x<- seq(-5,5, by=0.1)
y<- seq(-5,5, by=0.1)

# Für jede Kombination von Komponenten aus den Vektoren x und y wird ein  
# Wert der Funktion f berechnet und in der Matrix mat gespeichert: 
mat<- outer(x,y, f)


######### To understand how outer() works:
a <- 1:9; names(a) <- a
# Multiplication & Power Tables
a %o% a

b <- 2:8; names(b) <- paste(b,":",sep="")
outer(b, a, "^")




#3D Darstellung
persp(x, y, mat, theta = 30, phi = 30,  col = "light pink") 

############### (4) Surface colours corresponding to z-values ###############

x <- seq(-1.95, 1.95, length = 30)
y <- seq(-1.95, 1.95, length = 35)
z <- outer(x, y, function(a,b) a*b^2)
nrz <- nrow(z)
ncz <- ncol(z)
# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette( c("blue", "green") ) 
# Generate the desired number of colors from this palette
nbcol <- 100
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)
persp(x, y, z, col=color[facetcol], phi=30, theta=-30)





############### 3D Bivariate Normal Density ############### 
#
#
mu1<- 0.4 # setting the expected value of x1
mu2<- 0.3 # setting the expected value of x2
s11<- 1 # setting the variance of x1
s12<-15 # setting the covariance between x1 and x2
s22<-10 # setting the variance of x2
rho<-0.5 # setting the correlation coefficient between x1 and x2
x1<-seq(-10,10,length=41) # generating the vector series x1
x2<-x1 # copying x1 to x2
#
f<-function(x1,x2){
	term1<- 1/(2*pi*sqrt(s11*s22*(1-rho^2)))
	term2<- -1/(2*(1-rho^2))
	term3<- (x1-mu1)^2/s11
	term4<- (x2-mu2)^2/s22
	term5<- -2*rho*((x1-mu1)*(x2-mu2))/(sqrt(s11)*sqrt(s22))
	term1*exp(term2*(term3+term4-term5))
	} # setting up the function of the multivariate normal density
#
z<-outer(x1,x2,f) # calculating the density values
#
persp(x1, x2, z,
	main="Two dimensional Normal Distribution",
	sub=expression(italic(f)~(bold(x))==frac(1,2~pi~sqrt(sigma[11]~
		sigma[22]~(1-rho^2)))~phantom(0)^bold(.)~exp~bgroup("{",
		list(-frac(1,2(1-rho^2)),
		bgroup("[", frac((x[1]~-~mu[1])^2, sigma[11])~-~2~rho~frac(x[1]~-~mu[1],
		sqrt(sigma[11]))~ frac(x[2]~-~mu[2],sqrt(sigma[22]))~+~
		frac((x[2]~-~mu[2])^2, sigma[22]),"]")),"}")),
	col="lightgreen",
	theta=30, phi=20,
	r=70,
	d=0.1,
	expand=0.5,
	ltheta=90, lphi=180,
	shade=0.75,
	ticktype="detailed",
	nticks=5) # produces the 3-D plot
#
mtext(expression(list(mu[1]==0,mu[2]==0,sigma[11]==10,sigma[22]==10,sigma[12
]==15,rho==0.5)), side=3) # adding a text line to the grap







# ---------------------------------------------------------------------
# Book:         SFE3
# ---------------------------------------------------------------------
# Quantlet:     SFEdelta 
# ---------------------------------------------------------------------
# Description:  SFEdelta plots the Delta of a call option
# ---------------------------------------------------------------------
# Usage:        -
# ---------------------------------------------------------------------
# Inputs:       S_min - Lower Bound of Asset Price S
#               S_max - Upper Bound of Asset Price S
#               tau_min - Lower Bound of Time to Maturity tau
#               tau_max - Upper Bound of Time to Maturity tau
# ---------------------------------------------------------------------
# Output:       plot of the Delta of a call option
# ---------------------------------------------------------------------
# Example:      User inputs [lower, upper] bound of Asset price S like
#               [50,150], [lower, upper] bound of time to maturity tau 
#               like [0.01, 1], then plot of the Delta of a call option 
#               is given.
# ---------------------------------------------------------------------
# Author:       Wolfgang Haerdle, Ying Chen 20021101 Christian Hafner,
#               990627
# ---------------------------------------------------------------------


## Main computation
Smin<- 50			#lower bound of Asset price S
Smax<- 150			#upper bound of Asset price S
Taumin<- .01		#lower bound of time to maturity tau
Taumax<- 1			#upper bound of time to maturity tau

K<- 100			# exercise price 
r<- 0				# interest rate
sig<- 0.25			# volatility
d<- 0				# dividend rate
steps<- 60


Tau<- seq(Taumin,Taumax,by=(Taumax-Taumin)/(steps-1))
#tau<- matrix(seq1,steps,steps,byrow=T)

S<- seq(Smax,Smin,by=-(Smax-Smin)/(steps-1))
#S<- matrix(seq2,steps,steps,byrow=F)

delta = function(Tau,S,K,r,d,sig){
	y<- (log(S/K)+(r-d+sig^2/2)*Tau)/(sig*sqrt(Tau))
	return(pnorm(y))
	}

#names(Tau)<- paste("Tau",1:steps,sep="")
#names(S)<- paste("S",1:steps,sep="")

mesh <- outer(Tau,sort(S),delta,K=K,r=r,d=d,sig=sig)
#mesh <- t(mesh)



#-----------------still to change: Achsenbeschriftung
## Plot

# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette( c("darkblue","blue","green","yellow","brown") ) 
# Generate the desired number of colors from this palette
nbcol <- 100
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
nrz <- nrow(mesh)
ncz <- ncol(mesh)
zfacet <- mesh[-1, -1] + mesh[-1, -ncz] + mesh[-nrz, -1] + mesh[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)
persp(mesh, theta = 45, phi = 30, col=color[facetcol], 
	main="Delta of Delta Hedging",xlab="Asset Price S",
	ylab="Time to Maturity tau",ticktype="detailed",nticks=5)
