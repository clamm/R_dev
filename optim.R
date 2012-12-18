#----- test optim:
#data consists of draws from a Poisson distribution with unknown mu 
ngrid<-50 	#nb of grid points
u<- seq(from=0,to=1,length=ngrid)	#grid
h<- c(0.1,0.2,0.4)	#vector of bandwidths

X<- rpois(n=10,lambda=5)
U<- rnorm(n=10)
data<- data.frame(X,U)
nrow(data)
data

ind<- which(abs(U-u[1])>h[3])
ind
X[ind]

localdata = function(data,u,h){
	index<- which(abs(data[[2]]-u)>h)
	data[[1]][index]
	}

localdata(data,u[40],h[3])

ldata<- sapply(u,FUN=localdata,data=data,h=h[3])
names(ldata)<- paste("u",1:50,sep="")
ldata #for each grid point u1-u50 we have the corresponding local data X points

#we want to estimate mu via MLE
poisson.lik = function(mu,y){
	nb<- length(y)
	logl<- sum(y)*log(mu)-nb*mu
	return(-logl)
	}

sapply(ldata,FUN=poisson.lik,mu=5)
optim(par=1,fn=poisson.lik,y=ldata[[1]],method="BFGS")
#lapply(ldata,FUN=optim,par=1,fn=poisson.lik,y='x',method="BFGS")

lres<- list()
for (i in 1:length(u)){
	lres[i]<- optim(1,fn=poisson.lik,y=ldata[[i]],method="BFGS")
	}
lres 