##############################################################################
#example for princomp()
##############################################################################

require(graphics)

## The variances of the variables in the
## USArrests data vary by orders of magnitude, so scaling is appropriate
(pc.cr <- princomp(USArrests))  # inappropriate
princomp(USArrests, cor = TRUE) # =^= prcomp(USArrests, scale=TRUE)
## Similar, but different:
## The standard deviations differ by a factor of sqrt(49/50)

summary(pc.cr <- princomp(USArrests, cor = TRUE))
loadings(pc.cr)  ## note that blank entries are small but not zero
plot(pc.cr) # shows a screeplot.
biplot(pc.cr)

## Formula interface
princomp(~ ., data = USArrests, cor = TRUE)
# NA-handling
USArrests[1, 2] <- NA
pc.cr <- princomp(~ Murder + Assault + UrbanPop,
                  data = USArrests, na.action=na.exclude, cor = TRUE)
pc.cr$scores


princomp(USArrests, cor = TRUE) > 1


##############################################################################
#my tests
##############################################################################

A<- matrix(rnorm(16),4,4)
A
A_mean<- apply(A, FUN=mean, MARGIN=2)
A_mean
center=function(vec)
{
	return(vec-mean(vec))
}

apply(A, FUN=center, MARGIN=2)


B<- matrix(seq(1,9,1),3,3)
C<- matrix(seq(12,4,-1),3,3)
B;C
intersect(B,C)

D<- matrix(rnorm(n=10*10, 9),10,10)
res1<- prcomp(D, retx=TRUE, center=TRUE, scale=TRUE)

plot(res1$rotation[,1], res1$rotation[,2])


(x <- c(sort(sample(1:20, 9)),NA))
(y <- c(sort(sample(3:23, 7)),NA))

storage.mode(x)<- "character"
storage.mode(y)<- "character"
union(x, y)
intersect(x, y)
setdiff(x, y)
setdiff(y, x)
setequal(x, y)

D<- seq(7,16,1)
E<- seq(13,18,1)
D[which(D%in%E)]
E[which(E%in%D)]
intersect(D,E)

?intersect

#####################################
#apply and 2 vectors two iterize
#####################################
F<- matrix(seq(1,9,1),3,3)
sdF<- c(.2,2,-4)

Test = function(axe, et)
{
	ind<- which(axe > 2*abs(et))
	return(ind)
}
Test(F[,1], sdF[1]) #== which(F[,1] > 0.4)
Test(F[,2], sdF[2]) #== which(F[,2] > 4)
Test(F[,3], sdF[3]) #== which(F[,3] > 8)

#fct Test should be applied on each column of F with "et" = the 
#corresponding value of sdA
apply(F, FUN=Test, MARGIN=2:3, et=sdF)


####################################
#scale on matrix
####################################
g1<- rnorm(3, m=6, sd=1)
g2<- rnorm(3, m=60, sd=10)
g3<- rnorm(3, m=600, sd=50)

G<- matrix(c(g1,g2,g3), 3, 3)
G

scale(G)
#scale(G) is the same as:
apply(G, FUN=scale, MARGIN=2)


####################################
#matrix multiplication
####################################

A<- matrix(rnorm(12),4,3)
A

B<- matrix(seq(1,9,1),3,3)

A%*%B


####################################
#vectors of different length in one object
####################################

b<- c(1:9)
a<- c(1:2)
list(a,b)


####################################
#sort a matrix by column
####################################

b<- c(1:9)
r<- rnorm(9, m=0)
cbind(b,r)

o <- order(r,b)
cbind(b,r)[o, ]

####################################
#pie chart
####################################
require(grDevices)
pie(rep(1, 24), col = rainbow(24), radius = 0.9, border=NA)

n <- 200
pie(rep(1,n), labels="", col=rainbow(n), border=NA,
    main = "pie(*, labels=\"\", col=rainbow(n), border=NA,..")



