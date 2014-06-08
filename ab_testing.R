# A/B testing
# do more people click on new feature compared to old feature?

n1 <- 100   # people who where tested with old feature
n2 <- 50    # people who "saw" new feature

p1 <- 0.2   # proportion of clicks with old feature
p2 <- 0.26  # proportion of clicks with new feature
p1*n1 # 20  # nb of clicks for old feature
p2*n2 # 13  # nb of clicks for new feature

alpha <- 0.05


getPValue <- function (n1,n2,p1,p2) {
  # H0: p1 >= p2  &   H1: p1 < p2
  # accept H1 if pval < alpha (i.e. new feature is better than old one) 
  res <- prop.test(x=c(p1*n1, p2*n2),n=c(n1,n2), alternative="less")
  return(res$p.value)
}


getPValue(n1,n2,p1,p2)
# we accept H0: p1 >= p2, i.e. old feature is better than new feature 
# (having tested 50 people for new feature and only 13 clicked)


# different situations for n2 and p2
getPValue(100,100,.20,.26) # "H0"
getPValue(100,100,.20,.32) # "H1"
getPValue(100,50,.20,.32) # "H0"



#### How is the p value dependent on the test sample size and test proportion?
#### (test refers to new feature)

# analyse dependency on n2 for fixed p2 proportion
N2 <- seq(10, n1, by=10)
pval2 <- sapply(N2, getPValue, n1=n1, p1=p1, p2=p2)
xlim <- c(0,max(N2))
plot(N2, pval2, xlim=xlim, ylim=c(0,0.5),
     main=paste("p value depending on test sample size n2 for fixed p2=",p2,",\nn1=",n1,", p1=",p1,sep=""))
rect(0,0,max(N2),alpha, col='lightgrey')
mtext('here is the region to accept H1, i.e. new feature > old feature',side=1,line=-1.7)


# analyse dependency with variable n2 and p2
P2 <- seq(0,1,by=0.02)

# calculate p values for different values of n2 and p2
pvals_p <- matrix(,nrow=length(N2),ncol=length(P2))
for (i in 1:length(N2)) {
  for (j in 1:length(P2)) {
    pvals_p[i,j] <- getPValue(n1=100,N2[i],p1,P2[j])
  }
}

# 3d plot of p value depending on both n2 and p2
persp(N2, P2, pvals_p, theta=115, phi=0, ticktype="detailed", col="light pink",
      main=paste("p value depending on sample size n2 and proportion p2\nn1=",n1,", p1=",p1,sep=""),
      zlim=c(0,1), nticks=5) 


# multi line plot of p value depending on p2 (for different values for n2)
plot(P2, pvals_p[1,], ylim=c(0,0.4), t='n', xlab="p2", ylab="p value",
     main=paste("p value for different test sample sizes n2 depending on proportion p2\nn1=",n1,", p1=",p1,sep=""))
rect(-.2,0,max(N2),alpha, col='lightgrey')
mtext('accept H1 here',side=1,line=-1.9,adj=0.01)
mtext('(i.e. new feature > old)',side=1,line=-1.9,adj=0.99)
rbPal <- colorRampPalette(c('red','darkgreen','darkblue'))
b <- rbPal(length(N2))[1:length(N2)]
l <- sapply(1:length(N2), FUN=function(x){ lines(P2, pvals_p[x,],col=b[x], lwd=2) })
legend('topright',paste("n2=",N2,sep=""),col=rbPal(length(N2)),pch=16)

# --> of 20 test cases we would need a success rate of 0.4 to accept the new feature
#     i.e. 8 of 20 would need to click

# What is the necessary click rate for n2=100? 
P2[which(pvals_p[which(N2==100),] < alpha)[1]]
# of 100 test cases we would need a success rate of 0.32 to accept the new feature
# i.e. 32 of 100 would need to click


# get for each test sample size the success rate that would be needed to accept
# the new feature:
p2_min <- apply(pvals_p, MARGIN=1, FUN=function(x){ P2[which(x < alpha)[1]] })
cbind(N2,p2_min)


