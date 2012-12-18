### Creating example data set:
mydates<-rep(seq(as.Date("2008-12-29"), length = 100, by = "day"),2)
myfactor<-c(rep("group.1",100),rep("group.2",100))
set.seed(123)
myvalues<-runif(200,0,1)
myframe<-data.frame(dates=mydates,group=myfactor,value=myvalues)
(myframe)
dim(myframe)

## Removing same rows (dates) unsystematically:
set.seed(123)
removed.group1<- sample(1:100,size=20,replace=F)
set.seed(456)
removed.group2<- sample(101:200,size=20,replace=F)
to.remove<- c(removed.group1,removed.group2);length(to.remove)
to.remove<- to.remove[order(to.remove)]
myframe<- myframe[-to.remove,]
(myframe)
dim(myframe)
names(myframe)