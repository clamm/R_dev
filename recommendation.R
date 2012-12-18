# ------------------------------------------------------------------------------
# Try to recompute the Mahout item-based recommendation algorithm:
# ------------------------------------------------------------------------------

data<- read.table("~/Downloads/ipcount.dat",col.names=c("ip","freq"))
head(data)
attach(data)
plot(freq, t='l', xlab="ipaddress")
plot(sort(freq))

nItems <- 5
nUsers <- 4

pref <- data.frame(matrix(4*runif(nUsers*nItems),nUsers,nItems),
                   row.names = paste('u',1:nUsers,sep=''))
colnames(pref) <- paste('i',1:nItems,sep='')
pref

# 
#The standard procedure is to pairwisely compare the columns of the user-item-matrix 
#(the item-vectors) using a similarity measure like pearson-correlation, cosine or 
#loglikelihood to obtain similar items and use those together with the user's ratings 
#to predict his/her preference towards unknown items.

cor(pref) * upper.tri(cor(pref))




