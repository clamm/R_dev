library('glmnet')

#### data
x=matrix(rnorm(100*20),100,20)
y=rnorm(100)
g2=sample(1:2,100,replace=TRUE)
g4=sample(1:4,100,replace=TRUE)


#### linear model
fit1=glmnet(x,y)
predict(fit1,newx=x[1:5,],s=c(0.01,0.005))
predict(fit1,type="coef")
plot(fit1,xvar="lambda")


#### binomial model
fit2=glmnet(x,g2,family="binomial")
predict(fit2,type="response",newx=x[2:5,])
predict(fit2,type="nonzero")


#### multinomial model
fit3=glmnet(x,g4,family="multinomial")
predict(fit3,newx=x[1:3,],type="response",s=0.01)



#### play around with binomial model

# source functions to compute precision, recall and F1 score
source('precision_recall.R')

phat = predict(fit2, type="response", newx=x, s=fit2$lambda)

g2hat = phat > 0.5

f1 = c()
for (c in 1:dim(g2hat)[2]) {
  f1[c] = F1score_from_actual_pred(g2,g2hat)
}

plot(f1)

fit2$lambda[which(f1==max(f1))]

