#plotting the F and the Chi2 density
x<- seq(-10,10, by=.1)
plot(x, df(x, 4, 317), t="l", main="F", col="red")
lines(x, dchisq(x,3), col="blue")
pvalue<- 1 - pf(2.627071823, 4,317)

#plotting the Student's density
z<- seq(-10,10, by=.1)
plot(z, dt(z, 317), t="l", main="Student")

#plot d'une densité d'une loi exponentielle, lambda=0.18
z<- seq(-1,100, by=.1)
plot(z, dexp(z, 0.18), t="l", main="Exponentielle, lambda=0.18")


#t test:
t_p_value = function(t_emp, df)
#for a t test, df= n-k-1
#t_emp= beta/sd(beta)
{
	return (2 - 2*pt(t_emp, df))
}

significant = function(pvalue, alpha)
#true means "H1", two-sided test
{
	return (pvalue < alpha/2)
}


beta_all<- c(0.07,-0.02,-0.01,0,0.02,0.02,0.01,0.05,-0.02,0.05)
sd_beta_all<- c(0.03,	0.01,	0.01,	0.01,	0.01,	0.01,	0.01,	0.02,	0.01,	0.01)
temp_all<- beta_all/sd_beta_all
res_all<- sapply(temp_all, FUN=t_p_value, df=4845)
sapply(res_all, FUN=significant, alpha=.05)
sapply(res_all, FUN=significant, alpha=.01)

beta_w<- c(0.13, -0.04, -0.01, 0.02, 0.03,	0.03,	0.02,	0.06,	-0.04, 0.06)
sd_beta_w<- c(0.04,	0.01,	0.01,	0.03,	0.01,	0.02,	0.01,	0.03,	0.02,	0.02)
temp_w<- beta_w/sd_beta_w 
res_w<- sapply(temp_w, FUN=t_p_value, df=2410)
sapply(res_w, FUN=significant, alpha=.05)
sapply(res_w, FUN=significant, alpha=.01)

beta_b<- c(0.02,	0,	0.01,	-0.01,0,	0.01,	0,	0.03,	0,	0.04)
sd_beta_b<- c(0.03,	0.01,	0.01,	0.02,	0.01,	0.01,	0.01,	0.02,	0.01,	0.01)
temp_b<- beta_b/sd_beta_b 
res_b<- sapply(temp_b, FUN=t_p_value, df=2410)
sapply(res_b, FUN=significant, alpha=.05)
sapply(res_b, FUN=significant, alpha=.01)


array(c(temp, res, resu), dim=c(8,3), 
	dimnames=c("empirical value of the t statistic", "p-value", "H1"))

#F test:

#F p_value
F_p_value = function(F_emp, q, df)
#for an F test, q=nb of restriction, df= n-k-1
#F_emp= (R²ur-R²r)/q / (1-R²ur)/(n-k-1)
{
	return (1 - pf(F_emp, q, df))
}

F_p_value(8.136, 4, 366-8)
