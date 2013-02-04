# CSV structure in file data.csv
# id,age,gender, blub
# 1,19,male, blub
# 2,25,female, blub
# gender as string [male, female]
# CSV possible to extend

#setwd('server') #set path to where this .R file and data.csv is saved
graphics.off()	#turn off all graphic devices
#data<- read.csv('data.csv',header=TRUE) 
#attach(data) #make R assign data values to variables defined in .csv file

max_user_age=65	#set maximum user age
min_user_age=16 #set minimum user age

table(gender) #count total of male and female users


#-------------------- simulate data: ------------------

simu_n=80000 #set number of generated users
gender<- as.factor(sample(c('female','male'),simu_n,replace=T)) #simulate gender
#simulate age according to normal distribution:
age<- floor(rnorm(simu_n, mean=25, sd=10)) 
#simulate age according to uniform distribution:
#age<- sample(min_user_age:max_user_age,simu_n,replace=T) 
data<- as.data.frame(cbind(id=1:simu_n,age=age,gender=gender))
head(data) #show first 10 users and their associated variable values

#-------------------- end of simulation ---------------


#--------- plot and print general age-gender distribution -------------

#create age categories: (necessary input for barplot-fct)
cat_age= cut(age,c(seq(min_user_age,max_user_age,by=5),max_user_age),
	include.lowest=T)

#plot general age-gender distribution:
#save plot as pdf to home path:
pdf(paste(Sys.Date(),'-total-age-gender-distribution.pdf',sep=""),width=9) 
	barplot(table(gender, cat_age), beside=T, col=3:4, ylab='Frequency', 
		xlab='Age', ylim=c(0, .1*length(age)), 
		legend= levels(gender), 
		main= paste("Total age distribution by gender",Sys.Date()),
		sub= paste('total users: ',levels(gender)[1],': ',table(gender)[1],', ',
			levels(gender)[2],': ',table(gender)[2], sep=''))
dev.off() #close plot window to finish saving to pdf

#--------- end of plot and print general age-gender distribution -------------


#--------- plot and print age-gender distribution for special set -------------

#function to create age range of a set for a given user age:
create_set=function(user_age){
	if (user_age < min_user_age){
		cat('Warning: user_age too young, automatically set to minimum user age \n')
		user_age<- min_user_age}
	if (user_age > max_user_age){
		cat('Warning: user_age too old, automatically set to maximum user age \n')
		user_age<- max_user_age
		}
	if (user_age <= min_user_age+5){ #reach to lower boundary
		min_user_age:(user_age+5)}
	else if (user_age >= max_user_age-5){ #reach to upper boundary
		(user_age-5):max_user_age}
	else {
		(user_age-5):(user_age+5)} #no boundary reached
	}

#call fct create_set:
#create_set(user_age=16)


#function to plot the age-gender distribution for a set of a given user age:
plot_age_gender=function(user_age,age,gender){
	#input: user_age - user age the set should be created for
	#		age		 - age data from .csv
	#		gender	 - gender data from .csv
	set_ages<- create_set(user_age) #creat range +-5 around user_age
	keep_id<- age %in% set_ages #mark users that are in set
	subtitle<- paste('total set users: ',levels(gender[keep_id])[1],': ',
		table(gender[keep_id])[1],', ',levels(gender[keep_id])[2],': ',
		table(gender[keep_id])[2], sep='')
	ymax<- max(table(gender[keep_id], factor(age[keep_id],levels=set_ages)))
	barplot(table(gender[keep_id], factor(age[keep_id],levels=set_ages)), 
		beside=T, col=3:4, legend=levels(gender), xpd=T, ylab='Frequency', 
		xlab='Age', main = paste("Age distribution by gender for Set",user_age),
		sub=subtitle, ylim=c(0,ymax+.3*ymax))
	mtext(paste("Date:",Sys.Date()),side=3)
	}



#for (user_age in 20:30){ #possible to loop over sets via user_age
	#save plot as pdf to home path:
	pdf(paste(Sys.Date(),'-age-gender-distribution-set-',user_age,'.pdf',sep="")) 
		#call fct plot_age_gender for set user_age to open plot window:
		plot_age_gender(user_age=user_age,age=age,gender=gender)
	dev.off() #close plot window to finish saving to pdf
#} #end for

#--------- end of plot and print age-gender distribution for special set -------