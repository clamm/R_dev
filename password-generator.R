pwGenerator=function(pwlength){
	abc<- letters
	ABC<- toupper(letters)
	ziffern<- 0:9
	sonderzeichen<- c('!', '§', '$', '%', '&', '/', '=' ,'?')
	basis<- c(abc, ABC, ziffern, sonderzeichen)
	index<- sample(1:length(basis),pwlength,replace=T)
	pw<- basis[index]
	paste(pw, collapse="")
	}
	
pwGenerator(8)
	
	

print('Multiply these numbers by 11:')
sample(c(11:20),3)

i = 12
paste('Divide these numbers by ',i,':', sep='')
sample(c(i*11:20),5)