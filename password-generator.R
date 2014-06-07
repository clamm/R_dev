pwGenerator=function(pwlength){
	abc<- letters
	ABC<- toupper(letters)
	ziffern<- 0:9
	sonderzeichen<- c('!', '?', '$', '%', '&', '/', '=' ,'?')
	basis<- c(abc, ABC, ziffern, sonderzeichen)
	index<- sample(1:length(basis),pwlength,replace=T)
	pw<- basis[index]
	paste(pw, collapse="")
	}
	
pwGenerator(8)