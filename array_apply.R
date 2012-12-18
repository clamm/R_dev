#------------------------------array------------------------------------------
undeuxtrois<- array(data=c(1:6), dim=c(1,30))
undeuxtrois<- sort(undeuxtrois)
p<- array(data=undeuxtrois, dim=c(5,2,3), dimnames=c("n","B","theta"))
q<- c(t(p[,,1]),t(p[,,2]),t(p[,,3]))
r<- array(data=q, dim=c(2,5,3))
r[,,1]

#------------------------------array and apply--------------------------------
s<- array(data=c(1:60), dim=c(3,4,5))
s

#MARGIN= {1=rows, 2=columns, 3=subtables} and combinations
#2 ways to enter MARGIN: c(1,3)!=1:3 
#MARGIN=1:1 <-> MARGIN=1
#MARGIN=2:1 gives same result as MARGIN=1:2 but transposed
   #dim(.)=rows,columns
   #dim(output1:2)=nb_lin, nb_col   vs   dim(output2:1)=nb_col, nb_lin

apply(s,MARGIN=1:1,FUN=min)
apply(s,MARGIN=1,FUN=min)
apply(s,MARGIN=1,FUN=max)
apply(s,MARGIN=1,FUN=mean)
#MARGIN=1 -> FUN applied to each row of the whole table,
			#no regard to subtables

apply(s,MARGIN=2:2,FUN=min)
apply(s,MARGIN=2,FUN=min)
apply(s,MARGIN=2,FUN=max)
apply(s,MARGIN=2,FUN=mean)
#MARGIN=2 -> FUN applied to each column of the whole table, 
			#no regard to subtables

apply(s,MARGIN=3:3,FUN=min)
apply(s,MARGIN=3,FUN=sum)
apply(s,MARGIN=3,FUN=max)
apply(s,MARGIN=3,FUN=mean)
#MARGIN=3 -> FUN applied to each subtable


apply(s,MARGIN=c(1,2),FUN=min)
apply(s,MARGIN=1:2,FUN=min)
apply(s,MARGIN=1:2,FUN=max)
apply(s,MARGIN=1:2,FUN=sum)
#MARGIN=c(1,2) and MARGIN=1:2
 #-> FUN applied to each element [i,j] of each subtable
#dim(output)=nb_lin, nb_col
#applied on a mtrix: function is applied to every element [i,j] of the matrix

#MARGIN=c(2,1) and MARGIN=2:1
 #-> FUN applied to each element [i,j] of each subtable
#dim(output)=nb_col, nb_lin

s
1+13+25+37+49
4+16+28+40+52

apply(s,MARGIN=c(2,3),FUN=min)
apply(s,MARGIN=2:3,FUN=min)
apply(s,MARGIN=2:3,FUN=max)
apply(s,MARGIN=2:3),FUN=mean)
#MARGIN=c(2,3) and MARGIN=2:3
 #-> FUN applied to each column of each subtable
#dim(output)=nb_col, nb_subtables

#MARGIN=c(3,2) and MARGIN=3:2
 #-> FUN applied to each column of each subtable
#dim(output)=nb_subtables, nb_col


apply(s,MARGIN=c(1,3),FUN=min)
apply(s,MARGIN=c(1,3),FUN=max)
apply(s,MARGIN=c(1,3),FUN=mean)
#MARGIN=c(1,3) -> FUN applied to each row of each subtable	
#dim(output)=nb_lin, nb_subtables

apply(s,MARGIN=c(3,1),FUN=min)
apply(s,MARGIN=c(3,1),FUN=max)
apply(s,MARGIN=c(3,1),FUN=mean)
#MARGIN=c(1,3) -> FUN applied to each row of each subtable	
#dim(output)=nb_subtables, nb_lin


min2<- function(x){min(x)/2}
apply(s,MARGIN=1:3,FUN=min)
apply(s,MARGIN=1:3,FUN=min2)
apply(s,MARGIN=1:3,FUN=max)
apply(s,MARGIN=1:3,FUN=mean)
#MARGIN=1:3 -> FUN applied to each element [i,j] of each subtable	
#dim(output)=nb_lin, nb_col ,nb_subtables

min2<- function(x){min(x)/2}
apply(s,MARGIN=3:1,FUN=min)
apply(s,MARGIN=3:1,FUN=min2)
apply(s,MARGIN=3:1,FUN=max)
apply(s,MARGIN=3:1,FUN=mean)
#MARGIN=3:1 -> FUN applied to each element [i,j] of each subtable	




# How to sort array after 1st column:
# A<- array(sample(1:60,50),dim=c(5,2,3))
# A
# p<-apply(A[,1,],MARGIN=2,FUN=order)
# p
# A[p[,1],,] #every subtable ordered as the first one
# A[p[,2],,]

# A3<- array(,c(dim(A)[1],dim(A)[2]+1,dim(A)[3]))
# for (i in 1:dim(A)[3]){
	# A3[,,i]<- cbind(A[,,i],apply(A[,2,],MARGIN=2,FUN=cumsum)[,i])
# }
# A3

# oA<-array(,dim=c(5,2,3))
# for (i in 1:dim(A)[3]){
	# oA[,,i]<-A[p[,i],,i]
# }
