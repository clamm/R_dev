#----- test how to do simple calculus with arrays:
A<- as.array(cbind(1:4,4:7,7:10))
B<- as.array(cbind(3:5,5:7,8:10))
A;B
C<- A-B
C^2
sum(C^2)
A<- array(c(1,2))
B<- as.array(matrix(2:7,2,3))
A;B
A%*%B

A<- as.array(matrix(1:15,3,5))
B<- t(A)
A;B
#I want to multiply A's columns with B's rows -> each product gives a 3x3 matrix

C<- c(1:4)
D<- c(1:4)
C;D
C%*%D

A<- as.array(matrix(1:15,3,5))
B<- array(A,dim=c(dim(A)[1],1,dim(A)[2]))
C<- array(A,dim=c(1,dim(A)[1],dim(A)[2]))
B[,,1]*C[,,1]
B;C
B * C	

F<- array(1:12,dim=c(2,2,3))
F
F*c(rep(1,length(F[,,1])),rep(2,length(F[,,1])),rep(3,length(F[,,1])))

G<- matrix(1:12,3,4)
G
G*c(1,1,1,2)

J<- matrix(1:15,3,5)
J
K<- matrix(1:15/10,5,3)
K

apply(K,2,"%*%",J)

sapply(1:5,FUN=expo,A=K,B=as.list(J))

