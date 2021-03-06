#Tutorial de Statistique 1
#Sujet 3 - Loi beta

set.seed(3)

#help("rbeta")
#important comparer la saisie des param�tres! - cf Wikip�dia et �nonc�
#c'est bon, pas de choses particuli�res � respecter

simu_beta=function(n,B,theta)
{
	S <- matrix(rbeta(n*B, theta, 2),nrow=n,ncol=B,byrow=FALSE)
	return(S)	
}

#explication:
#on cr�e une fct qui va �tre appliquer 
#pour simuler des diff�rents nombre d'�chantillons B=500
#et de taille n=20
#d'apr�s une loi de BETA de param�tres theta, shape2=2
#les B colonnes de la matrice cr��e contiennent � la fois 
#un �chantillon simul�
#donc les N lignes par colonne sont des N observations simul�s 
#par �chantillon
#R simule en fait N*B observations d'une loi BETA 
#R les distribue en descendant les colonnes une apr�s une 

theta_list<- seq(from=2, to=4, by=0.1)

#-------------------------------TEST 1---------------------------------

test<-sapply(theta_list,n=20,B=500,simu_beta)
#donne une enorme matrice, pas clair o� se trouve les sous-matrices

#-------------------------------TEST 2---------------------------------

test2<-simu_beta(n=20,B=500,theta_list[1])
#donne une seule matrice, des �ch tir�s d'apr�s theta=2.0

#-------------------------------TEST 3---------------------------------

test3<- array(data=1:48,dim=c(4,3,5),dimnames=c("n","B","theta"))
#array(data,dim=c(nbrow,nbcol,nb subtables))
test3
is.matrix(test3[,,1])
#donne un truc multidimensionnel ("array")
#les colonnes de chaque sous-matrice test3[,,chiffre] sont des �ch
#le nombre de lignes des sous-matrices correspond � la taille des �ch
#sens de r�partition: par colonne, par sous-matrice
#-----
#summary(test3)
#is.matrix(test3)
#is.list(test3)
#is.array(test3)
#length(test3)

#-------------------------------TEST 4---------------------------------

test4<- array(
		c(	rnorm(n=12,mean=0,sd=1),
		  	rnorm(n=12,mean=50,sd=1)),
		dim=c(4,3,2),
		dimnames=c("n","B","length theta")
		)
test4
#donne un array avec 2 sous-matrices:
#1�re remplie avec des randoms N(0,1)
#2�me remplie avec des randoms N(50,1)

#-------------------------------TEST 5---------------------------------

#ensuite essayer � all�ger l'�criture:
moyenne<- c(0,50)
simu_norm<- sapply(moyenne,n=12,sd=1,rnorm)
is.matrix(simu_norm)
#donne une matrice avec 2 colonnes:
#1�re avec des randoms N(0,1)
#2�me avec des randoms N(50,1)

test5<- array(
		sapply(moyenne,n=12,sd=1,rnorm),
		dim=c(4,3,2),
		dimnames=c("n","B","length theta")
		)
test5

#tester si le sens de r�partition est encore valable
#diff�rence � test4: input �tait un vecteur, � test5 c'est une matrice

#-------------------------------TEST 51---------------------------------

test51<- array(
		rnorm(n=12,moyenne,sd=1),
		dim=c(4,3,2),
		dimnames=c("n","B","length theta")
		)
test51
#attention!!! la r�partition n'est pas correcte!!!

#-------------------------------TEST 6---------------------------------

#cr�er la matrice input pour tester
matrix(data=1:24,nrow=12,ncol=2,byrow=FALSE)

test6<- array(
		matrix(data=1:20,nrow=12,ncol=2,byrow=FALSE),
		dim=c(4,3,2),
		dimnames=c("n","B","length theta")
		)
test6
#effectivement,sens de r�partition reste pareil

#-------------------------------TEST 7---------------------------------
theta_list_test <- c(2,3,4)
simu_beta_test<- array(
		sapply(theta_list_test,n=2*3,shape2=2,rbeta),
		dim=c(2,3,length(theta_list_test)),
		dimnames=c("n","B","theta")
		)
simu_beta_test
#acc�s aux sous-matrices=simu d'un theta, p.ex. dernier theta:
#simu_beta_test[,,21]

#-------------------------------TEST 8---------------------------------

simu_beta_test2<- array(
		rbeta(n=2*3,shape2=2,theta_list_test),
		dim=c(2,3,length(theta_list_test)),
		dimnames=c("n","B","theta")
		)
simu_beta_test2


#-------------------------------vraie SIMU-----------------------------

#vraie simulation, avec une taille de l'�ch n=20, pour B=500 �ch
simu_beta<- array(
		sapply(theta_list,n=20*500,shape2=2,rbeta),
		dim=c(20,500,length(theta_list)),
		dimnames=c("n","B","theta")
		)
simu_beta


#------------------------------Kronecker--------------------------------
#help("kronecker")

n<- 5
m<- 2
nb_mu<- 3
mu<- seq(-6,6,length=nb_mu)
liste_mu<- kronecker(array(data=mu,c(1,1,nb_mu)), 
				array(1,c(m*n)))

donnees<- array(rnorm(n*m*nb_mu,liste_mu),c(m,n,nb_mu))
apply(donnees[,,],3,mean)	#moyenne par sous-tableau
length(donnees)	#5*2*3

eins<- array(data=1:24,c(2,3,4))
eins[1,,2]	#affiche la 1�re ligne du 2�me sous-tableau
eins[,3,2]	#affiche la 3�me colonne du 2�me sous-tableau


#dim(output)=nb_subtables, nb_col ,nb_lin
