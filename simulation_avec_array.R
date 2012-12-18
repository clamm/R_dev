#Tutorial de Statistique 1
#Sujet 3 - Loi beta

set.seed(3)

#help("rbeta")
#important comparer la saisie des paramètres! - cf Wikipédia et énoncé
#c'est bon, pas de choses particulières à respecter

simu_beta=function(n,B,theta)
{
	S <- matrix(rbeta(n*B, theta, 2),nrow=n,ncol=B,byrow=FALSE)
	return(S)	
}

#explication:
#on crée une fct qui va être appliquer 
#pour simuler des différents nombre d'échantillons B=500
#et de taille n=20
#d'après une loi de BETA de paramètres theta, shape2=2
#les B colonnes de la matrice créée contiennent à la fois 
#un échantillon simulé
#donc les N lignes par colonne sont des N observations simulés 
#par échantillon
#R simule en fait N*B observations d'une loi BETA 
#R les distribue en descendant les colonnes une après une 

theta_list<- seq(from=2, to=4, by=0.1)

#-------------------------------TEST 1---------------------------------

test<-sapply(theta_list,n=20,B=500,simu_beta)
#donne une enorme matrice, pas clair où se trouve les sous-matrices

#-------------------------------TEST 2---------------------------------

test2<-simu_beta(n=20,B=500,theta_list[1])
#donne une seule matrice, des éch tirés d'après theta=2.0

#-------------------------------TEST 3---------------------------------

test3<- array(data=1:48,dim=c(4,3,5),dimnames=c("n","B","theta"))
#array(data,dim=c(nbrow,nbcol,nb subtables))
test3
is.matrix(test3[,,1])
#donne un truc multidimensionnel ("array")
#les colonnes de chaque sous-matrice test3[,,chiffre] sont des éch
#le nombre de lignes des sous-matrices correspond à la taille des éch
#sens de répartition: par colonne, par sous-matrice
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
#1ère remplie avec des randoms N(0,1)
#2ème remplie avec des randoms N(50,1)

#-------------------------------TEST 5---------------------------------

#ensuite essayer à alléger l'écriture:
moyenne<- c(0,50)
simu_norm<- sapply(moyenne,n=12,sd=1,rnorm)
is.matrix(simu_norm)
#donne une matrice avec 2 colonnes:
#1ère avec des randoms N(0,1)
#2ème avec des randoms N(50,1)

test5<- array(
		sapply(moyenne,n=12,sd=1,rnorm),
		dim=c(4,3,2),
		dimnames=c("n","B","length theta")
		)
test5

#tester si le sens de répartition est encore valable
#différence à test4: input était un vecteur, à test5 c'est une matrice

#-------------------------------TEST 51---------------------------------

test51<- array(
		rnorm(n=12,moyenne,sd=1),
		dim=c(4,3,2),
		dimnames=c("n","B","length theta")
		)
test51
#attention!!! la répartition n'est pas correcte!!!

#-------------------------------TEST 6---------------------------------

#créer la matrice input pour tester
matrix(data=1:24,nrow=12,ncol=2,byrow=FALSE)

test6<- array(
		matrix(data=1:20,nrow=12,ncol=2,byrow=FALSE),
		dim=c(4,3,2),
		dimnames=c("n","B","length theta")
		)
test6
#effectivement,sens de répartition reste pareil

#-------------------------------TEST 7---------------------------------
theta_list_test <- c(2,3,4)
simu_beta_test<- array(
		sapply(theta_list_test,n=2*3,shape2=2,rbeta),
		dim=c(2,3,length(theta_list_test)),
		dimnames=c("n","B","theta")
		)
simu_beta_test
#accès aux sous-matrices=simu d'un theta, p.ex. dernier theta:
#simu_beta_test[,,21]

#-------------------------------TEST 8---------------------------------

simu_beta_test2<- array(
		rbeta(n=2*3,shape2=2,theta_list_test),
		dim=c(2,3,length(theta_list_test)),
		dimnames=c("n","B","theta")
		)
simu_beta_test2


#-------------------------------vraie SIMU-----------------------------

#vraie simulation, avec une taille de l'éch n=20, pour B=500 éch
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
eins[1,,2]	#affiche la 1ère ligne du 2ème sous-tableau
eins[,3,2]	#affiche la 3ème colonne du 2ème sous-tableau


#dim(output)=nb_subtables, nb_col ,nb_lin
