### likelihood of binomial distribution
# X ~ B(n,p)


# blog http://tdunning.blogspot.de/2008/03/surprise-and-coincidence.html
# that is how it is implemented in Java 

k <- matrix(c(3,0,1,3), nrow=2)
k <- matrix(c(3,0,0,3), nrow=2)

H <- function(k) {
  N <- sum(k)
  res <- sum(k/N * log(k/N + (k==0)))
  return(res)
}

LLR <- function(k) {
  fac1 <- 2*sum(k)
  fac2 <- H(k) - H(apply(k,1,sum)) - H(apply(k,2,sum))
  return(fac1*fac2)
} 

llr <- LLR(k)
1 - 1/(1+llr)

# paper http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.14.5962
L <- function(p,x,n) {
  return(p^x * (1-p)^(n-x))
}

binCoef <- function(n,k) {
  nom <- factorial(n)
  denom <- factorial(k) * factorial(n-k)
  return(nom/denom)
}  

L(p2,x2,n2)
dbinom(x2, n2, p2) / binCoef(n2,x2)


LLR2 <- function(x1,x2,n1,n2) {
  p1 <- x1/n1
  p2  <- x2/n2
  p <- (x1+x2)/(n1+n2)
  res <- log(L(p1,x1,n1)) + log(L(p2,x2,n2)) - log(L(p,x1,n1)) - log(L(p,x2,n2))
  return(2*res)
}

llr <- LLR2(3,3,7,7)
1 - 1/(1+llr)
