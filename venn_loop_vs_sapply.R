# originally from http://research.stowers-institute.org/efg/R/Math/VennDiagram.htm
require(limma)

getTrue <- function(universe, set) {
  sapply(intersect(universe, set), FUN=function(x){ which(universe==x) })
}

Venn3vec <- function(set1, set2, set3, setnames, title)
{
  stopifnot( length(setnames) == 3)
  universe <- sort( unique( c(set1, set2, set3) ) )
  size <- length(universe)
  Counts <- matrix(NA, nrow=size, ncol=3)
  colnames(Counts) <- setnames
  
  trueSet1 <- getTrue(universe, set1)
  Counts[trueSet1,1] <- TRUE
  Counts[-trueSet1,1] <- FALSE
  
  trueSet2 <- getTrue(universe, set2)
  Counts[trueSet2,1] <- TRUE
  Counts[-trueSet2,1] <- FALSE
  
  trueSet3 <- getTrue(universe, set3)
  Counts[trueSet3,3] <- TRUE
  Counts[-trueSet3,3] <- FALSE
  
  vc <- vennCounts(Counts)
  vc[1,] <- rep(NA,4)
  vennDiagram(vc, circle.col=c("orange1", "blue3"), mar=rep(0,4), cex=c(3.5,3,2))
  text(0, 3, title, cex=4)
}


# for loop
Venn3loop <- function(set1, set2, set3, setnames, title) 
{
  stopifnot( length(setnames) == 3)
  universe <- sort( unique( c(set1, set2, set3) ) )
  size <- length(universe)
  Counts <- matrix(NA, nrow=size, ncol=3)
  colnames(Counts) <- setnames
  for (i in 1:size) {
    Counts[i,1] <- universe[i] %in% set1
    Counts[i,2] <- universe[i] %in% set2
    Counts[i,3] <- universe[i] %in% set3
  }
  vc <- vennCounts(Counts)
  vc[1,] <- rep(NA,4)
  vennDiagram(vc, circle.col=c("orange1", "blue3"), mar=rep(0,4), cex=c(3.5,3,2))
  text(0, 3, title, cex=4)
} 


set1 <- 1:10000
set2 <- 10:10009
set3 <- 100:10099

system.time(
  Venn3vec(set1, set2, set3, c("set1", "set2", "set3"), "timing test")
)
#  user  system elapsed 
# 1.395   0.279   1.673 


system.time(
  Venn3loop(set1, set2, set3, c("set1", "set2", "set3"), "timing test")
)
#   user  system elapsed 
# 10.058   1.434  11.500