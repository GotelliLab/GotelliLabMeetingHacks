## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,message=FALSE,warning=FALSE)

## ------------------------------------------------------------------------
library(ggplot2)
library(popbio)
library(gtools)

## ------------------------------------------------------------------------
Success <- 1
Failure <- 5

# the beta needs shape1 and shape2, which represent the number of successes + 1 and the number of failures + 1

betaSim1 <- rbeta(n=10000,shape1=Success+1,shape2=Failure+1)
qplot(x=betaSim1,color=I("black"),fill=I("goldenrod"),xlim=c(0,1))

## ------------------------------------------------------------------------
Success <- 10
Failure <- 50

# the beta needs shape1 and shape2, which represent the number of successes + 1 and the number of failures + 1

betaSim1 <- rbeta(n=10000,shape1=Success+1,shape2=Failure+1)
qplot(x=betaSim1,color=I("black"),fill=I("goldenrod"),xlim=c(0,1))

## ------------------------------------------------------------------------
##################################################
# function: dirichletSampler
# Creates transition matrix with sampling uncertainty
# input: matrix of raw transition counts
# output: matrix of transition probabilities sampled from a Dirichlet for each column
# note: add 1 to each matrix element for proper interpretation
# of rdirichlet paramaters as param = number of successes + 1
#------------------------------------------------- 
dirichletSampler <- function(m=matrix(rpois(n=16,lambda=5),nrow=4)) {
  z <- apply(m+1,2,rdirichlet,n=1) 
  
  return(list(data=m,trans=z))
}
#------------------------------------------------- 
dirichletSampler() # sample output of a transition matrix

## ------------------------------------------------------------------------

stages <- c("Empty",paste("Species",LETTERS[1:3]))
# use the Duke Forest heated chamber counts
A <- matrix(c(245,14,8,0,
              18,17,0,2,
              8,1,0,5,
              1,0,0,1) ,nrow=length(stages),byrow=TRUE,dimnames=list(stages,stages)) 

print(A)

## ------------------------------------------------------------------------
scaledA <- apply(A,2, function(x) x/sum(x)) 
print(scaledA)

## ------------------------------------------------------------------------

damping.ratio(scaledA)

## ------------------------------------------------------------------------
sampleMatrix <- dirichletSampler(A)
print(sampleMatrix)

## ------------------------------------------------------------------------
damping.ratio(sampleMatrix$trans)

## ------------------------------------------------------------------------

simDat <- replicate(1000,damping.ratio(dirichletSampler(A)$trans))
qplot(x=simDat,col=I("black"),fill=I("goldenrod")) +
  geom_vline(xintercept=damping.ratio(scaledA),color="red",size=1.3) +
  geom_vline(xintercept=quantile(simDat,probs=c(0.025,0.975)),linetype="dashed")


