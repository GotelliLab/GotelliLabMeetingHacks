## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,message=FALSE)

## ------------------------------------------------------------------------
set.seed(02252018)
runif(1)

## ------------------------------------------------------------------------
# install TeachingDemos package from CRAN
library(TeachingDemos)
z <- char2seed("espresso",set=FALSE)
z
set.seed(z)
runif(1)

## ------------------------------------------------------------------------
char2seed("chai latte with soy")
runif(1)

## ------------------------------------------------------------------------
dirtyVec <- c(3.2, NA, 2.2, 6, 5)
complete.cases(dirtyVec)

# use this to select the NA values (inverse)
!complete.cases(dirtyVec)

## ---- eval=FALSE---------------------------------------------------------
## # removes all rows with 1 or more NAs
## cleanFrame <- dirtyFrame[complete.cases(dirtyFrame),]
## 

## ---- eval=FALSE---------------------------------------------------------
## # remove rows with NAs only in column 5 or column 6
## cleanFrame <- dirtyFrame[complete.cases(dirtyFrame[,(5:6)]),]
## 

## ---- message=FALSE------------------------------------------------------

# Using a vector of lists to store ggplot objects in a loop
# February 19, 2018
library(ggplot2)
library(patchwork)
n <- 3 # number of plots to create
plotList <- vector("list",n) # taken from a StackOverflow answer from Venables
# Now run a little for loop to create, plot, and store 3 replicates
for(i in seq(plotList)) {
  local({                     # special call to local
       i <- i                 # special re-assignment of counter 
  z <- rnorm(100)
  plotList[[i]] <<- qplot(x=z,color=I("black"),fill=I("goldenrod"))
  }) # note global assign <<- and environment closure })
}


# now plot these with patchwork
plotList[[1]] | plotList[[2]] | plotList[[3]]


