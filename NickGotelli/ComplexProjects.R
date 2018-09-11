## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval=FALSE---------------------------------------------------------
## read.table("filename.csv")
## write.table("output.csv")

## ---- eval=FALSE---------------------------------------------------------
## read.table("pathname/filename")
## read.table("folder1/filename") # go down into folder1
## read.table("folder1/subfolderB/filename") # go down 2 levels to subfolderB

## ---- eval=FALSE---------------------------------------------------------
## read.table("../filename") # this moves up one level
## read.table("../../filename") # this moves up two levels
## read.table("../../folderC/filename") # this moves up 2 levels and then into folderC

## ----eval=TRUE-----------------------------------------------------------
z <- runif(10000)
saveRDS(z,"myRandomVariates")
y <- readRDS("myRandomVariates")
identical(z,y)


## ---- eval=FALSE---------------------------------------------------------
## library(tictoc)
## tic()
## print("executed..")
## Sys.sleep(1)
## toc()

## ---- echo=FALSE---------------------------------------------------------
library(tictoc)
tic()
print("executed..")
Sys.sleep(1)
toc()

## ---- eval=FALSE---------------------------------------------------------
## tic("Step #1")
## Sys.sleep(2)
## toc()
## tic("Step #2")
## Sys.sleep(3)
## toc()

## ---- echo=FALSE---------------------------------------------------------
tic("Step #1")
Sys.sleep(2)
toc()
tic("Step #2")
Sys.sleep(3)
toc()

## ---- eval=FALSE---------------------------------------------------------
## 
## tic("outer")
##    Sys.sleep(1)
##    tic("middle")
##       Sys.sleep(2)
##       tic("inner")
##          Sys.sleep(3)
##       toc()
##       toc()
##       toc()

## ---- echo=FALSE---------------------------------------------------------

tic("outer")
   Sys.sleep(1)
   tic("middle")
      Sys.sleep(2)
      tic("inner")
         Sys.sleep(3)
      toc()
      toc()
      toc()

## ---- warning=FALSE, results='hide',message='hide', eval=FALSE-----------
## library(R.utils)
## total <- 20
## # create progress bar
## pb <- txtProgressBar(min = 0, max = total, style = 3,char=".")
## for(i in 1:total){
##    Sys.sleep(0.1)
##    if(i %% 5 ==0) print("print to console")
##    # update progress bar
##    setTxtProgressBar(pb, i)
## }
## close(pb)

## ---- warning=FALSE, message='hide',echo=FALSE---------------------------
total <- 20
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3,char=".")
for(i in 1:total){
   Sys.sleep(0.1)
   if(i %% 5 ==0) print("print to console")
   # update progress bar
   setTxtProgressBar(pb, i)
}
close(pb)

