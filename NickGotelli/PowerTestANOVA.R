## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
# set.seed <- 57   #for repeatable results
library(ggplot2)

RanN <- 1000     # number of random data sets to create

MeanA <- 100 # mean of treatment group A
sdA <- 15    # standard deviation of treatment group A
RepA <- 15   # number of replicates of treatment group A

MeanB <- 100
sdB <- 15
RepB <- 15

MeanC <- 100
sdC <- 15
RepC <- 15

ParamList <- list(RanN=RanN,
                  MeanA=MeanA,sdA=sdA,RepA=RepA,
                  MeanB=MeanB,sdB=sdB,RepB=RepB,
                  MeanC=MeanC,sdC=sdC,RepC=RepC)


## ------------------------------------------------------------------------
##################################################
# function: DataGen
# create a single data frame
# input: parameter list 
# output: 2-column dataframe, with factor Treatment and response
#------------------------------------------------- 
DataGen <- function(ParamList) {
Trt1 <- rnorm(n=RepA,mean=MeanA,sd=sdA)
Trt2 <- rnorm(n=RepB,mean=MeanB,sd=sdB)
Trt3 <- rnorm(n=RepC,mean=MeanC,sd=sdC)
Trt <- c(rep("A",RepA),rep("B",RepB),rep("C",RepC))
z <- data.frame(Trt,Response=c(Trt1,Trt2,Trt3))
return(z)

}
##################################################


## ------------------------------------------------------------------------

##################################################
# function: GetP
# Extract p-value from one-way ANOVA
# input: one-way ANOVA data frame 
# output: p-value from parametric test of group mean differences
#------------------------------------------------- 
GetP <- function(z) {
myModel <- aov(z$Response~z$Trt)  
pVal <- summary(myModel)[[1]][1,5]
return(pVal)
}

## ------------------------------------------------------------------------
##################################################
# function: AnovaPlot
# Creates box plots with data overlays for ANOVA data
# input: 2-column long-form data frame with 1-way layout
# output: ggplot graph
#------------------------------------------------- 
AnovaPlot <- function(x) {
  
 p <- format(GetP(x),digits=3)
Fig1 <- ggplot(data=x,aes(x=Trt,y=Response,fill=Trt)) + 
  xlab("Treatment") +
    theme_gray(base_size=20)
Fig1 + geom_violin() + 
  geom_point() +
  stat_summary(fun.y=mean, geom="point",fill="white",shape=21,size=2.5) +
   annotate("text",label=p,parse=TRUE,x=Inf,y=Inf, hjust=2.1,vjust=3.5)
}
##################################################

## ------------------------------------------------------------------------

##################################################
# function: SimGen
# Simulate multiple runs of data generator and extract ANOVA pvalue for each data set
# input: parameter list 
# output: vector of p values
#------------------------------------------------- 
SimGen <- function(ParamList) {
pVec <- replicate(n=ParamList$RanN,expr=GetP(DataGen(ParamList)))

return(pVec)
}

## ------------------------------------------------------------------------

##################################################
# function: PowerTester
# calculates one-way ANOVA power for random data sets with user-defined parameters
# input: pVec, a vector of p-values from simulated data sets
# output: a histogram of p values plus the power (= proportion of replicates <= 0.05)

#------------------------------------------------- 
PowerTester <- function(v) {
 Power <- format(mean(v<=0.05),digits=3)
 CutLevels <- ifelse(v<=0.05,"Sig","Non-Sig")
 PData <- data.frame(obs=1:length(v),Pvalue=v,CutLevels)
 Fig1 <- ggplot(data=PData,aes(x=Pvalue,fill=CutLevels)) +
    theme_gray(base_size=20)
Fig1 + geom_histogram(bins=40,col=I("black")) +
                      scale_fill_manual(values=c("goldenrod","brown")) + 
                      labs(x = "P-value", y = "Frequency") + xlim(0,1) +
  annotate("text",label=Power,parse=TRUE,x=Inf,y=Inf, hjust=2.1,vjust=3.5)


}

# Typical use of code to see a sample plot and then run a replicated analysis

AnovaPlot(DataGen(ParamList))  # run a single data set and display data
PowerTester(SimGen(ParamList)) # run the power test with replication

