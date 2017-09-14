## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
# set.seed <- 57   # for repeatable results
library(ggplot2)

RanN <- 1000     # number of random data sets to create

MeanA <- 100 # mean of treatment group A
sdA <- 20    # standard deviation of treatment group A
RepA <- 15   # number of replicates of treatment group A

MeanB <- 100
sdB <- 20
RepB <- 15

MeanC <- 100
sdC <- 20
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
x <- data.frame(Trt,Response=c(Trt1,Trt2,Trt3))
return(x)

}
##################################################


## ------------------------------------------------------------------------
##################################################
# function: getP
# extract p-value from ANOVA
# input: data frame
# output: p-value for ANOVA
#------------------------------------------------- 
getP <- function(dframe) {
pVal <- unlist(summary(aov(dframe$Response~dframe$Trt)))["Pr(>F)1"]
return(pVal)
}
##################################################

## ------------------------------------------------------------------------
##################################################
# function: AnovaPlot
# Creates box plots with data overlays for ANOVA data
# input: 2-column long-form data frame with 1-way layout
# output: ggplot graph
#------------------------------------------------- 
AnovaPlot <- function(z) {
  # pull out p value from model summary
  pVal <- getP(z)
  # create an expression to add to the graph
eqn <- as.character(as.expression(
       substitute(italic(p) == pCon,
                  list(pCon= format(pVal,digits=3)))))
  
  
AnovaFig <- ggplot(data=z, aes(x=Trt,y=Response,fill=Trt))
AnovaFig +  
  geom_violin() +
  geom_point() + 
  theme_gray(base_size = 20) +
  xlab("Treatment") +
  stat_summary(fun.y=mean,geom="point",fill="white",shape=23,size=2.5) +
  annotate("text",label=eqn,parse=TRUE,x=-Inf,y=Inf,hjust=-0.5,vjust=1.5)

}
##################################################


## ------------------------------------------------------------------------
##################################################
# function: SimEngine
# Simulates multiple runs of DataGen and retains p value from each
# input: ParamList, which contains the number of simulations RanN
# output: Vector of p values
#------------------------------------------------- 
SimEngine <- function(z) {

pVec <- replicate(n=z$RanN,expr=getP(DataGen(z)))  

return(pVec)
}
##################################################

## ------------------------------------------------------------------------

##################################################
# function: PowerHistogram
# Plots distribution of p values from simulated data
# input: vector of p values
# output: two-color histogram and calculation of overall power
#------------------------------------------------- 
PowerHistogram <- function(v) {

# create data frame including color split at p <= 0.05
  sigP <- ifelse(v<=0.05,"Sig","NonSig")
  plotData <- data.frame(pVal=v,sig=sigP)
  
  # pull out p value from model summary
  Powerx <- mean(plotData$pVal<=0.05)
    # create an expression to add to the graph
eqn <- as.character(as.expression(
       substitute(Power == Powerx,
                  list(Powerx = format(Powerx,digits=3)))))
  
  PowerPlot <- ggplot(data=plotData,aes(x=pVal,fill=sig))
  PowerPlot + geom_histogram(bins=40,color="black") +
  theme_gray(base_size=20) + 
  xlab("P value") +
  ylab("Frequency") +
  scale_fill_manual(values=c("goldenrod","brown")) +
  xlim(0,1) +
  annotate("text",label=eqn,parse=TRUE,x=Inf,y=Inf,hjust=1.5,vjust=1.5) +
  guides(fill=FALSE)
  
}
##################################################

## ---- warning=FALSE------------------------------------------------------
AnovaPlot(DataGen(ParamList))        # Depict sample data set
PowerHistogram(SimEngine(ParamList)) # Replicate model, conduct power test, plot results

