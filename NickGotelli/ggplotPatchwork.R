## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,message=FALSE)

## ---- eval=FALSE---------------------------------------------------------
## # install.packages("devtools")
## library(devtools)
## install_github("thomasp85/patchwork")

## ------------------------------------------------------------------------
library(ggplot2)
library(patchwork)

d1 <- runif(500)
d2 <- rep(c("Treatment","Control"),each=250)
d3 <- rbeta(500,shape1=100,shape2=3)
d4 <- d3 + rnorm(500,mean=0,sd=0.1)
plotData <- data.frame(d1,d2,d3,d4)
str(plotData)

## ------------------------------------------------------------------------
p1 <- ggplot(data=plotData) + geom_point(aes(x=d3, y=d4))
p1

p2 <- ggplot(data=plotData) + geom_boxplot(aes(x=d2,y=d1,fill=d2))+
theme(legend.position="none")
p2

p3 <- ggplot(data=plotData) +
  geom_histogram(aes(x=d1, color=I("black"),fill=I("orchid")))
p3

p4 <- ggplot(data=plotData) +
  geom_histogram(aes(x=d3, color=I("black"),fill=I("goldenrod")))
p4

## ------------------------------------------------------------------------
p1 + p2

## ------------------------------------------------------------------------
p1 + p2 + p3 + plot_layout(ncol=1)

## ------------------------------------------------------------------------
p1 + p2 + plot_layout(ncol=1,heights=c(2,1))

p1 + p2 + plot_layout(ncol=2,widths=c(2,1))

## ------------------------------------------------------------------------
# not working correctly!
p1 + plot_spacer() + p2

## ------------------------------------------------------------------------
p1 + {
  p2 + {
    p3 +
      p4 +
      plot_layout(ncol=1)
  }
} +
  plot_layout(ncol=1)

## ------------------------------------------------------------------------
p1 + p2 - p3 + plot_layout(ncol=1)

## ------------------------------------------------------------------------
(p1 | p2 | p3)/p4

(p1 | p2)/(p3 | p4)

(p1 / p2 /p3) | p4

