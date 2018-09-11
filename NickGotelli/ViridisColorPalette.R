## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(ggplot2)
library(TeachingDemos)
char2seed("Ozark")

# first create the data frame
dFrame <- expand.grid(xVar=1:100,yVar=1:100)
dFrame$z <- rnorm(10000)

## ------------------------------------------------------------------------
p1 <- ggplot(data=dFrame,aes(x=xVar,y=yVar)) +
      geom_tile(aes(fill=z))
print(p1)

## ------------------------------------------------------------------------

p2 <- ggplot(data=dFrame,aes(x=xVar,y=yVar)) +
      geom_tile(aes(fill=z)) + 
scale_fill_viridis_c()
print(p2)

## ---- eval=FALSE---------------------------------------------------------
## library(colorblindr) # devtools::install_github("clauswilke/colorblindr")
## library(colorspace) # install.packages("colorspace", repos = "http://R-Forge.R-project.org") --- colorblindr requires the development version
## # this also installs cowplot
## library(cowplot)
## p3 <- ggplot(data=dFrame,aes(x=xVar,y=yVar)) +
##     geom_tile(aes(fill=z)) +
##     scale_fill_viridis_c()
## p3des<-edit_colors(p3, desaturate)
## ggdraw(p3des)

## ---- echo=FALSE, warning=FALSE, results='hide',message=FALSE------------
library(colorblindr) # devtools::install_github("clauswilke/colorblindr")
library(colorspace) # install.packages("colorspace", repos = "http://R-Forge.R-project.org") --- colorblindr requires the development version
# this also installs cowplot 
library(cowplot)
p3 <- ggplot(data=dFrame,aes(x=xVar,y=yVar)) +
    geom_tile(aes(fill=z)) + 
    scale_fill_viridis_c() 
p3des<-edit_colors(p3, desaturate)
ggdraw(p3des)

## ------------------------------------------------------------------------
p4 <- ggplot(data=dFrame,aes(x=xVar,y=yVar)) +
      geom_tile(aes(fill=z))  
p4 + scale_fill_viridis_c(option="magma") # "A" option
p4 + scale_fill_viridis_c(option="inferno") # "B" option
p4 + scale_fill_viridis_c(option="plasma") # "C" option
p4 + scale_fill_viridis_c(option="viridis") # "D" (default) option
p4 + scale_fill_viridis_c(option="cividis") # "E" option




## ------------------------------------------------------------------------

p5 <- ggplot(data=mpg,aes(x=cty,y=hwy,color=as.factor(cyl))) +
geom_point() +
geom_jitter() +
scale_color_viridis_d(option="C")
print(p5)

