library(ggplot2)
library(dplyr)
tspr<-read.csv(file="DateTerRich.csv",header=TRUE)
#summarize data frame by date and fg, and calculating mean of terrestrial sp rich and sd
tspr1<-ddply(tspr,.(Date, FG), summarize, rich=mean(TerrestrialSpRich), se=sd(TerrestrialSpRich)/sqrt(length(TerrestrialSpRich)))
tspr1$FG<-as.factor(tspr1$FG)
tspr1$id<-row.names(tspr1)
tspr$id<-row.names(tspr)
# Line plot with multiple groups
ggplot(data=tspr1, aes(x=tspr1$Date, y=rich, group=tspr1$FG)) +
	geom_line()+
	geom_point()
# Change line types
ggplot(data=tspr1, aes(x=tspr1$Date, y=rich, group=tspr1$FG)) +
	geom_line(linetype="dashed")+
	geom_point()
# Change line colors and sizes
ggplot(data=tspr1, aes(x=tspr1$Date, y=rich, group=tspr1$FG)) +
	geom_line(linetype="dotted", color="black", size=1)+
	geom_point(color="black", size=2)
# Change line types by groups 
ggplot(tspr1, aes(x=tspr1$Date, y=rich, group=FG)) +
	geom_errorbar(aes(ymin=rich-se, ymax=rich+se), width=.1)+
	geom_line(aes(linetype=tspr1$FG))+
	geom_point()+
	theme(legend.position="top")+
  labs(x="Sampling Date",y="Terrestrial Species Richness", linetype="Number of Functional Groups")
# Change line types + colors
ggplot(tspr1, aes(x=tspr1$Date, y=rich, group=tspr1$FG)) +
	geom_errorbar(aes(ymin=rich-se, ymax=rich+se), width=.1)+
	geom_line(aes(color=tspr1$FG))+
	geom_point(aes(color=tspr1$FG))+
	theme(legend.position="top")+
  labs(x="Sampling Date",y="Terrestrial Species Richness", col="Number of Functional Groups")
#ANOVA
lmod<-aov(data=tspr,TerrestrialSpRich~FG*Date+Error(1/id))
summary(lmod)

