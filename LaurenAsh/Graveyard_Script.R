<<<<<<< Updated upstream

library(survival)
library(ggplot2)
# Graveyard Script

survival <-read.csv(file ="Graveyard_Survival.csv")

birth <-read.csv(file ="Graveyard_Birth.csv")

head(survival)

head(birth)

survival<-subset(survival, !survival$GroupID=="10")
attach(survival)
attach(birth)
#Age_of_death

#Death_Age <- as.numeric(Death_Year-Birth_Year )
survival$Death_Age <- as.numeric(Death_Year-Birth_Year )
survival$event<-as.logical(rep("TRUE",length(survival$Death_Age)))
#survival$event<-rep("1",length(survival$Death_Age))
survival$century<-as.factor(ifelse(survival$Birth_Year<1900,"19th","20th"))

bb<-Surv(survival$Death_Age, survival$event,type="right")
plot(survfit(bb~Sex+century,data=survival))
survdiff(bb~Sex,data=survival)
survdiff(bb~century, data=survival)
coxph(bb~Sex+century,data=survival)
#m<-Surv(survival$Birth_Year,survival$Death_Year,survival$event,type="mstate")
#survdiff(m~Sex*century,data=survival)


Death_Age <- Death_Year-Birth_Year 
Ages<- min(birth$Age):max(birth$Age) 
Death_Age <- factor(Death_Age,levels=Ages) 
Death_Age_19 <- Death_Age[ Birth_Year < 1900 ]
Death_Age_20 <- Death_Age[ Birth_Year >= 1900 ] 
Death_Age_19M <- Death_Age[Birth_Year<1900&Sex=="M"]
Death_Age_19F <- Death_Age[Birth_Year<1900&Sex=="F"]
Death_Age_20M <- Death_Age[Birth_Year>=1900&Sex=="M"]
Death_Age_20F <- Death_Age[Birth_Year>=1900&Sex=="F"]

#Sex plot set up
Death_Age_M<-Death_Age[Sex=="M"]
Death_Age_F<-Death_Age[Sex=="F"]
S0_M<-sum(Sex=="M")
S0_F<-sum(Sex=="F")
Deaths_M<-table(Death_Age_M)
Deaths_F<-table(Death_Age_F)
Sx_M <- c(S0_M, S0_M - cumsum(Deaths_M[-length(Deaths_M)])) 
Sx_F <- c(S0_F , S0_F - cumsum(Deaths_F[-length(Deaths_F)]))
lx_M = Sx_M/S0_M
lx_F = Sx_F/S0_F

plot(lx_F~Ages,
     type="l",
     col="hotpink",
     lwd=2,
     lty=2,
     xlab="Age",
     ylab="ln [ l(x) ]",
     log='y')

points(lx_M~Ages,
       type="l",
       col="blue",
       lwd=2,
       lty=2)

## Calculate Cohort Survival (Sx)

S0_19<-sum(Birth_Year<1900)
  
S0_20<-sum(Birth_Year>=1900)
S0_19M <- sum(Birth_Year<1900&Sex=="M")
S0_19F <- sum(Birth_Year<1900&Sex=="F")
S0_20M <- sum(Birth_Year>=1900&Sex=="M")
S0_20F <- sum(Birth_Year>=1900&Sex=="F")

#####life span)

Deaths_19 <- table(Death_Age_19)
Deaths_20 <- table(Death_Age_20)
Deaths_19M<-table(Death_Age_19M)
Deaths_19F<-table(Death_Age_19F)
Deaths_20M<-table(Death_Age_20M)
Deaths_20F<-table(Death_Age_20F)

#### Now we can make the cohort survival tables

Sx_19 <- c(S0_19, S0_19 - cumsum(Deaths_19[-length(Deaths_19)])) 
Sx_20 <- c(S0_20 , S0_20 - cumsum(Deaths_20[-length(Deaths_20)])) 
Sx_19M <- c(S0_19M, S0_19M - cumsum(Deaths_19M[-length(Deaths_19M)]))
Sx_19F <- c(S0_19F, S0_19F - cumsum(Deaths_19F[-length(Deaths_19F)]))
Sx_20M <- c(S0_20M, S0_20M - cumsum(Deaths_20M[-length(Deaths_20M)]))
Sx_20F <- c(S0_20F, S0_20F - cumsum(Deaths_20F[-length(Deaths_20F)]))

# Survivorship Schedule (lx)
lx_19 = Sx_19/S0_19
lx_20 = Sx_20/S0_20
lx_19M = Sx_19M/S0_19M
lx_19F = Sx_19F/S0_19F
lx_20M = Sx_20M/S0_20M
lx_20F = Sx_20F/S0_20F

lxvalues<-data.frame(c(lx_19M, lx_19F, lx_20M, lx_20F))
lxvalues$cat<-c(rep("lx_19M",length(lx_19M)),rep("lx_19F",length(lx_19F)),rep("lx_20M",length(lx_20M)),rep("lx_20F",length(lx_20F)))
names(lxvalues)[1]<-c("surv")
lxvalues$time<-rep(rep(0:100,1),4)
lxvalues<-data.frame(lxvalues)


cc1<-glm(surv~cat*time,family = "binomial",data=lxvalues)
summary(cc1)
lxvalues$predict<-predict(cc1,type="response")

ggplot(data=lxvalues,aes(x=time,y=surv,colour=cat))+geom_line()+geom_line(aes(y=predict,x=time))


summary(lm(surv~cat*time,data=lxvalues))
summary(aov(surv~cat*time,data=lxvalues))
plot(aov(surv~cat*time,data=lxvalues))


ggplot(data=lxvalues,aes(y=surv,x=time,colour=factor(cat)))+geom_()

# Survival probability

gx_19 = lx_19 [-1] / lx_19 [-length(lx_19)]
gx_20 = lx_20 [-1] / lx_20 [-length(lx_20)]

gx_19M = lx_19M [-1] / lx_19M [-length(lx_19M)]
gx_19F = lx_19F [-1] / lx_19F [-length(lx_19F)]

gx_20M = lx_20M [-1] / lx_20M [-length(lx_20M)]
gx_20F = lx_20F [-1] / lx_20F [-length(lx_20F)]
gx_20F


# Calculate l(x)b(x)


lxbx_19 = lx_19*bx_19
lxbx_20 = lx_20*bx_20

lxbx_19M = lx_19M*bx_19
lxbx_19F = lx_19F*bx_19

lxbx_20M = lx_20M*bx_20
lxbx_20F = lx_20F*bx_20


# Calculate l(x)b(x)x


lxbxx_19 = lxbx_19*Ages
lxbxx_20 = lxbx_20*Ages

lxbxx_19M = lxbx_19M*Ages
lxbxx_19F = lxbx_19F*Ages

lxbxx_20M = lxbx_20M*Ages
lxbxx_20F = lxbx_20F*Ages

# Calculate R0


R0_19 = sum(lxbx_19,na.rm = T)
R0_20 = sum(lxbx_20,na.rm = T)

R0_19M = sum(lxbx_19M,na.rm = T)
R0_19F = sum(lxbx_19F,na.rm = T)

R0_20M = sum(lxbx_20M,na.rm = T)
R0_20F = sum(lxbx_20F,na.rm = T)


R0_19
R0_20
R0_19M
R0_19F
R0_20M
R0_19F


# Calculate G(x)


G_19 = sum(lxbxx_19,na.rm = T)/R0_19
G_20 = sum(lxbxx_20,na.rm = T)/R0_20

G_19M = sum(lxbxx_19M,na.rm = T)/R0_19M
G_19F = sum(lxbxx_19F,na.rm = T)/R0_19F

G_20M = sum(lxbxx_20M,na.rm = T)/R0_20M
G_20F = sum(lxbxx_20F,na.rm = T)/R0_20F


G_19
G_20
G_19M
G_19F
G_20M
G_20F



# Estimate r
r_19 = log(R0_19)/G_19
r_20 = log(R0_20)/G_20

r_19M = log(R0_19M)/G_19M
r_19F = log(R0_19F)/G_19F

r_20M = log(R0_20M)/G_20M
r_20F = log(R0_20F)/G_20F

####### Making tables: they will display in a different source panel

# 19th Summary
CenturySummary19 = cbind(R0_19,G_19,r_19)
CenturySummary19F= cbind(R0_19F,G_19F,r_19F)
CenturySummary19M= cbind(R0_19M,G_19M,r_19M)
CenturySummaryTable19 = rbind(CenturySummary19, CenturySummary19F, CenturySummary19M)
rownames(CenturySummaryTable19) = c("Male + Female","Female", "Male")
colnames(CenturySummaryTable19) = c("R0_19th","G_19th","r_19th")
View(CenturySummaryTable19)

# 20th Summary
CenturySummary20 = cbind(R0_20,G_20,r_20)
CenturySummary20F= cbind(R0_20F,G_20F,r_20F)
CenturySummary20M= cbind(R0_20M,G_20M,r_20M)
CenturySummaryTable20 = rbind(CenturySummary20, CenturySummary20F, CenturySummary20M)
rownames(CenturySummaryTable20) = c("Male + Female","Female", "Male")
colnames(CenturySummaryTable20) = c("R0_20th","G_20th","r_20th")
View(CenturySummaryTable20)

# Need to attach package PerformanceAnalytics if you want a table in the plot panel
require(PerformanceAnalytics)
PerformanceAnalytics:::textplot(CenturySummaryTable19,
                                halign = "center", 
                                valign = "center",
                                max.cex = 2, 
                                cmar = 3, 
                                rmar = 3, 
                                hadj = 1, 
                                vadj = 1, 
                                row.valign = "center",
                                heading.valign = "center", 
                                mar = c(0, 0, 0, 0) + 0.5, 
                                col.data = par("col"),
                                col.rownames = par("col"), 
                                col.colnames = par("col"), 
                                wrap = TRUE,
                                wrap.colnames = 20, 
                                wrap.rownames = 20)


PerformanceAnalytics:::textplot(CenturySummaryTable20,
                                halign = "center", 
                                valign = "center",
                                max.cex = 2, 
                                cmar = 3, 
                                rmar = 3, 
                                hadj = 1, 
                                vadj = 1, 
                                row.valign = "center",
                                heading.valign = "center", 
                                mar = c(0, 0, 0, 0) + 0.5, 
                                col.data = par("col"),
                                col.rownames = par("col"), 
                                col.colnames = par("col"), 
                                wrap = TRUE,
                                wrap.colnames = 20, 
                                wrap.rownames = 20)



plot(lx_19F~Ages,
     type="l",
     col="hotpink",
     lwd=2,
     lty=2,
     xlab="Age",
     ylab="ln [ l(x) ]",
     log = "y")
     
points(lx_19M~Ages,
       type="l",
       col="blue",
       lwd=2,
       lty=2)

points(lx_20M~Ages,
       type="l",
       col="blue",
       lwd=2)

points(lx_20F~Ages,type="l",
       col="hotpink",
       lwd=2)
legend("bottomleft",
       c("19th Century Male","19th Century Female",
         "20th Century Male","20th Century Female"),
       lty=c(2,2,1,1),
       lwd=2,
       col=c("blue","hotpink"))

## Survival plot between centuries
plot(lx_19~Ages,
     type="l",
     col="black",
     lwd=2,
     xlab="Age",
     ylab="ln [ l(x) ]",
     log = "y")

points(lx_20~Ages,
       type="l",
       col="red",
       lwd=2,
       lty=2)

### Testing things

lxvalues<-data.frame(c(lx_19M, lx_19F, lx_20M, lx_20F))
g3 <- subset(glioma, histology == "Grade3")
survdiff(Surv(time, event) ~ group, data = g3)

plot(survfit(Surv(time, event) ~ group, data = g3),
     main = "Grade III Glioma", lty = c(2, 1),
     ylab = "Probability", xlab = "Survival Time in Month")

## Testing other things
lxAll<-cbind(lx_19F, lx_19M, lx_20F, lx_20M)
class(lxAll)
lx_anova<-cbind(rep(0:99), lxAll[2:101,])
colnames(lx_anova)[1]<-"Time"
head(lx_anova)
# lmod<-aov(data=tspr,TerrestrialSpRich~FG*Date+Error(1/id))
notimelx<-lx_anova[,2:5]
library(reshape2)
longlx<-melt(notimelx, id=c("lx_19F", "lx_19M", "lx_20F", "lx_20M"))
names(longlx)<-c("Time", "YearSex","lx")
longlx$id<-row.names(longlx)
Rep_ANOVA_Graveyard<-aov(data=longlx, lx~YearSex*Time+Error(1/id))
summary(Rep_ANOVA_Graveyard)
#ANOVA_Graveyard<-aov(lx~YearSex*Time, data=longlx)
ANOVA_Graveyard<-aov(lx~YearSex, data=longlx)
TukeyHSD(ANOVA_Graveyard)
=======


# Graveyard Script

survival <-read.csv(file ="Graveyard_Survival.csv")

birth <-read.csv(file ="Graveyard_Birth.csv")

head(survival)

head(birth)

survival<-subset(survival, !survival$GroupID=="10")
attach(survival)
attach(birth)
#Age_of_death

#Death_Age <- as.numeric(Death_Year-Birth_Year )
survival$Death_Age <- as.numeric(Death_Year-Birth_Year )
survival$event<-as.logical(rep("TRUE",length(survival$Death_Age)))
#survival$event<-rep("1",length(survival$Death_Age))
survival$century<-as.factor(ifelse(survival$Birth_Year<1900,"19th","20th"))

bb<-Surv(survival$Death_Age, survival$event,type="right")
plot(survfit(bb~Sex+century,data=survival))
survdiff(bb~Sex,data=survival)
survdiff(bb~century, data=survival)
coxph(bb~Sex+century,data=survival)
#m<-Surv(survival$Birth_Year,survival$Death_Year,survival$event,type="mstate")
#survdiff(m~Sex*century,data=survival)


Death_Age <- Death_Year-Birth_Year 
Ages<- min(birth$Age):max(birth$Age) 
Death_Age <- factor(Death_Age,levels=Ages) 
Death_Age_19 <- Death_Age[ Birth_Year < 1900 ]
Death_Age_20 <- Death_Age[ Birth_Year >= 1900 ] 
Death_Age_19M <- Death_Age[Birth_Year<1900&Sex=="M"]
Death_Age_19F <- Death_Age[Birth_Year<1900&Sex=="F"]
Death_Age_20M <- Death_Age[Birth_Year>=1900&Sex=="M"]
Death_Age_20F <- Death_Age[Birth_Year>=1900&Sex=="F"]

#Sex plot set up
Death_Age_M<-Death_Age[Sex=="M"]
Death_Age_F<-Death_Age[Sex=="F"]
S0_M<-sum(Sex=="M")
S0_F<-sum(Sex=="F")
Deaths_M<-table(Death_Age_M)
Deaths_F<-table(Death_Age_F)
Sx_M <- c(S0_M, S0_M - cumsum(Deaths_M[-length(Deaths_M)])) 
Sx_F <- c(S0_F , S0_F - cumsum(Deaths_F[-length(Deaths_F)]))
lx_M = Sx_M/S0_M
lx_F = Sx_F/S0_F

plot(lx_F~Ages,
     type="l",
     col="hotpink",
     lwd=2,
     lty=2,
     xlab="Age",
     ylab="ln [ l(x) ]",
     log='y')

points(lx_M~Ages,
       type="l",
       col="blue",
       lwd=2,
       lty=2)

## Calculate Cohort Survival (Sx)

S0_19<-sum(Birth_Year<1900)
  
S0_20<-sum(Birth_Year>=1900)
S0_19M <- sum(Birth_Year<1900&Sex=="M")
S0_19F <- sum(Birth_Year<1900&Sex=="F")
S0_20M <- sum(Birth_Year>=1900&Sex=="M")
S0_20F <- sum(Birth_Year>=1900&Sex=="F")

#####life span)

Deaths_19 <- table(Death_Age_19)
Deaths_20 <- table(Death_Age_20)
Deaths_19M<-table(Death_Age_19M)
Deaths_19F<-table(Death_Age_19F)
Deaths_20M<-table(Death_Age_20M)
Deaths_20F<-table(Death_Age_20F)

#### Now we can make the cohort survival tables

Sx_19 <- c(S0_19, S0_19 - cumsum(Deaths_19[-length(Deaths_19)])) 
Sx_20 <- c(S0_20 , S0_20 - cumsum(Deaths_20[-length(Deaths_20)])) 
Sx_19M <- c(S0_19M, S0_19M - cumsum(Deaths_19M[-length(Deaths_19M)]))
Sx_19F <- c(S0_19F, S0_19F - cumsum(Deaths_19F[-length(Deaths_19F)]))
Sx_20M <- c(S0_20M, S0_20M - cumsum(Deaths_20M[-length(Deaths_20M)]))
Sx_20F <- c(S0_20F, S0_20F - cumsum(Deaths_20F[-length(Deaths_20F)]))

# Survivorship Schedule (lx)
lx_19 = Sx_19/S0_19
lx_20 = Sx_20/S0_20
lx_19M = Sx_19M/S0_19M
lx_19F = Sx_19F/S0_19F
lx_20M = Sx_20M/S0_20M
lx_20F = Sx_20F/S0_20F

lxvalues<-data.frame(c(lx_19M, lx_19F, lx_20M, lx_20F))
lxvalues$cat<-c(rep("lx_19M",length(lx_19M)),rep("lx_19F",length(lx_19F)),rep("lx_20M",length(lx_20M)),rep("lx_20F",length(lx_20F)))
names(lxvalues)[1]<-c("surv")
lxvalues$time<-rep(rep(0:100,1),4)
lxvalues<-data.frame(lxvalues)


cc1<-glm(surv~cat*time,family = "binomial",data=lxvalues)
summary(cc1)
lxvalues$predict<-predict(cc1,type="response")

ggplot(data=lxvalues,aes(x=time,y=surv,colour=cat))+geom_line()+geom_line(aes(y=predict,x=time))


summary(lm(surv~cat*time,data=lxvalues))
summary(aov(surv~cat*time,data=lxvalues))
plot(aov(surv~cat*time,data=lxvalues))


ggplot(data=lxvalues,aes(y=surv,x=time,colour=factor(cat)))+geom_()

# Survival probability

gx_19 = lx_19 [-1] / lx_19 [-length(lx_19)]
gx_20 = lx_20 [-1] / lx_20 [-length(lx_20)]

gx_19M = lx_19M [-1] / lx_19M [-length(lx_19M)]
gx_19F = lx_19F [-1] / lx_19F [-length(lx_19F)]

gx_20M = lx_20M [-1] / lx_20M [-length(lx_20M)]
gx_20F = lx_20F [-1] / lx_20F [-length(lx_20F)]
gx_20F


# Calculate l(x)b(x)


lxbx_19 = lx_19*bx_19
lxbx_20 = lx_20*bx_20

lxbx_19M = lx_19M*bx_19
lxbx_19F = lx_19F*bx_19

lxbx_20M = lx_20M*bx_20
lxbx_20F = lx_20F*bx_20


# Calculate l(x)b(x)x


lxbxx_19 = lxbx_19*Ages
lxbxx_20 = lxbx_20*Ages

lxbxx_19M = lxbx_19M*Ages
lxbxx_19F = lxbx_19F*Ages

lxbxx_20M = lxbx_20M*Ages
lxbxx_20F = lxbx_20F*Ages

# Calculate R0


R0_19 = sum(lxbx_19,na.rm = T)
R0_20 = sum(lxbx_20,na.rm = T)

R0_19M = sum(lxbx_19M,na.rm = T)
R0_19F = sum(lxbx_19F,na.rm = T)

R0_20M = sum(lxbx_20M,na.rm = T)
R0_20F = sum(lxbx_20F,na.rm = T)


R0_19
R0_20
R0_19M
R0_19F
R0_20M
R0_19F


# Calculate G(x)


G_19 = sum(lxbxx_19,na.rm = T)/R0_19
G_20 = sum(lxbxx_20,na.rm = T)/R0_20

G_19M = sum(lxbxx_19M,na.rm = T)/R0_19M
G_19F = sum(lxbxx_19F,na.rm = T)/R0_19F

G_20M = sum(lxbxx_20M,na.rm = T)/R0_20M
G_20F = sum(lxbxx_20F,na.rm = T)/R0_20F


G_19
G_20
G_19M
G_19F
G_20M
G_20F



# Estimate r
r_19 = log(R0_19)/G_19
r_20 = log(R0_20)/G_20

r_19M = log(R0_19M)/G_19M
r_19F = log(R0_19F)/G_19F

r_20M = log(R0_20M)/G_20M
r_20F = log(R0_20F)/G_20F

####### Making tables: they will display in a different source panel

# 19th Summary
CenturySummary19 = cbind(R0_19,G_19,r_19)
CenturySummary19F= cbind(R0_19F,G_19F,r_19F)
CenturySummary19M= cbind(R0_19M,G_19M,r_19M)
CenturySummaryTable19 = rbind(CenturySummary19, CenturySummary19F, CenturySummary19M)
rownames(CenturySummaryTable19) = c("Male + Female","Female", "Male")
colnames(CenturySummaryTable19) = c("R0_19th","G_19th","r_19th")
View(CenturySummaryTable19)

# 20th Summary
CenturySummary20 = cbind(R0_20,G_20,r_20)
CenturySummary20F= cbind(R0_20F,G_20F,r_20F)
CenturySummary20M= cbind(R0_20M,G_20M,r_20M)
CenturySummaryTable20 = rbind(CenturySummary20, CenturySummary20F, CenturySummary20M)
rownames(CenturySummaryTable20) = c("Male + Female","Female", "Male")
colnames(CenturySummaryTable20) = c("R0_20th","G_20th","r_20th")
View(CenturySummaryTable20)

# Need to attach package PerformanceAnalytics if you want a table in the plot panel
require(PerformanceAnalytics)
PerformanceAnalytics:::textplot(CenturySummaryTable19,
                                halign = "center", 
                                valign = "center",
                                max.cex = 2, 
                                cmar = 3, 
                                rmar = 3, 
                                hadj = 1, 
                                vadj = 1, 
                                row.valign = "center",
                                heading.valign = "center", 
                                mar = c(0, 0, 0, 0) + 0.5, 
                                col.data = par("col"),
                                col.rownames = par("col"), 
                                col.colnames = par("col"), 
                                wrap = TRUE,
                                wrap.colnames = 20, 
                                wrap.rownames = 20)


PerformanceAnalytics:::textplot(CenturySummaryTable20,
                                halign = "center", 
                                valign = "center",
                                max.cex = 2, 
                                cmar = 3, 
                                rmar = 3, 
                                hadj = 1, 
                                vadj = 1, 
                                row.valign = "center",
                                heading.valign = "center", 
                                mar = c(0, 0, 0, 0) + 0.5, 
                                col.data = par("col"),
                                col.rownames = par("col"), 
                                col.colnames = par("col"), 
                                wrap = TRUE,
                                wrap.colnames = 20, 
                                wrap.rownames = 20)



plot(lx_19F~Ages,
     type="l",
     col="hotpink",
     lwd=2,
     lty=2,
     xlab="Age",
     ylab="ln [ l(x) ]",
     log = "y")
     
points(lx_19M~Ages,
       type="l",
       col="blue",
       lwd=2,
       lty=2)

points(lx_20M~Ages,
       type="l",
       col="blue",
       lwd=2)

points(lx_20F~Ages,type="l",
       col="hotpink",
       lwd=2)
legend("bottomleft",
       c("19th Century Male","19th Century Female",
         "20th Century Male","20th Century Female"),
       lty=c(2,2,1,1),
       lwd=2,
       col=c("blue","hotpink"))

## Survival plot between centuries
plot(lx_19~Ages,
     type="l",
     col="black",
     lwd=2,
     xlab="Age",
     ylab="ln [ l(x) ]",
     log = "y")

points(lx_20~Ages,
       type="l",
       col="red",
       lwd=2,
       lty=2)

library(survival)
lxvalues<-data.frame(c(lx_19M, lx_19F, lx_20M, lx_20F))
g3 <- subset(glioma, histology == "Grade3")
survdiff(Surv(time, event) ~ group, data = g3)

plot(survfit(Surv(time, event) ~ group, data = g3),
     main = "Grade III Glioma", lty = c(2, 1),
     ylab = "Probability", xlab = "Survival Time in Month")
>>>>>>> Stashed changes
