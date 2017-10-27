#Graveyard Script

survival <-read.csv(file ="Graveyard_Survival.csv")

birth <-read.csv(file ="Graveyard_Birth.csv")

head(survival)

head(birth)

attach(survival)
attach(birth)
#View(survival)

#Age_of_death

Death_Age <- Death_Year-Birth_Year 


Ages<- min(birth$Age):max(birth$Age) 
Death_Age <- factor(Death_Age,levels=Ages) 
Death_Age_19 <- Death_Age[ Birth_Year < 1900 ]
Death_Age_20 <- Death_Age[ Birth_Year >= 1900 ] 
Death_Age_19M <- Death_Age[Birth_Year<1900&Sex=="M"]
Death_Age_19F <- Death_Age[Birth_Year<1900&Sex=="F"]
Death_Age_20M <- Death_Age[Birth_Year>=1900&Sex=="M"]
Death_Age_20F <- Death_Age[Birth_Year>=1900&Sex=="F"]


## Calculate Cohort Survival (Sx)

S0_19<-sum(Birth_Year<1900)
S0_20<-sum(Birth_Year>=1900)
S0_19M <- sum(Birth_Year<1900&Sex=="M")
S0_19F <- sum(Birth_Year<1900&Sex=="F")
S0_20M <- sum(Birth_Year>=1900&Sex=="M")
S0_20F <- sum(Birth_Year>=1900&Sex=="F")

#####life span)

Deaths_19 <- table(Death_Age_19)
Deaths_20 <- table(Death_Age_19)
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
#View(CenturySummaryTable19)

# 20th Summary
CenturySummary20 = cbind(R0_20,G_20,r_20)
CenturySummary20F= cbind(R0_20F,G_20F,r_20F)
CenturySummary20M= cbind(R0_20M,G_20M,r_20M)
CenturySummaryTable20 = rbind(CenturySummary20, CenturySummary20F, CenturySummary20M)
rownames(CenturySummaryTable20) = c("Male + Female","Female", "Male")
colnames(CenturySummaryTable20) = c("R0_20th","G_20th","r_20th")
#View(CenturySummaryTable20)

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

# Statistical test. First install surival, coin and splines to run the test.

#require(splines)       
library(survival)
#library(coin)

#Using logrank_test
# survival$Death_Age <- as.numeric(Death_Year-Birth_Year )
# survival$Event<-as.logical(rep("TRUE",length(survival$Death_Age)))
# survival$Century<-as.factor(ifelse(survival$Birth_Year<1900,"19th","20th"))
# SurvCurve<-Surv(survival$Death_Age, survival$Event,type="right") 
# logrank_test(SurvCurve~Sex,data=survival)
# logrank_test(SurvCurve~Century,data=survival)


# Very helpful website: http://bioconnector.org/workshops/r-survival.html
survival$Death_Age <- as.numeric(Death_Year-Birth_Year )
survival$Event<-as.logical(rep("TRUE",length(survival$Death_Age)))
survival$Century<-as.factor(ifelse(survival$Birth_Year<1900,"19th","20th"))

### Using Cox PH (proportional hazards) regression
# shows significance and direction
# if exp(coef)>1 Increase in probability of death
# if exp(coef)<1 Reduction in probability of death
coxph(Surv(Death_Age, Event)~Sex+Century, data=survival)

### Using survdiff log rank test
survdiff(Surv(Death_Age, Event)~Century, data=survival)
survdiff(Surv(Death_Age, Event)~Sex, data=survival)


