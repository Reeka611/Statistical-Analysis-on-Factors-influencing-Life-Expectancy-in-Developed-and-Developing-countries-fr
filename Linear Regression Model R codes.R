library(dplyr)
library(corrplot)
library(ggplot2)
library(gapminder)
library(ggpubr)
library(gifski)
library(png)
library(gganimate)

data<-read.csv("C:/Users/HAZARIKA/Desktop/Presentations/Linear Regression Model Presentation/Life Expectancy/Life Expectancy Data - Main.csv")
str(data)
#'data.frame':	2938 obs. of  22 variables:
#$ Country                     : Factor w/ 193 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ Year                        : int  2015 2014 2013 2012 2011 2010 2009 2008 2007 2006 ...
#$ Status                      : Factor w/ 2 levels "Developed","Developing": 2 2 2 2 2 2 2 2 2 2 ...
#$ LifeExpectancy              : num  65 59.9 59.9 59.5 59.2 58.8 58.6 58.1 57.5 57.3 ...
#$ AdultMortality              : int  263 271 268 272 275 279 281 287 295 295 ...
#$ InfantDeaths                : int  62 64 66 69 71 74 77 80 82 84 ...
#$ Alcohol                     : num  0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.03 0.02 0.03 ...
#$ PercentageExpenditure       : num  71.3 73.5 73.2 78.2 7.1 ...
#$ HepatitisB                  : int  65 62 64 67 68 66 63 64 63 64 ...
#$ Measles                     : int  1154 492 430 2787 3013 1989 2861 1599 1141 1990 ...
#$ BMI                         : num  19.1 18.6 18.1 17.6 17.2 16.7 16.2 15.7 15.2 14.7 ...
#$ UnderFiveDeaths             : int  83 86 89 93 97 102 106 110 113 116 ...
#$ Polio                       : int  6 58 62 67 68 66 63 64 63 58 ...
#$ TotalExpenditure            : num  8.16 8.18 8.13 8.52 7.87 9.2 9.42 8.33 6.73 7.43 ...
#$ Diphtheria                  : int  65 62 64 67 68 66 63 64 63 58 ...
#$ HIV                         : num  0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 ...
#$ GDP                         : num  584.3 612.7 631.7 670 63.5 ...
#$ Population                  : num  33736494 327582 31731688 3696958 2978599 ...
#$ thinness1To19years        : num  17.2 17.5 17.7 17.9 18.2 18.4 18.6 18.8 19 19.2 ...
#$ thinness5To9years          : num  17.3 17.5 17.7 18 18.2 18.4 18.7 18.9 19.1 19.3 ...
#$ IncomeCompositionOfResources: num  0.479 0.476 0.47 0.463 0.454 0.448 0.434 0.433 0.415 0.405 ...
#$ LiteracyRate                   : num  10.1 10 9.9 9.8 9.5 9.2 8.9 8.7 8.4 8.1 ...


data<-data[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
data1<-data.matrix(data, rownames.force = NA)


#Checking if there are NAs in the dataset
any(is.na(data))
FALSE


#Calculating the linear correlation coefficient "r" for 17 pairs

M<-cor(data1,use="complete.obs")
M
(CM=round(M,2))

corrplot(CM,type="upper",method="number")
corrplot.mixed(CM,lower.col = "black", number.cex = 2,number.font = 5,
               col = wb,bg="gold2",order="PCA",addcolorlabel="no")

#Correlation Coefficient
#the size of "r" indicates the strenght of the linear relationship between 
#independent(x) and dependent(y) variables
#If "r"is near to either +1 or -1, then the linear relationship between x and y is strong.
#If "r" is near 0 of either sign, the linear relationship between x and y is weak.

#Taking into consideration the above details we will consider only the below 8 pairs 
#to have strong linear relationship

#LifeExpectancy,HIV
#LifeExpectancy,IncomeCompositionOfResources
#LifeExpectancy,Schooling
#LifeExpectancy,AdultMortality
#LifeExpectancy,BMI
#LifeExpectancy,thinness5To9years
#LifeExpectancy,thinness1To19years


mod=lm(LifeExpectancy~IncomeCompositionOfResources+HIV+Schooling+AdultMortality+BMI
       +thinness5To9years+thinness1To19years,data)
summary(mod)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-21.0762  -2.1138  -0.1312   2.2161  23.0595 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                  54.2597724  0.4943759 109.754  < 2e-16 ***
#  IncomeCompositionOfResources  9.0010988  0.6441638  13.973  < 2e-16 ***
#  HIV                          -0.4980397  0.0181707 -27.409  < 2e-16 ***
#  LiteracyRate                  1.0187736  0.0431776  23.595  < 2e-16 ***
#  AdultMortality               -0.0184903  0.0008502 -21.748  < 2e-16 ***
#  BMI                           0.0390795  0.0053313   7.330 3.01e-13 ***
#  thinness5To9years            -0.0070214  0.0512302  -0.137    0.891    
#thinness1To19years           -0.0794721  0.0522268  -1.522    0.128    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 4.18 on 2728 degrees of freedom
#(202 observations deleted due to missingness)
#Multiple R-squared:  0.7993,	Adjusted R-squared:  0.7988 
#F-statistic:  1552 on 7 and 2728 DF,  p-value: < 2.2e-16


mod=lm(LifeExpectancy~HIV,data)
summary(mod)
p<-data%>%
  ggplot(aes(HIV,LifeExpectancy))+geom_point(color="red")+
  geom_abline(intercept = 71, slope = -9)+facet_wrap(~Status)+
  labs(title = 'Year: {frame_time}', x = 'HIV', y = 'LifeExpectancy') +
  transition_time(Year) +
  ease_aes('linear')+
  annotate("rect", xmin = 40, xmax = 50, ymin = 75, ymax = 90, fill="white", colour="red") +
  annotate("text", x=44, y=88, label = "R^2 == 0.35", parse=T) +
  annotate("text", x=44, y=82, label = "alpha == 71", parse=T) +
  annotate("text", x=44, y=78, label = "beta == -9", parse=T)
p
animate(p)
anim_save("p.gif",p)


mod=lm(LifeExpectancy~IncomeCompositionOfResources,data)
summary(mod)
p<-data%>%
  ggplot(aes(IncomeCompositionOfResources,LifeExpectancy))+geom_point(color="red")+
  geom_abline(intercept = 47, slope = 35)+facet_wrap(~Status)+
  labs(title = 'Year: {frame_time}', x = 'IncomeCompositionOfResources', y = 'LifeExpectancy') +
  transition_time(Year) +
  ease_aes('linear')+
  annotate("rect", xmin = 0.09, xmax = 0.25, ymin = 75, ymax = 90, fill="white", colour="red") +
  annotate("text", x=0.17, y=88, label = "R^2 == 0.52", parse=T) +
  annotate("text", x=0.15, y=82, label = "alpha == 47", parse=T) +
  annotate("text", x=0.15, y=78, label = "beta == 35", parse=T)
p
animate(p)
anim_save("p.gif",p)



mod=lm(LifeExpectancy~LiteracyRate,data)
summary(mod)
p<-data%>%
  ggplot(aes(LiteracyRate,LifeExpectancy))+geom_point(color="red")+
  geom_abline(intercept = 41, slope = 2)+facet_wrap(~Status)+
  labs(title = 'Year: {frame_time}', x = 'LiteracyRate', y = 'LifeExpectancy') +
  transition_time(Year) +
  ease_aes('linear')+
  annotate("rect", xmin = 15, xmax = 20, ymin = 40, ymax = 55, fill="white", colour="red") +
  annotate("text", x=17, y=53, label = "R^2 == 0.53", parse=T) +
  annotate("text", x=17, y=47, label = "alpha == 41", parse=T) +
  annotate("text", x=17, y=42, label = "beta == 2", parse=T)
p
animate(p)
anim_save("p.gif",p)

mod=lm(LifeExpectancy~LiteracyRate,data)
summary(mod)
p<-data%>%
  ggplot(aes(LiteracyRate,LifeExpectancy))+geom_point(color="red")+
  geom_abline(intercept = 41, slope = 2)+
  annotate("rect", xmin = 15, xmax = 20, ymin = 40, ymax = 55, fill="white", colour="red") +
  annotate("text", x=17, y=53, label = "R^2 == 0.53", parse=T) +
  annotate("text", x=17, y=47, label = "alpha == 41", parse=T) +
  annotate("text", x=17, y=42, label = "beta == 2", parse=T)
p

mod=lm(LifeExpectancy~AdultMortality,data)
summary(mod)
p<-data%>%
  ggplot(aes(AdultMortality,LifeExpectancy))+geom_point(color="red")+
  geom_abline(intercept = 77, slope = -0.05)+facet_wrap(~Status)+
  labs(title = 'Year: {frame_time}', x = 'AdultMortality', y = 'LifeExpectancy') +
  transition_time(Year) +
  ease_aes('linear')+
  annotate("rect", xmin = 400, xmax = 700, ymin = 75, ymax = 90, fill="white", colour="red") +
  annotate("text", x=530, y=87, label = "R^2 == 0.49", parse=T) +
  annotate("text", x=530, y=82, label = "alpha == 77", parse=T) +
  annotate("text", x=530, y=78, label = "beta == -0.05", parse=T)
p
animate(p)
anim_save("p.gif",p)


mod=lm(LifeExpectancy~BMI,data)
summary(mod)
p<-data%>%
  ggplot(aes(BMI,LifeExpectancy))+geom_point(color="red")+
  geom_abline(intercept = 60, slope = 0.2)+
  labs(title = 'Year: {frame_time}', x = 'BMI', y = 'LifeExpectancy') +
  transition_time(Year) +
  ease_aes('linear')+
  annotate("rect", xmin = 48, xmax = 75, ymin = 40, ymax = 55, fill="white", colour="red") +
  annotate("text", x=62, y=52, label = "R^2 == 0.29", parse=T) +
  annotate("text", x=60, y=47, label = "alpha == 60", parse=T) +
  annotate("text", x=60, y=43, label = "beta == 0.2", parse=T)
p
animate(p)
anim_save("p.gif",p)


mod=lm(LifeExpectancy~Polio,data)
summary(mod)
p<-data%>%
  ggplot(aes(Polio,LifeExpectancy))+geom_point(color="red")+
  geom_abline(intercept = 53.7, slope = 0.2)+
  labs(title = 'Year: {frame_time}', x = 'Polio', y = 'LifeExpectancy') +
  transition_time(Year) +
  ease_aes('linear')+
  annotate("rect", xmin = 20, xmax = 40, ymin = 83, ymax = 93, fill="white", colour="red") +
  annotate("text", x=30, y=91, label = "R^2 == 0.22", parse=T) +
  annotate("text", x=30, y=88, label = "alpha == 53.7", parse=T) +
  annotate("text", x=30, y=85, label = "beta == 0.2", parse=T)
p
animate(p)
anim_save("p.gif",p)
  


head(fortify(mod))
#.hat          .sigma      .cooksd  .fitted    .resid  .stdresid
#1 0.003931122 4.180243 9.122759e-05 63.20619  1.793808  0.4300258
#2 0.004100690 4.179993 2.635337e-04 62.88460 -2.984604 -0.7155538
#3 0.004166311 4.180028 2.437233e-04 62.74735 -2.847352 -0.6826705
#4 0.004301214 4.179996 2.740103e-04 62.47097 -2.970965 -0.7123558
#5 0.004404711 4.180043 2.471532e-04 61.98797 -2.787975 -0.6685144
#6 0.004482474 4.180060 2.390052e-04 61.51754 -2.717537 -0.6516498

residPlot<-ggplot(aes(x=.fitted,y=.resid),data=mod)+geom_point()+geom_hline(yintercept = 0)+
  labs(x="Fitted Values",y="Residual")
residPlot

mymodel=lm(LifeExpectancy~.,data)
summary(mymodel)
mymodel=lm(LifeExpectancy~.-Year-Status-InfantDeaths-Alcohol-PercentageExp-HepatitisB
           -Measles-UnderFiveDeaths-Polio-TotalExp-Diphtheria-GDP-Population,data)
summary(mymodel)
plot(mymodel)


vif(mod)
#IncomeCompositionOfResources                          HIV                    Schooling 
#2.774723                                           1.407718                  3.129025 
#AdultMortality                   BMI            thinness5To9years 
#1.705367                     1.769541                     8.564093 
#thinness1To19years             Polio 
#8.556438                     1.227036