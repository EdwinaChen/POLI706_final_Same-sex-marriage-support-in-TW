############################################
### R script for Final Project #############
### POLI 706 SP21 ##########################
############by Edwina Chih-Yu Chen #########
############################################


setwd("/Users/ChihYuChen/Desktop/POLI 706/final/edu")
library(tidyverse)
library(haven)
library(ggplot2)
library(stargazer)
library(ggeffects) 
library(ggpubr)
library(car) 
library(Amelia)
library("PerformanceAnalytics")
library(devtools)
library(jtools)


### prepare data set "population"
#population data with basic information (sex, educational level, marriage status)
population<- read.csv("opendata108Y051.csv")
summary(population)

## make 4 variables
# marriage=1 if marry with differnt sex- partner
# samesexmarriage=1 if marry with same-sex partner
# young=1 if age between 20-34
# highedu=1 if have a degree from higher education (BA,MA,PhD)
population<- population %>%
  mutate(marriage = ifelse( marital_status == "有偶_不同性別",1,0),
         samesexmarriage = ifelse( marital_status == "有偶_相同性別" ,1,0),
         young = ifelse(age  == "20~24歲"| age  == "25~29歲"| age  == "30~34歲",1,0),
         highedu = ifelse(edu  == "博畢"| edu  == "大畢"| edu  == "碩畢",1,0),
  )

population$population<- gsub(",", "", population$population)
population$population<-as.numeric(population$population) 
sum(is.na(population$population)) ##only 1 missing value in first row (Chinese header)

## count how many person in one district is certain status
population$marriage <- population$marriage*population$population
population$samesexmarriage <- population$samesexmarriage*population$population
population$young  <- population$young*population$population
population$highedu <- population$highedu*population$population


## group the data by district(site_id))
population<- population %>%
  group_by(site_id) %>%
  summarise(marriage  = sum(marriage),samesexmarriage = sum(samesexmarriage),
            young = sum(young),highedu= sum(highedu))

## rename some district name, so the format is same in 2 data sets
population[15,1] <-"嘉義市東區"
population[16,1] <-"嘉義市西區"
population[142,1] <-"新竹市北區"
population[143,1] <-"新竹市東區"
population[177,1] <-"臺中市中區"
population[178,1] <-"臺中市北區"
population[180,1] <-"臺中市南區"
population[224,1] <-"臺南市北區"
population[226,1] <-"臺南市南區"
population[242,1] <-"臺南市東區"
population[368,1] <-"高雄市鳳山區"
population[369,1] <-"高雄市鳳山區"
population[332,1] <-"高雄市三民區"
population[333,1] <-"高雄市三民區"

### prepare data set "referendum"
#referendum vote data on same-sex marriage
#Question 14- “Do you agree to the protection of same-sex marital rights with marriage as defined in the Civil Code?”

referendum<- read.csv("Q14vote.csv")

names(referendum)[3] <- "site_id"
names(referendum)[10] <- "votercount" ##how many valid voters
names(referendum)[5] <- "agreevote" ##support same-sex marriage

## merged the data bt district
data <-left_join(referendum, population, by = c("site_id"="site_id"))


## make variables (percentage)

data$agreevote<- gsub(",", "", data$agreevote)
data$votercount<- gsub(",", "", data$votercount)
data$agreevote <-as.numeric(data$agreevote) 
data$votercount<-as.numeric(data$votercount) 

data$support.rate <- data$agreevote/data$votercount
data$youth.rate <- data$young/data$votercount
data$highedu.rate <- data$highedu/data$votercount
data$marriage.rate <- data$marriage/data$votercount
data$samesexmarriage.rate <- data$samesexmarriage/data$votercount

summary(data)

data <- data%>%
  select(site_id,support.rate,youth.rate,highedu.rate,marriage.rate,samesexmarriage.rate)

##
missmap(data, y.labels=NULL, y.at=NULL, main="missingness in Data") ## the national row

#####Data Visualization
# I save image in pdf form by using R studio function (export button)
####Get Chinese in text
install.packages("showtext")
library(showtext)
showtext.auto(enable = TRUE)


## Highest & Lowest support rate district

data %>%
  filter(support.rate >0.2) %>%
  mutate(name = fct_reorder(site_id,support.rate)) %>%
  ggplot( aes(x=site_id, y=support.rate)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  ggtitle("Districts with highest support rate") +
  xlab("") +
  theme_bw()+
  theme(text=element_text(family="Apple LiSung Light"))

data %>%
  filter(support.rate <0.1) %>%
  mutate(name = fct_reorder(site_id,support.rate)) %>%
  ggplot( aes(x=site_id, y=support.rate)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  ggtitle("Districts with lowest support rate") +
  xlab("") +
  theme_bw()+
  theme(text=element_text(family="Apple LiSung Light"))



## Distribution of support.rate/youth.rate/highedu.rate/marriage.rate
d.support <- data %>%
  ggplot( aes(x=support.rate)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Distribution of support rate") 

d.youth <- data %>%
  ggplot( aes(x=youth.rate)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Distribution of youth population rate") 

d.highed <- data %>%
  ggplot( aes(x=highedu.rate)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Distribution of higher educational rate") 

d.marriage <-data %>%
  ggplot( aes(x=marriage.rate)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Distribution of marriage rate") 

d.samemarriage <- data %>%
  ggplot( aes(x=samesexmarriage.rate)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Distribution of same-sex marriage rate") 

ggarrange(d.support, d.youth, d.highed, d.marriage, d.samemarriage + rremove("x.text"), 
          ncol = 2, nrow = 3)

## Relationship between support.rate and youth.rate/highedu.rate/marriage.rate
r.youth <- data %>%
 ggplot(aes(x=youth.rate, y=support.rate)) + 
  geom_point()+
  ggtitle("Relationship between support rate and youth rate") 

r.highed  <- data %>%
  ggplot(aes(x=highedu.rate, y=support.rate)) + 
  geom_point()+
  ggtitle("Relationship between support rate and higher educational rate") 

r.marriage <- data %>%
  ggplot(aes(x=marriage.rate, y=support.rate)) + 
  geom_point()+
  ggtitle("Relationship between support rate and marriage.rate") 

r.samemarriage <- r.marriage <- data %>%
  ggplot(aes(x=samesexmarriage.rate, y=support.rate)) + 
  geom_point()+
  ggtitle("Relationship between support rate and same-sex marriage rate") 

ggarrange(r.youth, r.highed, r.marriage, r.samemarriage + rremove("x.text"), 
          ncol = 2, nrow = 2)




#####OLS   model
##all possible combinations, max R-squared
modeldata <- data%>%
  select(support.rate,youth.rate,highedu.rate,marriage.rate,samesexmarriage.rate)

#write.csv(modeldata,"model.data.csv") ## save a csv to save some time later
#modeldata<- read.csv("model.data.csv")

#Correlation matrix 
library("PerformanceAnalytics")
chart.Correlation(modeldata, histogram=FALSE, pch=19)


cols<-c()
n<-c()
r2<-c()
for(p in 1:4){
  possible<-t(combn(length(names(modeldata)[-1]), p))
  for(r in 1:nrow(possible)){
    subdata<-modeldata[,c(1,possible[r,]+1)]
    mod<-lm(support.rate~., data=subdata)
    cols<-c(cols, paste(possible[r,]+1, collapse=","))
    n<-c(n,length(mod$coefficients))
    r2<-c(r2, summary(mod)$adj.r.squared)
    mod.inter<-lm(support.rate~(.)^2, data=subdata)
    cols<-c(cols, paste(paste(possible[r,]+1, collapse=","), ".inter", sep=""))
    n<-c(n,length(mod.inter$coefficients))
    r2<-c(r2, summary(mod.inter)$adj.r.squared)
  }
}
which(r2==max(r2))
cols[which(r2==max(r2))]  # 2,3,4,5 

model1 <- lm(support.rate~ highedu.rate + marriage.rate +youth.rate + samesexmarriage.rate , data = modeldata)
summary(model1)


##backwards selection

model2 <- lm(support.rate~ highedu.rate +marriage.rate +youth.rate, data = modeldata)
summary(model2)

model3 <- lm(support.rate~ highedu.rate + marriage.rate  , data = modeldata)
summary(model3)

coef.table1 <- stargazer(model1, model2, model3, 
                         column.labels = c("Model 1", "Model 2", "Model 3"), 
                         model.names=FALSE, style="aer",
                         ci=TRUE, 
                         dep.var.labels.include=TRUE,
                         dep.var.labels = "DV = Same-sex marriage support rate",
                         table.placement="htbp", 
                         font.size="scriptsize",
                         model.numbers=FALSE, 
                         notes.label=" ",
                         digits=2, df=FALSE, omit.stat=c("rsq"), 
                         order = c(1, 2, 3),
                         covariate.labels = c("Higher education rate", "Marriage rate", "Youth Population rate","Same-sex marriage rate"), 
                         no.space=TRUE, 
                         title = "Coefficients and 95 percent confidence bounds OLS",
                         label = "table:coef1", 
                         notes.align = "r",
                         notes = c("*** p less than 0.001, ** p less than  0.01, * p less than  0.05"), 
                         star.cutoffs = c(0.05, 0.01, 0.001),
                         notes.append = FALSE)


##Relationship between the response variable and youth.rate/samesexmarriage.rate
avPlots(model1, "youth.rate")
avPlots(model1, "samesexmarriage.rate")

## these 2 variables are not very important


##illustration  of the regression estimates
plot_coefs(model3, plot.distributions=T, ci_level=.95, omit.coefs = c("(Intercept)"))
plot_summs(model1, model2, model3, scale = TRUE, plot.distributions = TRUE)

fit<-ggpredict(model3, terms = "highedu.rate") 
plot(fit)

fit2<-ggpredict(model3, terms = "marriage.rate") 
plot(fit2)




## OLS assumptions 

## Regression Diagnostics

## Diagnostic Plots from car package
par(mfrow=c(2,2))
plot(model3, which=1:4)

cookd <- cooks.distance(model3)
plot(cookd, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 0.05, col="red") 

## df beta test
barplot(sort(dfbetas(model3)[,2]))
modeldata[which(dfbetas(model3)[,2]<=-.1),]


## VIF is fine
car::vif(model3)

# Heteroskedasticity
library(lmtest)
bptest(model3) 


# Outlier
inval<-as.numeric(names(model3$residuals))

coefs<-c(NA, NA)
for(r in inval){
  mod3<-lm(support.rate~ highedu.rate + marriage.rate , data=modeldata[-r,])
  coefs<-rbind(coefs, model3$coefficients-mod3$coefficients)
}; coefs<-coefs[-1,]

hist(coefs[,2], main="Difference in Coefficients(Higher educatioan) when each observation is removed", xlab="Coefficients")
abline(v=-.002,col= "red" )
modeldata[which(coefs[,2]<=-.002),]


hist(coefs[,3], main="Difference in Coeffiecients(Marriage rate) when each observation is removed", xlab="Coefficients")
abline(v=.003,col= "red" )
modeldata[which(coefs[,3]>=.003),]


plot(rstudent(model3)~hatvalues(model3), cex=cooks.distance(model3)*100)
text(hatvalues(model3), rstudent(model3), data$site_id, cex=cooks.distance(model3)*20)

summary(modeldata)

####dfbeta

barplot(sort(dfbetas(model3)[,2]))
barplot(sort(dfbetas(model3)[,3]))
modeldata[which(dfbetas(model3)[,2]>=.2),]
modeldata[which(dfbetas(model3)[,3]<=-.4),]


