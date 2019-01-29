setwd("E:/dersler/Statistics 1/main assignment")
library(tidyverse)
library(foreign)
library(nortest)
library(Hmisc)
library(arsenal)
library(psych)
library(corrplot)
library(car)
library(glmnet)
library(randtests)
library(lmtest)
library(caret)
library(e1071)

#Q1
news <- read.csv("alldata_onlinenews_25.csv", sep=";")
test <- read.csv("OnlineNewsPopularity_test.csv", sep=";")
names(test)
news <- subset(news, select = -c(X, url,timedelta))
names(news)
test <- subset(test, select = -c(X, url,timedelta))
names(test)
sum(news %in% test)

fulldata <- rbind(news, test)
str(fulldata)
which(is.na(fulldata))

str(news)
glimpse(news)
str(test)
summary(news)
head(news)

########################################
####change data types

sapply(news,class)
facnews <- news[,sapply(news, is.factor)]
intnews <- news[,sapply(news, is.integer)]

for(i in 1:62){
  if (count(unique(news[i]))<=2){
    print(unique(news[i]))
  }  
}

a<- c("data_channel_is_lifestyle"
,"data_channel_is_entertainment"
,"data_channel_is_bus"
,"data_channel_is_socmed"
,"data_channel_is_tech"
,"data_channel_is_world"
,"weekday_is_monday"
,"weekday_is_tuesday"
,"weekday_is_wednesday"
,"weekday_is_thursday"
,"weekday_is_friday"
,"weekday_is_saturday"
,"weekday_is_sunday"
,"is_weekend")

facnews2 <- news[a]
facnews2[,a] <- apply(facnews2[,a], 2, function(x) as.logical(x))

intnews2 <- news[,!(names(news)%in%a)]
asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[sapply(d,is.factor)], as.numeric))
intnews2 <- factorsNumeric(intnews2)
str(facnews2)
str(intnews2)
news2 <- cbind(intnews2, facnews2)
news2 <- news2[names(test)]
str(news2)

test2 <- test
test2[,a] <- apply(test2[,a], 2, function(x) as.logical(x))
test2[,!(names(test2)%in%a)] <-factorsNumeric(test2[,!(names(test2)%in%a)])
str(test2)

fulldata2 <- rbind(news2, test2)
#########################################

#shares

head(sort(news2$shares, decreasing = T),5)
summary(news2$shares)
ggplot(news2, aes(shares))+geom_histogram()

par(mfrow=c(2,2))
for(i in 1:48){
  hist(intnews2[,i], main=names(intnews2)[i], xlab = names(intnews2)[i])
}

####################################
#cheking normality of variables
normality <- function(x){
  lillie.test(x)
  shapiro.test(x)
  if((lillie.test(x)$p.value >0.05) &
     (shapiro.test(x)$p.value >0.05)){
    print("Data is normally distributed")
  } else {
    print("Not normally distributed, Reject null Hypothesis")
  }
}

for(i in 1:ncol(intnews2)){
  print(names(intnews2[i]))
  normality(intnews2[,i])
}
###################################
#### explore factor variables and probabilities


for(i in 1:ncol(facnews2)){
  tbl= table(facnews2[i])
  tbl = cbind(tbl,round(prop.table(tbl),2))
  colnames(tbl) <- c(names(facnews2)[i], "prob in column")
  print(tbl[2,])
}

par(mfrow=c(1,2))
for(i in 1:ncol(facnews2)){
  boxplot(intnews2$shares~facnews2[,i], xlab=paste("cor btw:",(cor(intnews2$shares, faknews2[i]))), main=names(faknews2)[i], ylab='Nr of Shares')
  abline(lm(intnews2$shares~facnews2[,i]))
}

par(mfrow=c(1,1))
data_channel <- sapply(lapply(1:6, function(x) facnews2[,x]==1), sum)
pie(data_channel, labels = names(facnews2[1:7]), main = "Distribution of Data Channel")

days <- sapply(lapply(7:13, function(x) facnews2[,x]==1), sum)
pie(days, labels = names(faknews2[7:13]), main = "Share Intensity btw Days")

##################################
##### pairwise comparisons

par(mfrow=c(2,2))
for(i in 1:(ncol(news2)-1)){
  scatter.smooth(x=news2[,i], y=news2$shares, 
                 xlab = names(news2)[i],
                 ylab = "Nr of shares",
                 main=paste(names(news2)[i],"~shares"))
}

pairs(data=intnews2[1:6], intnews2$shares~.)
par(mfrow=c(1,1))
corrplot(cor(intnews2), method = "number",order='hclust', type="upper")
round(cor(intnews2, intnews2$shares),2)


par(mfrow=c(2, 2))
for(i in 1:ncol(intnews2)){
  plot(density(intnews2[,i]), main=paste("Density Plot:",names(news2)[i]), ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(intnews2[,i]), 2))) 
  polygon(density(intnews2[,i]), col="red")
}


#####################################
#Q2

### checking multicollinearity
alias( lm(shares~.,news2) )
##Nonzero entries in the "complete" matrix show that those terms are linearly dependent on UseMonthly. 
##This means they're highly correlated, so I need to solve this problem.


fulldata3 <- fulldata2

fulldata3$is_weekday <- fulldata3$weekday_is_friday +
  fulldata3$weekday_is_monday + fulldata3$weekday_is_thursday+ 
  fulldata3$weekday_is_tuesday+ fulldata3$weekday_is_wednesday
unique(fulldata3$is_weekday)
str(fulldata3)
sum(fulldata3$is_weekday , fulldata3$is_weekend)

#from now on is_weekday is has same data with is_weekend and other weekday datas
fulldata3 <- subset(fulldata3, select = -c(weekday_is_monday
                                           ,weekday_is_tuesday
                                           ,weekday_is_wednesday
                                           ,weekday_is_thursday
                                           ,weekday_is_friday
                                           ,weekday_is_saturday
                                           ,weekday_is_sunday
                                           ,is_weekend))
news3 <- fulldata3[1:3000,]
test3 <- fulldata3[3001:13000,]
cor(news3$shares, news3$is_weekday)
modelday <- lm(log(shares)~., news3)
summary(modelday)                       #12.9%

fulldata3 <- fulldata2
fulldata3 <- subset(fulldata3, select = -c(weekday_is_monday
                                           ,weekday_is_wednesday
                                           ,weekday_is_thursday
                                           ,weekday_is_saturday
                                           ,weekday_is_sunday
                                           ))

news3 <- fulldata3[1:3000,]
test3 <- fulldata3[3001:13000,]
modelday <- lm(log(shares)~., news3)
summary(modelday)                       #13.13%
round(vif(modelday),1) 

# this works better

for(i in 1:(ncol(intnews2)-1)){
  if (abs(cor(intnews2$shares, intnews2[i]))>0.05){
    cn <- cor(intnews2$shares, intnews2[i])
    print(cor(intnews2$shares, intnews2[i]))
  }
}

# add log to shares

fulldata3$shares <- log(fulldata3$shares)
news3 <- fulldata3[1:3000,]
test3 <- fulldata3[3001:13000,]


#########################################
####create modelraw

modelraw <- lm(shares~num_hrefs+num_keywords+kw_min_avg+
              kw_max_avg+kw_avg_avg+LDA_02+LDA_03+LDA_04+
              global_subjectivity+avg_negative_polarity+
              title_subjectivity+data_channel_is_world+
              data_channel_is_socmed+weekday_is_tuesday+
              weekday_is_friday+is_weekend, news3)
summary(modelraw)     #9.88 %
RMSE(modelraw, news3$shares)

modelraw <- lm(shares~num_hrefs+log(num_keywords)+log(kw_min_avg)+
                 log(kw_max_avg)+kw_avg_avg+LDA_03+LDA_04+
                 
                 title_subjectivity+data_channel_is_world+
                 data_channel_is_socmed+weekday_is_tuesday+
                 weekday_is_friday+is_weekend, news3)
summary(modelraw)    #10.51%


#delete some of insignificant variables and try again, after
#several tries, I found one significant model
modelraw <- lm(shares~num_hrefs+num_keywords+
              kw_avg_avg+LDA_03+kw_max_avg+
              title_subjectivity+data_channel_is_world+
              data_channel_is_socmed+is_weekend+
              weekday_is_tuesday, news3)
summary(modelraw)    #9.63%
######################################
#######create stepwise models raw data

#This time, I try to find a model using stepwise method without deleting days and taking log of shares;
modelfull <- lm(shares ~ .,news2)
modelnull <- lm(shares~1, news2)
summary(modelfull)
step(modelfull, direction='back')
modelsb <- lm(formula = shares ~ n_tokens_content + num_hrefs + 
                average_token_length + data_channel_is_entertainment
              + data_channel_is_world + kw_min_min + kw_avg_min + 
                kw_avg_max + kw_avg_avg + weekday_is_tuesday + 
                LDA_04 + global_subjectivity + max_negative_polarity
              + title_subjectivity + abs_title_subjectivity, data = news2)
summary(modelsb)
step(modelfull, direction='both')
modelst <- lm(formula = shares ~ n_tokens_content + num_hrefs + average_token_length + 
                data_channel_is_entertainment + data_channel_is_world + kw_min_min + 
                kw_avg_min + kw_avg_max + kw_avg_avg + weekday_is_tuesday + 
                LDA_04 + global_subjectivity + global_rate_positive_words + 
                max_negative_polarity + title_subjectivity + abs_title_subjectivity + 
                weekday_is_friday + max_positive_polarity, data = news2)
summary(modelst)
step(modelnull,
     scope = list(upper=modelfull),
     direction="forward",
     data=news2) 
modelsf <- lm(formula = shares ~ kw_avg_avg + num_hrefs + data_channel_is_entertainment + 
                average_token_length + LDA_04 + global_subjectivity + weekday_is_tuesday + 
                max_negative_polarity + data_channel_is_tech + global_rate_positive_words + 
                weekday_is_friday + title_subjectivity + abs_title_subjectivity + 
                n_tokens_content + max_positive_polarity, data = news2)
summary(modelsf)



######################################
#######create stepwise models based on log(shares)

modelfull2 <- lm(shares ~ .,news3)
modelnull2 <- lm(shares~1, news3)
summary(modelfull2)                   #13.13%
step(modelfull2, direction='back')
modelsb2 <- lm(formula = shares ~ n_tokens_content + n_non_stop_words + num_hrefs + 
                 average_token_length + data_channel_is_lifestyle + data_channel_is_entertainment + 
                 data_channel_is_bus + data_channel_is_world + kw_min_min + 
                 kw_avg_min + kw_min_max + kw_avg_max + kw_max_avg + kw_avg_avg + 
                 self_reference_min_shares + self_reference_max_shares + self_reference_avg_sharess + 
                 weekday_is_tuesday + weekday_is_friday + is_weekend + LDA_00 + 
                 LDA_03 + global_subjectivity + rate_negative_words + max_positive_polarity + 
                 title_subjectivity + abs_title_subjectivity, data = news3)
summary(modelsb2)            #13.46%
step(modelfull2, direction='both')
modelst2 <- lm(formula = shares ~ n_tokens_content + n_non_stop_words + num_hrefs + 
                 average_token_length + data_channel_is_lifestyle + data_channel_is_entertainment + 
                 data_channel_is_bus + data_channel_is_world + kw_min_min + 
                 kw_avg_min + kw_min_max + kw_avg_max + kw_max_avg + kw_avg_avg + 
                 self_reference_min_shares + self_reference_max_shares + self_reference_avg_sharess + 
                 weekday_is_tuesday + weekday_is_friday + is_weekend + LDA_00 + 
                 LDA_03 + global_subjectivity + rate_negative_words + max_positive_polarity + 
                 title_subjectivity + abs_title_subjectivity, data = news3)
summary(modelst2)           #13.46%
step(modelnull2,
     scope = list(upper=modelfull2),
     direction="forward",
     data=news3) 
modelsf2 <-  lm(formula = shares ~ kw_avg_avg + data_channel_is_entertainment + 
                 is_weekend + num_hrefs + average_token_length + kw_min_min + 
                 kw_min_max + data_channel_is_tech + data_channel_is_socmed + 
                 kw_max_avg + title_subjectivity + weekday_is_friday + abs_title_subjectivity + 
                 rate_negative_words + self_reference_max_shares + kw_avg_min + 
                 LDA_03 + max_positive_polarity + global_subjectivity + n_tokens_content + 
                 n_non_stop_words + weekday_is_tuesday + LDA_04 + self_reference_min_shares + 
                 self_reference_avg_sharess + data_channel_is_world, data = news3) 
            
summary(modelsf2)    # 13.4%


round(vif(modelfull2),1)

anova(modelsb, modelst, modelraw, modelsb2, modelsf2,modelst2)


######################################
#######create model with lasso

X <- model.matrix(modelfull2)[,-1]
lasso <- glmnet(X, news3$shares)
lasso1 <- cv.glmnet(X, news3$shares, alpha = 1)
lasso1$lambda
lasso1$lambda.min
lasso1$lambda.1se
plot(lasso1)
coef(lasso1, s = "lambda.min")
coef(lasso1, s = "lambda.1se")
modellassoraw <- lm(shares~num_hrefs+average_token_length+num_keywords
                 +data_channel_is_entertainment+data_channel_is_world+data_channel_is_socmed+
                   data_channel_is_tech+kw_min_min+kw_min_max+
                   kw_max_avg+kw_avg_avg+self_reference_max_shares+weekday_is_tuesday+
                   is_weekend+global_subjectivity+rate_negative_words+
                   title_subjectivity,news3) 

modellasso <- lm(shares~num_hrefs+average_token_length
                 +data_channel_is_entertainment+data_channel_is_socmed+
                   data_channel_is_tech+kw_min_min+kw_min_max+
                   I(kw_max_avg^2)+kw_avg_avg+weekday_is_tuesday+
                   is_weekend+I(rate_negative_words^(2))+
                   I(title_subjectivity^5),news3)
summary(modellasso)     #%12.53
residualPlots(modellasso, plot=F)

anova(modelraw, modelsb,modelsf, modelst,modellasso)


##########################################
#############################################
#feature scaling

normalizevar <- function(x) {
  ((x - mean(x))/(max(x)-min(x)))
}

scalevar <- function(x){
  x/max(x)
}
fd1 <- fulldata2


fd1[c("kw_min_min", "kw_avg_min", "kw_min_avg")] <- abs(fd1[c("kw_min_min", "kw_avg_min", "kw_min_avg")]
)
fd1[c("n_tokens_content", "kw_max_min", "kw_avg_min", "kw_min_min",
      "kw_min_avg","kw_min_max","kw_max_max", "kw_avg_max",
      "kw_max_avg", "kw_avg_avg","self_reference_min_shares",
      "self_reference_max_shares","self_reference_avg_sharess", "shares")] <- log(fd1[c("n_tokens_content", "kw_max_min", "kw_avg_min", "kw_min_min",
                                                                                        "kw_min_avg","kw_min_max","kw_max_max", "kw_avg_max",
                                                                                        "kw_max_avg", "kw_avg_avg","self_reference_min_shares",
                                                                                        "self_reference_max_shares","self_reference_avg_sharess", "shares")]+1)
fd1[c("n_tokens_title","num_hrefs","num_self_hrefs","num_imgs", 
      "num_videos","average_token_length","num_keywords")] <- scalevar(fd1[c("n_tokens_title","num_hrefs","num_self_hrefs","num_imgs", 
                                                                             "num_videos","average_token_length","num_keywords")])
n1 <- fd1[1:3000,]
t1 <- fd1[3001:13000,]
m1 <- lm(shares~.,n1)
summary(m1)                # 11.6%
step(m1, direction='both')

sb1 <- lm(formula = shares ~ n_tokens_content + n_non_stop_words + num_hrefs + 
            num_self_hrefs + average_token_length + num_keywords + data_channel_is_lifestyle + 
            data_channel_is_entertainment + data_channel_is_bus + data_channel_is_world + 
            kw_min_min + kw_avg_min + kw_avg_max + kw_max_avg + kw_avg_avg + 
            self_reference_min_shares + self_reference_max_shares + weekday_is_monday + 
            weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + 
            weekday_is_friday + LDA_00 + global_subjectivity + title_subjectivity + 
            abs_title_subjectivity, data = n1)
summary(sb1)              # 12%
round(vif(sb1),1)


fd1[c("n_tokens_content", "kw_max_min", "kw_avg_min", "kw_min_min",
      "kw_min_avg","kw_min_max","kw_max_max", "kw_avg_max",
      "kw_max_avg", "kw_avg_avg","self_reference_min_shares",
      "self_reference_max_shares","self_reference_avg_sharess", 
      "shares","n_tokens_title","num_hrefs","num_self_hrefs","num_imgs", 
      "num_videos","average_token_length","num_keywords")] <- log(fd1[c("n_tokens_content", "kw_max_min", "kw_avg_min", "kw_min_min",
                                                                        "kw_min_avg","kw_min_max","kw_max_max", "kw_avg_max",
                                                                        "kw_max_avg", "kw_avg_avg","self_reference_min_shares",
                                                                        "self_reference_max_shares","self_reference_avg_sharess", 
                                                                        "shares","n_tokens_title","num_hrefs","num_self_hrefs","num_imgs", 
                                                                        "num_videos","average_token_length","num_keywords")]+1)
n1 <- fd1[1:3000,]
t1 <- fd1[3001:13000,]
m1 <- lm(shares~.,n1)
summary(m1)                # 11.22%
step(m1, direction='both')
sb2 <- lm(formula = shares ~ n_tokens_content + n_non_stop_words + num_hrefs + 
            num_self_hrefs + average_token_length + num_keywords + data_channel_is_lifestyle + 
            data_channel_is_entertainment + data_channel_is_bus + data_channel_is_world + 
            kw_min_min + kw_avg_min + kw_avg_max + kw_max_avg + kw_avg_avg + 
            self_reference_min_shares + self_reference_max_shares + weekday_is_monday + 
            weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + 
            weekday_is_friday + LDA_00 + global_subjectivity + rate_positive_words + 
            max_positive_polarity + title_subjectivity + abs_title_subjectivity, 
          data = n1)

sb2 <- lm(formula = shares ~ n_tokens_content  + num_hrefs + 
            average_token_length + num_keywords + data_channel_is_lifestyle + 
            data_channel_is_entertainment + data_channel_is_bus + data_channel_is_world + 
            kw_min_min   + kw_max_avg + kw_avg_avg + 
            self_reference_max_shares + weekday_is_monday + 
            weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + 
            weekday_is_friday + LDA_00 + global_subjectivity  + 
            title_subjectivity + abs_title_subjectivity, 
          data = n1)

summary(sb2)              # 11.23%
residualPlots(sb2, plot=F)


##############################################
#################################################
################################################
###############################################
####cheking assumptions and transform predictors

#Q3

######################################
#####modelraw assumptions

modelraw <- lm(shares~num_hrefs+num_keywords+
                 kw_avg_avg+LDA_03+kw_max_avg+
                 title_subjectivity+data_channel_is_world+
                 data_channel_is_socmed+is_weekend+
                 weekday_is_tuesday, news3)

summary(modelraw)        #9.63%
residualPlots(modelraw, plot=F, type = "rstudent")

par(mfrow=c(1,1))
plot(modelraw, which = 2)
Stud.residuals <- rstudent(modelraw)
yhat <- fitted(modelraw)
par(mfrow=c(1,2))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2)
ncvTest(modelraw)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(modelraw)~yhat.quantiles)
boxplot(rstudent(modelraw)~yhat.quantiles)

residualPlot(modelraw, type='rstudent')
residualPlots(modelraw, plot=F, type = "rstudent")
plot(rstudent(modelraw), type='l')

round(vif(modelraw),1)

######################################
#####modelst2 assumptions

modelst2 <- lm(formula = shares ~ I(n_tokens_content^2) + num_hrefs + 
                 average_token_length + data_channel_is_lifestyle + data_channel_is_entertainment + 
                 data_channel_is_bus + data_channel_is_world + kw_min_min + 
                 kw_avg_min + kw_min_max + kw_max_avg + kw_avg_avg + 
                 self_reference_max_shares + 
                 weekday_is_friday + is_weekend + 
                 I(LDA_00^4) + 
                 LDA_03 + max_positive_polarity + 
                 title_subjectivity + abs_title_subjectivity, data = news3)
summary(modelst2)           #13.42%

par(mfrow=c(1,1))
plot(modelst2, which = 2)
Stud.residuals <- rstudent(modelst2)
yhat <- fitted(modelst2)
par(mfrow=c(1,2))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2)
ncvTest(modelst2)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(modelst2)~yhat.quantiles)
boxplot(rstudent(modelst2)~yhat.quantiles)

residualPlot(modelst2, type='rstudent')
residualPlots(modelst2, plot=F, type = "rstudent")
plot(rstudent(modelst2), type='l')

round(vif(modelst2),1)

######################################
#####modellasso assumptions

summary(modellasso)
residualPlots(modellasso, plot=F)
par(mfrow=c(1,1))
plot(modellasso, which = 2)
Stud.residuals <- rstudent(modellasso)
yhat <- fitted(modellasso)
par(mfrow=c(1,2))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2)
ncvTest(modellasso)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(modellasso)~yhat.quantiles)
boxplot(rstudent(modellasso)~yhat.quantiles)

residualPlot(modellasso, type='rstudent')
residualPlots(modellasso, plot=F, type = "rstudent")
plot(rstudent(modellasso), type='l')

round(vif(modellasso),1)


####################################
########################################
#####################################
####transform predictors again and create modelst3

fulldata4 <- fulldata3
fulldata4$min_negative_polarity <- abs(fulldata4$min_negative_polarity)
fulldata4$max_negative_polarity <- abs(fulldata4$max_negative_polarity)
fulldata4$avg_negative_polarity <- abs(fulldata4$avg_negative_polarity)
fulldata4$title_sentiment_polarity <- abs(fulldata4$title_sentiment_polarity)
fulldata4$kw_min_min <- abs(fulldata4$kw_min_min)


summary(fulldata4[,names(intnews2)])

news4 <- fulldata4[1:3000,]
test4 <- fulldata4[3001:13000,]

modelfull3 <- lm(shares~.,news4)
step(modelfull3, direction='both')

modelst3 <- lm(formula = shares ~ n_tokens_content + n_non_stop_words + num_hrefs + 
                 average_token_length + data_channel_is_lifestyle + data_channel_is_entertainment + 
                 data_channel_is_bus + data_channel_is_world + kw_min_min + 
                 kw_avg_min + kw_min_max + kw_avg_max + kw_max_avg + kw_avg_avg + 
                 self_reference_min_shares + self_reference_max_shares + self_reference_avg_sharess + 
                 weekday_is_tuesday + weekday_is_friday + is_weekend + LDA_00 + 
                 LDA_03 + global_subjectivity + rate_negative_words + max_positive_polarity + 
                 title_subjectivity + abs_title_subjectivity, data = news4)

summary(modelst3)    #13.46%

modelst3 <- lm(formula = shares ~ I(n_tokens_content^2) + num_hrefs + 
                 average_token_length + data_channel_is_lifestyle + data_channel_is_entertainment + 
                 data_channel_is_bus + data_channel_is_world + kw_min_min + 
                 kw_avg_min + kw_min_max  + log(kw_max_avg+1) + I(kw_avg_avg^2) + 
                  self_reference_max_shares   
                  + weekday_is_friday + is_weekend + 
                 I(LDA_03^2)  + I(rate_negative_words^2) + max_positive_polarity + 
                 title_subjectivity + abs_title_subjectivity, data = news4)

summary(modelst3)    #13.46%


######################################
#####modelst3 assumptions

residualPlots(modelst3, plot=F)
par(mfrow=c(3,2))
plot(modelst3, which = 2)
Stud.residuals <- rstudent(modelst3)
yhat <- fitted(modelst3)
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2)
ncvTest(modelst3)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(modelst3)~yhat.quantiles)
boxplot(rstudent(modelst3)~yhat.quantiles)

residualPlot(modelst3, type='rstudent')
residualPlots(modelst3, plot=F, type = "rstudent")
plot(rstudent(modelst3), type='l')

round(vif(modelst3),1)

#########################################
#Q4


######################################
#####create modelcv

modelcv <- train(
  shares ~ ., news3,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)


summary(modelcv) # 13.13%


######################################
#####modelcv assumptions

residualPlots(modelcv$finalModel, plot=F, type="rstudent")
par(mfrow=c(1,1))
plot(modelcv$finalModel, which = 2)
Stud.residuals <- rstudent(modelcv$finalModel)
yhat <- fitted(modelcv$finalModel)
par(mfrow=c(1,2))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2)
ncvTest(modelcv2$finalModel)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(modelcv$finalModel)~yhat.quantiles)
boxplot(rstudent(modelcv$finalModel)~yhat.quantiles)

residualPlot(modelcv$finalModel, type='rstudent')
residualPlots(modelcv$finalModel, plot=F, type = "rstudent")
plot(rstudent(modelcv$finalModel), type='l')

round(vif(modelcv$finalModel),1)

########################################
######################################
#Q5

######################################
#####modelraw predictions


predraw <- predict(modelraw, test3)
actual_pred <- data.frame(cbind(actuals=test3$shares, predicteds=predraw))
(correlation_accuracy <- cor(actual_pred))
data.frame(
  R2 = R2(predraw, test3$shares),        #7.46%
  RMSE = RMSE(predraw, test3$shares)
  )

######################################
#####modelst2 predictions

predst2 <- predict(modelst2, test3)
actual_pred <- data.frame(cbind(actuals=test3$shares, predicteds=predst2))
(correlation_accuracy <- cor(actual_pred))
data.frame(
  R2 = R2(predst2, test3$shares),        #0.95%
  RMSE = RMSE(predst2, test3$shares)
  )
(cov(predst2, test3$shares)/(var(predst2)*var(test3$shares)))^2
######################################
#####modellasso predictions

predlasso <- predict(modellasso, test3)
actual_pred <- data.frame(cbind(actuals=test3$shares, predicteds=predlasso))
(correlation_accuracy <- cor(actual_pred))
data.frame(
  R2 = R2(predlasso, test3$shares),      #4.19%
  RMSE = RMSE(predlasso, test3$shares)
  )
(cov(predlasso, test3$shares)/(var(predlasso)*var(test3$shares)))^2
cor(predlasso, test3$shares)^2
######################################
#####modelst3 predictions

predst3 <- predict(modelst3, test4)
actual_pred <- data.frame(cbind(actuals=test4$shares, predicteds=predst3))
(correlation_accuracy <- cor(actual_pred))
data.frame(
  R2 = R2(predst3, test4$shares),        #6.77%
  RMSE = RMSE(predst3, test4$shares),
  SD = sd(test4$shares)
  )
sqrt(mean((test4$shares - predst3)^2))
(cov(predst3, test4$shares)/(var(predst3)*var(test4$shares)))^2
summary(predst3)$adj.r.squared

######################################
#####modelcv predictions
predcv <- predict(modelcv, test3)
actual_pred <- data.frame(cbind(actuals=test3$shares, predicteds=predcv))
(correlation_accuracy <- cor(actual_pred))
data.frame(
  R2 = R2(predcv, test3$shares),        #9.18%
  RMSE = RMSE(predcv, test3$shares)
  )
(cov(predcv, test3$shares)/(var(predcv)*var(test3$shares)))^2
((var(test3$shares)-var(test3$shares-predcv))/(var(test3$shares)))
cor(predcv, test3$shares)^2
