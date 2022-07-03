Dataframe<-Linear_Regression_Case
View(Dataframe)
## CREATE A RESPONSE VARIABLE TOTALSPEND AND REMOVING UNWANTED VARIABLES.
Dataframe$TotalSpend<-Dataframe$cardspent+Dataframe$card2spent
Dataframe1<-Dataframe[,-c(1,79,81)]

##COLLECT ALL THE NUMERICAL VARIABLES
#Numerical columns:
Dataframe2<-Dataframe1[c("age","ed","income","lninc","employ","debtinc","creddebt",
                         "lncreddebt","othdebt","address","commute","lnothdebt","spoused",
                         "carvalue","commutetime","tenure","longmon","lnlongmon","longten",
                         "tollmon","tollten","lntollmon","equipmon", "lnequipmon", "equipten",
                         "lnequipten","cardmon","lncardmon","cardten","lncardten","wiremon",
                         "lnwiremon","wireten","lnwireten","hourstv","TotalSpend")]

#Converting categorical variables into 'Factor' data type
Dataframe3<-data.frame(lapply(Dataframe[c('region','townsize','gender','agecat','edcat','jobcat',
                                 'union','empcat','retire','inccat','default','jobsat','marital','spousedcat',
                                 'homeown','hometype','addresscat','cars','carown','cartype','carcatvalue','carbought',
                                 'carbuy','commutecat','commutecar','commutemotorcycle','commutecarpool','commutebus',
                                 'commuterail','commutepublic','commutebike','commutewalk','commutenonmotor', 'telecommute',
                                 'reason', 'polview','polparty', 'polcontrib', 'vote','card','cardtype','cardbenefit', 'cardfee', 
                                 'cardtenure','cardtenurecat', 'card2', 'card2type', 'card2benefit','card2fee', 'card2tenure', 
                                 'card2tenurecat', 'active','bfast','churn', 'tollfree', 'equip', 'callcard', 'wireless', 'multline', 
                                 'voice', 'pager', 'internet', 'callid','callwait', 'forward', 'confer', 'ebill', 'owntv', 'ownvcr',
                      'owndvd', 'owncd','ownpda', 'ownpc', 'ownipod', 'owngame', 'ownfax', 'news','response_01','response_02','response_03')],as.factor))

## HERE WE WILL COMBINE "Dataframe2" & "Dataframe3".
df <-cbind(Dataframe2,Dataframe3)
class(df)


##FIND THE STRUCTURE OF THE DATAFRAME.
str(df)


##NORMALITY TEST
qqnorm(df$TotalSpend, main="NOT NORMAL")

NOTE:-
THE POINTS ARE NOT  SHOWING IN THE STRAIGHT LINE. SO THAT WE CAN SAY DATA IS NOT NORMAL.




##SHAPRIO WILKS TEST
shapiro.test(df$TotalSpend)

##MEAN NORMALISATION
hist(sqrt(sqrt(df$TotalSpend)), main='Normal')

NOTE:-
ON THE BASIS OF THE ABOVE DIAGRAM WE CAN SAY THAT DATA IS NOMALLY DISTRIBUTED.




###DOUBLE SQUAREROOT
df$TotalSpend<-sqrt(sqrt(Dataframe1$TotalSpend))




##FIND OUTLIERS
boxplot(df$employ)
boxplot(df$income)
boxplot(df$commute)
boxplot(df$address)

NOTE:-
REPLACING THESE FOUR OUTLIERS WITH THIRD QUARTILE .

## 1-REPLACEMENT 

EMPLOY
upperwhisker_EMPLOY <- quantile(df$employ,0.75)+1.5*(quantile(df$employ,0.75)-quantile(df$employ,0.25))

df$employ[df$employ>upperwhisker_EMPLOY]<-quantile(df$employ,0.75)
boxplot(df$employ)



INCOME
upperwhisker_INCOME<-quantile(df$income,0.75)+1.5*(quantile(df$income,0.75)-quantile(df$income,0.25))

df$income[df$income>upperwhisker_INCOME]<-quantile(df$income,0.75)
boxplot(df$income)



ADDRESS
upperwhisker_ADDRESS<-quantile(df$address,0.75)+1.5*(quantile(df$address,0.75)-quantile(df$address,0.25))

df$address[df$address>upperwhisker_ADDRESS]<-quantile(df$address,0.75)
boxplot(df$address)



COMMUTE
upperwhisker_COMMUTE<-quantile(df$commute,0.75)+1.5*(quantile(df$commute,0.75)-quantile(df$commute,0.25))

df$commute[df$commute>upperwhisker_COMMUTE]<-quantile(df$commute,0.75)
boxplot(df$commute)




## 2- MISSING VALUES TREATEMENT
#TO CHECK MISSING VALUES

#Sum of total missing values in columns.
colSums(is.na(df))%>%View

#replacement of missing values with median and mean.

INCREDDEBT 
MEAN_I<-mean(df$lncreddebt, na.rm=T)

df$lncreddebt= ifelse(is.na(df$lncreddebt),MEAN_I,df$lncreddebt)
any(is.na(df$lncreddebt))



LNOTHDEBT 
MEAN_L<-mean(df$lnothdebt, na.rm=T)

Dataframe1$lnothdebt= ifelse(is.na(Dataframe1$lnothdebt), MEAN_L,Dataframe1$lnothdebt)
                       
any(is.na(Dataframe1$lnothdebt))




COMMUTETIME
MEAN_C<-mean(df$commutetime, na.rm=T)

df$commutetime= ifelse(is.na(df$commutetime), MEAN_C,df$commutetime)
                     
any(is.na(df$commutetime))



LONGTEN 
MEAN_L<-mean(df$longten, na.rm=T)

df$longten= ifelse(is.na(df$longten), MEAN_L,df$longten)
                 
any(is.na(df$longten))



INTOLLMON 
mean(df$lntollmon, na.rm=T)
df$lntollmon= ifelse(is.na(df$lntollmon),
                     mean(df$lntollmon, na.rm = T),df$lntollmon)
any(is.na(df$lntollmon))



INEQUIPMON  
MEAN_I2<-mean(df$lnequipmon, na.rm=T)

df$lnequipmon= ifelse(is.na(df$lnequipmon),   MEAN_I2,df$lnequipmon)
                  
any(is.na(df$lnequipmon))



INEQUIPTEN  
MEAN_I3<-mean(df$lnequipten, na.rm=T)

df$lnequipten= ifelse(is.na(df$lnequipten), MEAN_I3,df$lnequipten)
                     
any(is.na(df$lnequipten))



CARDTEN 
MEAN_C2<-mean(df$cardten, na.rm=T)

df$cardten= ifelse(is.na(df$cardten), MEAN_C2,df$cardten)
                
any(is.na(df$cardten))



INCARDMON 
MEAN_I5<-mean(df$lncardmon, na.rm=T)

df$lncardmon= ifelse(is.na(df$lncardmon), MEAN_I5,df$lncardmon)
                    
any(is.na(df$lncardmon))





INWIREMON 
MEAN_I6<-mean(df$lnwiremon, na.rm=T)

df$lnwiremon= ifelse(is.na(df$lnwiremon), MEAN_I6,df$lnwiremon)
                   
any(is.na(df$lnwiremon))



INCARDTEN 
MEAN_17<-mean(df$lncardten, na.rm=T)

df$lncardten= ifelse(is.na(df$lncardten),  MEAN_17,df$lncardten)
                  
any(is.na(df$lncardten))




INWIRETEN
MEAN_18<-mean(df$lnwireten, na.rm=T)

df$lnwireten= ifelse(is.na(df$lnwireten), MEAN_18,df$lnwireten)
                    
any(is.na(df$lnwireten))




TOWNSIZE
MEAN_T<-mean(df$townsize,na.rm=T)

df$townsize=ifelse(is.na(df$townsize),MEAN_T,df$townsize)

any(is.na(df$townsize))




##FITTING MODELS WITH THE VARIABLES and CREATE REGRESSION MODEL
class(df)
model<-lm(TotalSpend~.,data=df)
summary(model)




#REMOVE ALL THE INSIGNIFICANT VARIABLES  
```{r}
library(dplyr)
Dataframe_1 = df %>% select(spoused,age,lninc,edcat,jobcat,union,marital,agecat,card2benefit,region,reason,polview,card,spousedcat,card2, TotalSpend)
View(Dataframe_1)

lr_model = lm(TotalSpend~.,data=Dataframe_1)
summary(lr_model)



imcdiag(lr_model)

#DROPPING THE VAIABLES WHICH ARE HAVING VIF>5

Dataframe_2 = Dataframe_1%>%select(agecat,union,reason,edcat,lninc,polview,card,card2,marital, TotalSpend)
View(Dataframe_2)

lr_model_2=lm(TotalSpend~., data=Dataframe_2)
summary(lr_model_2)

#VIF
imcdiag(lr_model_2)




#NEW DATASETS
Dataframe_new = Dataframe_2
Dataframe_new%>%View



#BREAK THE DATATEST INO TRAIN ANF TEST
set.seed(123)

Split= sample.split(Dataframe_new$TotalSpend,SplitRatio = 2/3)

Train=subset(Dataframe_new, Split==T)

Test=subset(Dataframe_new, Split==F)


#FINAL MODEL
lr_model_final <- lm(TotalSpend~., data = Train)
final=summary(lr_model_final)
final%>%View



#PREDICTIONS FOR TOTALSPEND
Predictions<- predict(lr_model_final, newdata = Test)
Predictions%>%View


#ADDING PREDICT COLUMN 
Test$predict<- Predictions
Test%>%View


#CONVERSION OF TOTALSPEND COULUMN
Test$TotalSpend<-Test$TotalSpend^4
Test%>%View


#CONVERSION OF PREDICT COULUMN

Test$predict<-Test$predict^4
Test%>%View


#ERROR
Test$Error<- Test$TotalSpend - Test$predict
Test%>%View


#ERROR TESTING 
qqnorm(Test$Error)



#MEAN OF ABSOLUTE PERCENATEG
MAPE(Test$TotalSpend,Test$predict)

#MAPE = 45.4%
#ACCURACY = 55% (LOW)
NOTE:-IT MEANS THE DIFFERENCE BETWEEN THE PREDICTED VALUE AND ACTUAL IS 45%.












