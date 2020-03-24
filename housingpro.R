#--------------------------------------------------------------------------------------------------------------------------
## Real Estate Dataset using Linear Regression:
#--------------------------------------------------------------------------------------------------------------------------
#Set Directory

setwd('C:\\Users\\NIlesh\\Desktop\\Edvancer\\R Fundamentals\\Project 1-Real Estate')

getwd()

#Import the Datasets:

re_train=read.csv('housing_train.csv',stringsAsFactors = F)
re_test=read.csv('housing_test.csv',stringsAsFactors = F)

View(re_train) #7536 rows and 16 column
View(re_test) #1885 rows and 15 column


re_test$Price=NA #1885 NA in Price Test dataset.

re_test$data='test'
re_train$data='train'

re_all=rbind(re_train,re_test)

View(re_all) #7536 rows(train)+1885 rows(test)=9421 Rows.

#Create Dummy Variables.
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

#Lets Look at the NA Values in the Dataset:

lapply(re_all,function(x) sum(is.na(x)))

#We will Drop the BuildingArea and YearBuilt as it is Mostly Missing.
library(dplyr)
glimpse(re_all)

re_all=re_all %>% select(-c(BuildingArea,YearBuilt))

#Lets Impute the Missing Values:

for(col in names(re_all)){
  
  if(sum(is.na(re_all[,col]))>0 & !(col %in% c("data","Price"))){
    
    re_all[is.na(re_all[,col]),col]=round(mean(re_all[re_all$data=='train',col],na.rm=T))
  }
}
  


lapply(re_all, function(x) sum(is.na(x))) #There are NA Values only in  the Price Column.

glimpse(re_all)

#Convert Character Variables into Dummy variables:

names(re_all)[sapply(re_all,function(x) is.character(x))]

lapply(re_all,function(x) length(unique(x)))

#As Adress has too many Unique Values we will Drop that Column and Create Dummies for rest of the Variables:

re_all=re_all %>% select(-Address)

names(re_all)[sapply(re_all,function(x) is.character(x))]

#cat_cols=c("Suburb" ,"Type" ,"Method" ,"SellerG","CouncilArea")

sort(table(re_all$Suburb),decreasing = T) #cut off = 90
table(re_all$Type)  #cut off=0
sort(table(re_all$Method),decreasing =T) #cut off=600
sort(table(re_all$SellerG),decreasing = T) #cut off=70,100
sort(table(re_all$CouncilArea),decreasing = T) #cut off=300,400

View(re_all)


#Create Dummy Variables for the Character Variables:

re_all=CreateDummies(re_all,'Suburb',90)
re_all=CreateDummies(re_all,'Type')
re_all=CreateDummies(re_all,'Method',600)
re_all=CreateDummies(re_all,'SellerG',70)
re_all=CreateDummies(re_all,'CouncilArea',300)
lapply(re_all,function(x) sum(is.na(x))) #1885 data Column in Test Dataset.
glimpse(re_all)


#Seperate the Train and Test Dataset to Create Training Dataset and Validation Dataset from the Train Dataset.
library(dplyr)
re_train=re_all %>%  filter(data=='train') %>% select(-data)
re_test=re_all %>% filter(data=='test') %>% select(-data,-Price)


#Create Samples to Train the Data Set.

set.seed(2)
s=sample(1:nrow(re_train),0.7*nrow(re_train))
re_train1=re_train[s,]
re_train2=re_train[-s,]


View(re_train1)
View(re_train2)
glimpse(c(re_train1,re_train2))

#Create an Linear REgression model from Train Data 1:
library(car)
fit=lm(Price~.,data=re_train1)
sort(vif(fit),decreasing = T)[1:10]

#We will detect the Variance Inflation.


#library(tidyr)

sort(vif(fit),decreasing = T)

fit=lm(Price~.,data=re_train1)

#We will use Step Function to get Significant Variables for the Linear Model:


fit=step(fit)

#Lets Predict the Values:

summary(fit)

fit=step(fit)

formula(fit)

fit=lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
   Suburb_Toorak + Suburb_Maribyrnong + Suburb_Doncaster + 
  Suburb_MooneePonds + Suburb_Thornbury + Suburb_Hampton + 
  Suburb_Balwyn + Suburb_MalvernEast + Suburb_Camberwell + 
  Suburb_PortMelbourne + Suburb_BrightonEast + Suburb_Hawthorn + 
  Suburb_BalwynNorth + Suburb_Brighton + Suburb_Essendon + 
  Suburb_SouthYarra + Suburb_StKilda + Suburb_Preston + Suburb_Richmond + 
  Suburb_Reservoir + Type_u + Type_h + Method_SP + Method_S + 
  SellerG_Williams + SellerG_Kay + SellerG_Miles + SellerG_Greg + 
  SellerG_RT + SellerG_Fletchers + SellerG_Biggin + SellerG_Marshall + 
  SellerG_Jellis + CouncilArea_Banyule + CouncilArea_Darebin + 
  CouncilArea_Moreland + CouncilArea_Boroondara + CouncilArea_,
       data=re_train1)

summary(fit)

###----------------------------------------------------------------------------------------------------------


val_score=predict(fit,newdata = re_train2)
val_score
View(round(val_score))

Val=re_train2$Price-round(val_score)

Val_1=Val**2 %>% mean() %>% sqrt

Score=212467/Val_1
Score #0.5347099

#Build a LInear Model of the Train Data:
fit_final=lm(Price~.,data=re_train)

sort(vif(fit_final),decreasing =T )

fit_final=step(fit_final)

summary(fit_final)

formula(fit_final)

fit_final=lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
               Landsize + Suburb_Toorak + Suburb_Maribyrnong + Suburb_Doncaster + 
               Suburb_MooneePonds + Suburb_Thornbury + Suburb_Hampton + 
               Suburb_Balwyn + Suburb_MalvernEast + Suburb_Camberwell + 
               Suburb_PortMelbourne + Suburb_BrightonEast + Suburb_Hawthorn + 
               Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + Suburb_Essendon + 
               Suburb_SouthYarra + Suburb_StKilda + Suburb_Preston + Suburb_Richmond + 
               Suburb_Reservoir + Type_u + Type_h + Method_SP + 
               Method_S + SellerG_Williams +  SellerG_Kay + 
               SellerG_Miles + SellerG_Greg + SellerG_RT + SellerG_Fletchers + 
                 SellerG_Marshall +  
               SellerG_Jellis + CouncilArea_Banyule + CouncilArea_PortPhillip + 
              CouncilArea_Darebin + CouncilArea_Moreland + 
               CouncilArea_Boroondara + CouncilArea_,data=re_train)


test_pred_score=predict(fit_final,newdata = re_test)
View(test_pred_score)

class(test_pred_score)
test_pred_score=data.frame(Price=round(test_pred_score)) #Since the Price is not is Decimal and its a Pure Number we will use round Function.
View(test_pred_score)

#As the Values are in Decimal we will Round it and SUbmit:
write.csv(test_pred_score,'Nilesh_Ohol_P1_part2.csv',row.names = F)
