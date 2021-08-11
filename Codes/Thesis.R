Packages <-(c('readxl','mlr','minerva','dplyr'))
lapply(Packages, library, character.only = TRUE)

require(languageserver)
library(energy)
require(dplyr)
require(caret)
library(minerva)
library(pROC)
library(mlr)
require(ggplot2) # loads packages 
library(e1071) # NEW
library(readxl)

setwd("C:\\Users\\alexx\\Documents\\Untitled Folder")
df1 <- read.csv("SALES.csv") 
df2 <- read.csv("TOPICS.csv") 
df3 <- read.csv("KEYWORDS_with_labels.csv") 
df4 <- read.csv("Other_variables.csv") 

dim(df1)+dim(df2)+dim(df3)+dim(df4) #276 12873

sum(is.na(df1)) # 0
sum(is.na(df2)) # 0
sum(is.na(df3)) # 0
sum(is.na(df4)) # 0

df_spike = df1[ c(1,2)] # date & spike
df_sale_pp = df1[ c(1,3)] # date & sale_pp

df1 <- removeConstantFeatures(df1, perc = 0.02) # 42 columns removed
df2 <- removeConstantFeatures(df2, perc = 0.02) # 479 columns removed
df3 <- removeConstantFeatures(df3, perc = 0.02) # 728 columns removed

dim(df1)+dim(df2)+dim(df3)+dim(df4) #276 11624


setwd("C:\\Users\\alexx\\Documents\\Untitled Folder\\results731")

#### df ####
df <- merge(df1,df2,by="date")
df <- merge(df,df3,by="date")
df <- merge(df,df4,by="date")
df$date <- NULL
df$sale_pp <- NULL

df_bi = Filter(function(x) all(x %in% c(0, 1)), df)
df_num = Filter(function(x) !all(x %in% c(0, 1)), df)

dim(df_bi) # 70 4704
dim(df_num) # 70 6915

var2trans <- c(colnames(df_num))

# Function to transform variables in binaries
change <- function(x){
  
  z <- which(names(df) == x)
  
  Q1 <- summary(df[,z]) [2]
  Q3 <- summary(df[,z]) [5]
  mean <- summary(df[,z]) [4]
  
  df$l <<- ifelse(df[,z] < Q1,1,0); df$l <<- as.factor(df$l)
  colnames(df)[ncol(df)] <<- paste(x,".low",sep="")
  df$m <<- ifelse(df[,z] >= Q1 & df[,z] <= Q3,1,0); df$m <<- as.factor(df$m)
  colnames(df)[ncol(df)] <<- paste(x,".medium",sep="")
  df$h <<- ifelse(df[,z] > Q3,1,0); df$h <<- as.factor(df$h)
  colnames(df)[ncol(df)] <<- paste(x,".high",sep="")
  
  df[,z] <<- NULL
  
}

# Change all now through looping
for (i in var2trans) {
  change(i)
}

str(df)


# Function to transform integers & numeric in factors
changeF <- function(x){
  
  z <- which(names(df) == x)
  
  df[,z] <<- as.factor(df[,z])
  
}

# List of integers and numeric that need to be transformed in factors
num2fac <- c(colnames(df))

# Now change this
for (i in num2fac) {
  changeF(i)
}

str(df)

dim(df) # 69 25449


write.csv(df,"features_after_changes.csv", row.names = FALSE)


#### Log Odds ####

dfxx <- df
str(dfxx)
dfxx$spike <- as.numeric(as.character(dfxx$spike))
variables <- colnames(dfxx) # get all variable names

## Create Means
dfxx[variables] <- lapply(dfxx[variables], as.numeric)
means_overall <- sapply(dfxx[colnames(dfxx) %in% variables],FUN=mean)
counts_overall <- sapply(dfxx[colnames(dfxx) %in% variables],FUN=sum)

success_N <- sum(dfxx$spike)
fail_N <- sum(!dfxx$spike)

means_profile <- aggregate(dfxx[colnames(dfxx) %in% variables], by=list(dfxx$spike),FUN=mean)
means_profile <- t(means_profile)
means_profile <- means_profile[-1, ]

count_profile <- aggregate(dfxx[colnames(dfxx) %in% variables], by=list(dfxx$spike),FUN=sum)
count_profile <- t(count_profile)
count_profile <- count_profile[-1, ]

odds_data <- data.frame(variables,nrow(dfxx),success_N,fail_N,counts_overall,count_profile[,1], count_profile[,2], means_overall,means_profile[,1],means_profile[,2])
colnames(odds_data) <- c("variables","cases","total_successes","total_failures","hiper_count_overall","hiper_count_fail","hiper_count_success","mean_overall","mean_fail", "mean_success")

odds_data$success_odds <- odds_data$hiper_count_success/(odds_data$total_successes-odds_data$hiper_count_success)
odds_data$success_odds[which(!is.finite(odds_data$success_odds))] <- 0
odds_data$fail_odds <- odds_data$hiper_count_fail/(odds_data$total_failures-odds_data$hiper_count_fail)
odds_data$odds_ratio <- odds_data$success_odds/odds_data$fail_odds
odds_data$odds_ratio[is.na(odds_data$odds_ratio)]<-0
odds_data$log10_odds_ratio <- log10(odds_data$odds_ratio)
odds_data$log10_odds_ratio[which(!is.finite(odds_data$log10_odds_ratio))] <- 0
write.csv(odds_data, "logOdds.csv")





#### Maximum Information Coefficient ####

str(dfxx)

x <- round(runif(10000,0,nrow(dfxx)))

dfmic <- dfxx[x,]


micres<-sapply(2:ncol(dfmic), 
               function(x) mine(dfmic[, 1], dfmic[, x]))

micres<-cbind(micres)
micres<-t(micres)

write.csv(micres, "MIC.csv")
micres <- read.csv("MIC.csv")
micres$variable <- colnames(dfmic)[2:ncol(dfmic)] 
write.csv(micres, "MIC.csv") # back and forth because of hidden elements in matrix.



#### select from MIC+LogOdds ####

MIC_LogOdds <- read_excel("MICLogOdds.xlsx") 
df_new <- df[, MIC_LogOdds$X]
df_new <- cbind(df_spike, df_new)
df_new$date <- NULL
dim(df_new) # 69 580

#### alias ####

library(caret)

data1 <- df_new[1:60]
data2 <- df_new[c(1, 61:120)]
data3 <- df_new[c(1,121:180)]
data4 <- df_new[c(1,181:240)]
data5 <- df_new[c(1,241:300)]
data6 <- df_new[c(1,301:360)]
data7 <- df_new[c(1,361:420)]
data8 <- df_new[c(1,421:480)]
data9 <- df_new[c(1,481:540)]
data10 <- df_new[c(1,540:580)]

full1=glm(spike~., data = data1, family = binomial)
full2=glm(spike~., data = data2, family = binomial)
full3=glm(spike~., data = data3, family = binomial)
full4=glm(spike~., data = data4, family = binomial)
full5=glm(spike~., data = data5, family = binomial)
full6=glm(spike~., data = data6, family = binomial)
full7=glm(spike~., data = data7, family = binomial)
full8=glm(spike~., data = data8, family = binomial)
full9=glm(spike~., data = data9, family = binomial)
full10=glm(spike~., data = data10, family = binomial)

A1 <- alias(full1)
A2 <- alias(full2)
A3 <- alias(full3)
A4 <- alias(full4)
A5 <- alias(full5)
A6 <- alias(full6)
A7 <- alias(full7)
A8 <- alias(full8)
A9 <- alias(full9)
A10 <- alias(full10)


# drop features accordingly

drop <- c('keywords_df7653.medium','keywords_df8158.medium','keywords_df7776.medium','keywords_df8281.medium','keywords_df7464.medium',
          'keywords_df7969.medium','keywords_df7985.medium','keywords_df7480.medium','interval.peak.homework.and.study.tips.medium',                      
'Lag1.interval.peak.traveling.with.kids.medium', 'Roll3.school.supplies.medium','DiffRoll3.school.supplies.medium',                    
'Lag2.interval.peak.business.software.medium', 'keywords_df8179.medium', 'keywords_df7674.medium', 'keywords_df7843.medium',                  
'Lag2.interval.peak.vehicle.brands.medium', 'keywords_df7338.medium', 'DiffRoll3.teaching.and.classroom.resources.medium',                   
'Lag3.interval.peak.armed.forces.medium', 'interval.peak.weather.medium', 'keywords_df7832.medium', 'keywords_df7327.medium',                   
'Roll3.interval.peak.education.medium', 'keywords_df7716.medium', 'keywords_df8221.medium','keywords_df7326.medium',                    
'Roll3.interval.peak.armed.forces.medium' , 'keywords_df7831.medium', 'keywords_df1789.medium', 'keywords_df3110.medium',                
'Roll3.interval.peak.business.software.medium' ,  'keywords_df7866.medium', 'keywords_df7361.medium',                 
'Roll3.interval.peak.traveling.with.kids.medium', 'keywords_df2683.medium', 'keywords_df1362.medium',
'interval.peak.education.medium','Lag1.interval.peak.education.medium', 'keywords_df7852.medium', 'keywords_df7347.medium',
'Lag1.interval.peak.armed.forces.medium','Lag1.interval.peak.homework.and.study.tips.medium',
'Lag2.interval.peak.traveling.with.kids.medium', 'DiffLag2.heating.moisturizing_share.medium','DiffLag1.heating.moisturizing_share.medium',
'Lag3.interval.peak.business.software.medium','Lag3.interval.peak.vehicle.brands.medium','keywords_df2733.medium',
'keywords_df1412.medium','keywords_df8110.medium','keywords_df7605.medium','cold.and.flu',
'Lag2.cold.and.flu1','Lag3.cold.and.flu','Roll3.interval.peak.law..govt.and.politics.high','Lag1.cold.and.flu',
'interval.peak.green.solutions.medium',  'Lag3.yes.high', 'DiffLag1.ten_thirty_package_number.low',                   
'Lag1.interval.peak.weather.medium', 'DiffLag2.ten_thirty_package_number.low', 'Roll3.urethane.high',                   
'Lag2.interval.peak.education.medium', 'Roll3.over_thirty_package_number.high', 'Lag3.three_ten_package_number.high',                  
'Lag2.interval.peak.armed.forces.medium',  'Roll3.three_ten_package_number.high', 'Lag3.over_thirty_package_number.high',                  
'Lag2.interval.peak.homework.and.study.tips.medium', 'smaller.high', 'Lag2.epidemic',                
'Lag3.interval.peak.traveling.with.kids.medium','dogs','keywords_df7651.low', 'keywords_df4172.low',
'interval.peak.government.medium', 'Roll3.regular.high','keywords_df7651.low','keywords_df7515.low',
'interval.peak.shopping.low' , 'keywords_df2849.low',  'keywords_df7651.low','Roll3.heating.moisturizing_share.low',                                         
'interval.peak.government.low' ,   'Roll3.bigger.high',  'Roll3.kids.high',  'Roll3.smaller.high',  'Roll3.no.high',                               
'interval.peak.green.solutions.low' , 'keywords_df1023.low',  'keywords_df7515.low',  'keywords_df7516.low',                                
'Lag1.interval.peak.news.low', 'keywords_df7651.low', 'Lag3.heating.moisturizing_share.low',                                      
'Lag1.interval.peak.national.news.low', 'Roll3.smaller.high',  'Roll3.bigger.high',                             
'Lag1.interval.peak.weather.low'  , 'Lag2.kids.high',  'Lag2.kids.high',  'Lag2.heating.moisturizing_share.low',                              
'Lag2.interval.peak.education.low',  'keywords_df1023.low',   'ten_thirty_package_number.high', 'Lag1.urethane.high',                               
'Lag2.interval.peak.armed.forces.low' ,  'over_thirty_package_number.high',
'Lag2.interval.peak.homework.and.study.tips.low',  'DiffLag1.aids.and.hiv.low',
'Lag2.interval.peak.shows.and.events.low'  , 'DiffLag2.interval.peak.homework.and.study.tips.high',                           
'Lag3.interval.peak.traveling.with.kids.low',  'DiffLag3.aids.and.hiv.low',
'Lag3.regular.high', 'Lag3.smaller.high', 'Lag3.no.high', 'Roll3.interval.peak.walking.high',
'Lag3.epidemic', 'Lag3.teaching.and.classroom.resources.high','Roll3.interval.peak.family.and.parenting.high',
'Lag1.three_ten_package_number.high','Lag1.zero_three_package_number.high', 'Lag2.three_ten_package_number.high',
'Roll3.zero_three_package_number.high','interval.peak.education.low', 'interval.peak.armed.forces.low','Lag1.interval.peak.traveling.with.kids.low',                                      
'Lag2.interval.peak.business.software.low','Lag2.interval.peak.vehicle.brands.low','Lag3.interval.peak.armed.forces.low',                                     
'Roll3.interval.peak.education.low', 'keywords_df6201.high',  'keywords_df1729.low','Roll3.interval.peak.armed.forces.low', 
'keywords_df7271.low','Roll3.interval.peak.business.software.low', 'DiffLag3.interval.peak.energy.high','Roll3.interval.peak.traveling.with.kids.low', 
'keywords_df7985.low','Roll3.interval.peak.vehicle.brands.low','Lag3.school.supplies',
'Lag1.over_thirty_package_number.high', 'Lag1.ten_thirty_package_number.high', 'Lag2.nonwoven.high',
'DiffLag3.interval.peak.vehicle.brands.high','Lag1.interval.peak.food.and.drink.high','Lag3.interval.peak.food.and.drink.high',
'Roll3.interval.peak.food.and.drink.high','Roll3.dogs.medium','Roll3.homework.and.study.tips.medium','DiffLag1.homework.and.study.tips.medium',
'DiffLag2.teaching.and.classroom.resources.medium','keywords_df7545.medium','keywords_df7673.medium','keywords_df8290', 'keywords_df4056.medium',
'keywords_df7349.medium','DiffRoll3.heating.moisturizing_share.medium','keywords_df7854.medium','keywords_df8030.medium','keywords_df6339.medium',
'keywords_df7525.medium','DiffRoll3.aids.and.hiv.medium','interval.peak.homework.and.study.tips.low','Lag1.heating.moisturizing_share.low',
'interval.peak.national.news.low','Lag1.epidemic','Lag1.smaller.high','Lag2.smaller.high','heating.moisturizing_share.low',
'interval.peak.weather.low','urethane.high','DiffRoll3.interval.peak.auto.parts.high','DiffLag2.interval.peak.auto.parts.high',
'Lag1.interval.peak.education.low', 'DiffRoll3.interval.peak.vegetarian.high', 'DiffLag2.interval.peak.vegetarian.high',  
'Lag1.interval.peak.armed.forces.low', 'aids.and.hiv','DiffRoll3.aids.and.hiv.low', 'DiffRoll3.homework.and.study.tips.medium',
'Lag1.interval.peak.homework.and.study.tips.low','Lag1.interval.peak.shows.and.events.low','Lag2.interval.peak.traveling.with.kids.low',               
'Lag3.interval.peak.business.software.low','Lag3.interval.peak.vehicle.brands.low','interval.peak.news.low')


df_new = df_new[,!(names(df_new) %in% drop)]
dim(df_new) # 69 401

write.csv(df_new, "features_from_MIC_OddLogs_alias.csv") 



#### similarity matrix for data above ####
res_t <- data.frame()
for(i in seq(ncol(df_new))){
  z <- df_new[,i]
  z <- apply(df_new,2,function(x){sum(x==z)/length(z)})
  res_t <- rbind(res_t,z)
}
colnames(res_t) <- colnames(df_new)
rownames(res_t) <- colnames(df_new)
res_t <- as.matrix(res_t) 
write.csv(res_t, "CorMatrix.csv") 


# delete from results of matrix
Matrix_to_delete <- read_excel("Matrix_to_delete.xlsx")  
df_new <- df_new[, Matrix_to_delete$V2]
dim(df_new) #69 163

write.csv(df_new,"features_from_Matrix.csv", row.names = FALSE)





#### I. MIC-LogOdds-RF-StepWise ####
# !clean objects & console first!

df <- read.csv("features_from_MIC_OddLogs.csv") 
df$X <- NULL
dim(df) #69 580

RF <- read.csv("I-RF-features.csv") 
df_new <- df[, RF$X]
df_new <- cbind(df$spike, df_new)
names(df_new)[names(df_new) == "df$spike"] <- "spike"

dim(df_new) #69 181

null=glm(df_new$spike~1, data = df_new, family = binomial)
full=glm(df_new$spike~., data = df_new, family = binomial)
fwd = step(null, scope = list(lower=null, upper=full), direction = "forward")
bkd = step(full, direction = "backward")

f <- summary(fwd) # Roll3.interval.peak.food.and.drink.high + keywords_df6844.medium + keywords_df7832.low
b <- summary(bkd) #Lag3.interval.peak.food.and.drink.high + keywords_df7653.medium + keywords_df6844.medium + keywords_df2683.medium

capture.output(b, file = "BackwardStepwise_I.csv")
capture.output(f, file = "ForwardStepwise_I.csv")




#### individual significance of featuers ####

solo=glm(spike~  
             #keywords_df300 #negative 武漢
           #keywords_df3450 #Lag2 negative 武漢
           #keywords_df1736.high #pct count 武漢
         #Lag1.Brand39
         #Brand39
           
          # Lag3.Brand31
        #Roll3.epidemic.high

#X.health.and.fitness.disease
#keywords_df1231.high #count 武漢
    ###################################################Lag1.nonwoven.high
   ##########################################Lag1.other_share.low
  #####################################################nonwoven.high
 ###########################Lag1.regular.high
#Lag2..health.and.fitness.disease
#Lag3.Brand39
###############################################keywords_df7723.high #Roll3 count 武漢
#####################################################regular.high
###########################other_share.low
##############no.high
##############Lag1.no.high
##############Lag2.regular.high
##############Lag2.no.high
##############Roll3.nonwoven.high
##############Roll3.other_share.low
##############Lag2.other_share.low
##############Roll3.nonwoven_share.high

##############keywords_df7661.high #Roll3 count 新型コロナウイルス
##############Roll3.yes.high
##############Roll3.ten_thirty_package_number.high
#keywords_df4771 #Lag3 negative 武漢
##############Lag2.over_thirty_package_number.high
##############Lag3.nonwoven_share.high
##############interval.peak.travel.low
#keywords_df1556.high pct count 中国
##############yes.high
##############Lag3.other_share.low
##############DiffLag3.ten_thirty_package_number.low
#Lag3..health.and.fitness.disease
#keywords_df1736.low #pct count 武漢
#Lag2.Brand32


#	keywords_df7218.low #DiffLag3 count 武漢
#	Roll3.interval.peak.travel.low
#	Lag2..health.and.fitness.disease.epidemic
#	Roll3.cloth.high
#	Lag1.health.and.fitness.high
#	Lag1..health.and.fitness.disease
##################################	keywords_df7535.high #Roll3 count ワクチン
#	Lag1.Brand32
#	keywords_df1829.low #Lag1 positive_label_count
#	nonwoven_share.high
#	Lag1.nonwoven_share.high
#	Lag1.kids.high
#	Lag2.nonwoven_share.high
#	Lag2.urethane.high
#	Lag3.nonwoven.high
#	DiffLag2.three_ten_package_number.low
#	Lag1.yes.high
#	DiffLag3.three_ten_package_number.low
#	keywords_df2995.high #Lag1 count 新型コロナウイルス
#	keywords_df7311.low #Roll3 positive_label_count
#	three_ten_package_number.high
#	zero_three_package_number.high
#	Lag2.yes.high
#	Lag2.ten_thirty_package_number.high
#	kids.high
#	Lag3.kids.high
#	Lag1..health.and.fitness.disease.epidemic
#	Lag2.interval.peak.travel.low
#	Lag3.no_share.high
#	keywords_df5050 #Lag3 positive 中国
#	Lag2.cloth.high
#	DiffLag3.yes.low
#	disease.high
#	Lag3.interval.peak.food.low
#	Lag3.Brand32
#	DiffRoll3.ten_thirty_package_number.low
#	keywords_df3150.low #Lag2 positive_label_count
#	keywords_df4473.low #Lag3 positive_label_count
#	Lag3.cloth.high
#	Lag2.interval.peak.food.low
#	keywords_df4471.high #Lag3 negative_label_count
#	Lag3.Brand45
keywords_df1674.high
#	Roll3.disease.high
#	keywords_df4318.high
#	keywords_df1051.high
#	X.health.and.fitness.disease.cold.and.flu
#	Lag2..health.and.fitness.disease.cold.and.flu
#	Lag2.cold.and.flu
#	Roll3.health.and.fitness.high
#	Roll3.teaching.and.classroom.resources.high
#	Lag1.disease.high
#	Lag3..health.and.fitness.disease.epidemic
#	Lag3.interval.peak.travel.low
#	keywords_df8228.low
#	Lag1.bigger.high
#	gauze.high
#	Lag3.ten_thirty_package_number.high
#	Lag2.Brand45
#	Lag3.urethane.high
#	Roll3.gauze.high
#	Lag3.interval.peak.shopping.high
#	Lag2.bigger.high
#	keywords_df7156.low
#	keywords_df2.low
#	DiffLag2.smaller.low
#	Lag2.zero_three_package_number.high
#	DiffLag2.yes.low
#	Lag1.interval.peak.law..govt.and.politics.high
#	Lag2.health.and.fitness.high
#	Lag2.disease.high
#	Roll3.interval.peak.shows.and.events.low
#	Lag1..health.and.fitness.disease.cold.and.flu
#	Lag2.no_share.high
#	DiffLag3.smaller.low
#	Lag3..health.and.fitness.disease.cold.and.flu
#	keywords_df3148.high
#	Lag1.cloth.high
#	DiffLag3.zero_three_package_number.low
#	bigger.high
#	keywords_df8228.high
#	keywords_df8303.low
#	Lag1.gauze_share.low
#	Lag3.bigger.high
#	keywords_df7523.high
#	keywords_df8303.high
#	DiffLag3.other.low
#	Lag2.interval.peak.disease.low
#	keywords_df680
#	keywords_df2506
#	keywords_df3829
#	Lag2.Brand22
#	Lag3.Brand22
#	Lag1.Brand22
#	Lag3.peak.business.and.industrial
#	keywords_df5796.low
#	X.business.and.industrial.business.news

, data = df, family = binomial)
summary(solo)
confint(solo)



#### final models ####

topics_keywords = glm(spike~
                        
                        keywords_df1231.high    +                 #count 武漢	
                        #keywords_df1736.high   +                #pct count 武漢	
                        #	keywords_df7218.low        +              #DiffLag3 count 武漢
                        #keywords_df1736.low            +              #pct count 武漢	
                        #	keywords_df8228.high           +             #DiffRoll3 count 武漢
                        
                        keywords_df2995.high          +             #Lag1 count 新型コロナウイルス
                        # keywords_df1674.high           +          #pct count 新型コロナウイルス	
                        #	keywords_df4318.high            +       #Lag2 count 新型コロナウイルス
                        
                        #	keywords_df7523.high         +              #Roll3 count マスク
                        #keywords_df1556.high         +         #pct count 中国	
                        keywords_df1051.high         +           #count 中国      
                        
                        
                        #	Lag1.health.and.fitness.high
                        #	Roll3.health.and.fitness.high
                        #	Lag2.health.and.fitness.high
                        
                        #Lag2..health.and.fitness.disease	
                        #Lag3..health.and.fitness.disease	
                        #	Lag2.interval.peak.disease.low
                        #	Roll3.disease.high
                        #	Lag1.disease.high      
                      #X.health.and.fitness.disease	
                      Lag1..health.and.fitness.disease   
                        #	Lag2.disease.high
                        
                        # Roll3.epidemic.high	
                        #	Lag2..health.and.fitness.disease.epidemic
                        #	Lag3..health.and.fitness.disease.epidemic
                        #	Lag1..health.and.fitness.disease.epidemic    
                        
                        
                        #	Roll3.interval.peak.travel.low      
                        #	Lag2.interval.peak.travel.low      
                        #	Lag3.interval.peak.travel.low  
                      #	Lag3.interval.peak.shopping.high      
                      #	Lag1.interval.peak.law..govt.and.politics.high    
                      #	Roll3.interval.peak.shows.and.events.low    
                      
                      
                      #	Roll3.cloth.high
                      #	nonwoven_share.high
                      #	Lag1.nonwoven_share.high
                      #	Lag1.kids.high
                      #	Lag2.nonwoven_share.high
                      #	Lag2.urethane.high
                      #	Lag3.nonwoven.high
                      #	DiffLag2.three_ten_package_number.low
                      #	Lag1.yes.high
                      #	DiffLag3.three_ten_package_number.low
                      #	three_ten_package_number.high
                      #	zero_three_package_number.high
                      #	Lag2.yes.high
                      #	Lag2.ten_thirty_package_number.high
                      #	kids.high
                      #	Lag3.kids.high
                      #	Lag3.no_share.high
                      #	Lag2.cloth.high
                      #	DiffLag3.yes.low
                      #	disease.high
                      #	DiffRoll3.ten_thirty_package_number.low
                      #	Lag3.cloth.high
                      #	Lag1.bigger.high
                      #	gauze.high
                      #	Lag3.ten_thirty_package_number.high
                      #	Lag3.urethane.high
                      #	Roll3.gauze.high
                      #	Lag2.bigger.high
                      #	DiffLag2.smaller.low
                      #	Lag2.zero_three_package_number.high
                      #	DiffLag2.yes.low
                      #	Lag2.no_share.high
                      #	DiffLag3.smaller.low
                      #	Lag1.cloth.high
                      #	DiffLag3.zero_three_package_number.low
                      #	bigger.high
                      #	Lag1.gauze_share.low
                      #	Lag3.bigger.high
                      #	DiffLag3.other.low
                      
                      #Lag3.Brand31	
                      #Brand39	
                      #Lag1.Brand39	
                      #Lag3.Brand39
                      #Lag2.Brand32	       
                      #	Lag2.Brand45       
                      #	Lag3.Brand45       
                      #	Lag3.Brand32       
                      #	Lag1.Brand32     
                      
                      , data = df, family = binomial)
summary(topics_keywords_count)
confint(topics_keywords_count)




sentiment = glm(spike~
                  
                  #keywords_df3450          +              #Lag2 negative 武漢	
                  #keywords_df4771              +             #Lag3 negative 武漢	        
                  keywords_df300              +              #negative 武漢	      
                  
                  #	keywords_df2.low                       #positive_label_count
                  #	keywords_df7311.low                       #Roll3 positive_label_count        
                  #	keywords_df3150.low                        #Lag2 positive_label_count
                  #	keywords_df4473.low                             #Lag3 positive_label_count
                  keywords_df1829.low         +             #Lag1 positive_label_count
                  
                  #	keywords_df4471.high                        #Lag3 negative_label_count        
                  #	keywords_df3148.high                   #Lag2 negative_label_count
                  
                  #	keywords_df7523.high         +              #Roll3 count マスク
                  #keywords_df1556.high         +         #pct count 中国	
                  #keywords_df1051.high         +           #count 中国      
                  
                  
                  
                  #	Roll3.cloth.high
                #	nonwoven_share.high
                Lag1.nonwoven_share.high
                  #	Lag1.kids.high
                  #	Lag2.nonwoven_share.high
                  #	Lag2.urethane.high
                  #	Lag3.nonwoven.high
                  #	DiffLag2.three_ten_package_number.low
                  #	Lag1.yes.high
                  #	DiffLag3.three_ten_package_number.low
                  #	three_ten_package_number.high
                  #	zero_three_package_number.high
                  #	Lag2.yes.high
                  #	Lag2.ten_thirty_package_number.high
                #	kids.high
                #	Lag3.kids.high
                #	Lag3.no_share.high
                #	Lag2.cloth.high
                #	DiffLag3.yes.low
                #	disease.high
                #	DiffRoll3.ten_thirty_package_number.low
                #	Lag3.cloth.high
                #	Lag1.bigger.high
                #	gauze.high
                #	Lag3.ten_thirty_package_number.high
                #	Lag3.urethane.high
                #	Roll3.gauze.high
                #	Lag2.bigger.high
                #	DiffLag2.smaller.low
                #	Lag2.zero_three_package_number.high
                #	DiffLag2.yes.low
                #	Lag2.no_share.high
                #	DiffLag3.smaller.low
                #	Lag1.cloth.high
                #	DiffLag3.zero_three_package_number.low
                #	bigger.high
                #	Lag1.gauze_share.low
                #	Lag3.bigger.high
                #	DiffLag3.other.low
                
                #Lag3.Brand31	
                #Brand39	
                #Lag1.Brand39	
                #Lag3.Brand39	
                #Lag2.Brand32	      
                #	Lag2.Brand45      
                #	Lag3.Brand45       
                #	Lag3.Brand32       
                #	Lag1.Brand32    
                
                , data = df, family = binomial)
summary(sentiment)
confint(sentiment)











####training & testing ####

#random selection of 25% cases
set.seed(19)
validation_rows <- sort(sample(10000, size = 2500,replace = FALSE))
#create validation data
test <- df[validation_rows,]
#create remaining data
train <- df[-validation_rows,]



#### in sample ####

# confusion matrix
library(caret)
library(InformationValue)

predicted_in <- predict(topics_keywords, train, type="response")
confusionMatrix(train$spike, predicted_in) 
#   0 1
# 0 49 2
# 1 2 6

predicted_in2 <- predict(sentiment, train, type="response")
confusionMatrix(train$spike, predicted_in2) 
#   0 1
# 0 51 3
# 1  0 5

sensitivity(train$spike, predicted_in) # 0.75
specificity(train$spike, predicted_in) # 0.9607843

sensitivity(train$spike, predicted_in2) # 0.625
specificity(train$spike, predicted_in2) # 1

# ROC
prob_in=predict(topics_keywords,train, type=c("response"))
train$prob_in = prob_in
g = roc(spike ~ prob_in, data = train)
plot(g) 
auc(g) # 0.9865

prob_in2=predict(sentiment,train, type=c("response"))
train$prob_in2 = prob_in2
g = roc(spike ~ prob_in2, data = train)
plot(g) 
auc(g) # 0.973




#### out of sample ####

# confusion matrix

predicted_out <- predict(topics_keywords, test, type="response")
confusionMatrix(test$spike, predicted_out) 
#   0 1
# 0 9 0
# 1 0 1

predicted_out2 <- predict(sentiment, test, type="response")
confusionMatrix(test$spike, predicted_out2)
#   0 1
# 0 9 1
# 1 0 0

sensitivity(test$spike, predicted_out) # 1
specificity(test$spike, predicted_out) # 1

sensitivity(test$spike, predicted_out2) # 0
specificity(test$spike, predicted_out2) # 1

# ROC
prob_out=predict(topics_keywords,test, type=c("response"))
test$prob_out = prob_out
g = roc(spike ~ prob_out, data = test)
plot(g) 
auc(g) # 1

prob_out2=predict(sentiment,test, type=c("response"))
test$prob_out2 = prob_out2
g = roc(spike ~ prob_out2, data = test)
plot(g) 
auc(g) # 0.8889


#### all data ####

# confusion matrix

predicted <- predict(topics_keywords, df, type="response")
confusionMatrix(df$spike, predicted) 
#   0 1
# 0 58 2
# 1  2 7

predicted2 <- predict(sentiment, df, type="response")
confusionMatrix(df$spike, predicted2)
#   0 1
# 0 60 4
# 1  0 5

sensitivity(df$spike, predicted) # 0.7777778
specificity(df$spike, predicted) # 0.9666667

sensitivity(df$spike, predicted2) # 0.5555556
specificity(df$spike, predicted2) # 1

# ROC
prob=predict(topics_keywords,df, type=c("response"))
df$prob = prob
g = roc(spike ~ prob, data = df)
plot(g) 
auc(g) # 1

prob2=predict(sentiment,df, type=c("response"))
df$prob2 = prob2
g = roc(spike ~ prob2, data = df)
plot(g) 
auc(g) # 0.9639










