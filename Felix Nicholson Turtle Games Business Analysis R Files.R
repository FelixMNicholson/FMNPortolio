            
            #Assignment Activity 4: Visualize data to gather insights

#Get Working Directory

getwd()

#Import CSV File to Global Environment 

turtles <- read.csv("turtle_sales.csv",header = TRUE, sep = ",")

#Explore the dataset 

head(turtles, 5)
tail(turtles, 5)

#Import dplyer & tidyverse 

library(dplyr); library(tidyverse)

#Drop Columns 

turtles2 <- select(turtles, -Ranking, -Year,-Genre,-Publisher)

#Check new DF 

head(turtles2,5)           

      #Create plots to review and determine insights

#Sum total sales by platform 

plats <- turtles2 %>% group_by(Platform) %>% summarise(GS=sum(Global_Sales))

      #Create barplot for Global Sales (GS) against platform 

ggplot(plats, aes(Platform, GS)) + geom_col()


      #Create stacked barchart for sales, region and platform 

plats2 <- turtles2 %>% group_by(Platform) %>% summarise(NS=sum(NA_Sales), ES=sum(EU_Sales), GS=sum(Global_Sales))

plats3 <- pivot_longer(plats2, cols = c(NS, ES, GS))

#Rename Column 

plats3 <- plats3 %>% 
  rename(
    Region = name ,
    Sales = value)

#Check DF 

plats3

#Create plot

ggplot(plats3,  aes(Platform, Sales, fill=Region)) + geom_col()


#dads cool plot 

new_sales <- turtles3 %>% mutate(RoW = (turtles3$Global_Sales-(turtles3$NA_Sales+turtles3$EU_Sales))) 

plats4 <- new_sales%>% group_by(Platform) %>% summarise(NS=sum(NA_Sales), ES=sum(EU_Sales), RestofWorld=sum(RoW))

#Pivot DF 

plats5 <- pivot_longer(plats4, cols = c(ES, NS, RestofWorld))

#Rename Columns 

plats5 <- plats5 %>% 
  rename(
    Region = name ,
    Sales = value)

#Plot 
ggplot(plats5,  aes(Platform, Sales, fill=Region)) + geom_col()


      #Create 3 scatterplots for each region vs sales over time 

turtles3 <- select(turtles, -Ranking, -Genre,-Publisher)

year1 <- turtles3 %>% group_by(Year) %>% summarise(NS=sum(NA_Sales), ES=sum(EU_Sales), GS=sum(Global_Sales))

#Make a DF for each region 

NA1 <- select(year1,-ES, -GS)

ES1 <- select(year1,-NS, -GS)

GS1 <- select(year1,-NS, -ES)


      #Create scatterplots for each region 

#NA Sales

NA1 <- NA1 %>% 
  rename(NA_Sales = NS)

qplot(Year, NA_Sales, data=NA1) +  geom_smooth(method="lm") 

#Global Sales 

GS1 <- GS1 %>% 
  rename(Global_Sales = GS)

qplot(Year, Global_Sales, data=GS1) +  geom_smooth(method="lm") 

#EU Sales 

ES1 <- ES1 %>% 
  rename(
    EU_Sales = ES, Time=Year)

qplot(Time, EU_Sales, data=ES1) +  geom_smooth(method="lm") 


                #Clean, manipulate, and visualize the data

#Determine the min, max and mean for each sales group

    #NA Sales Figures 

print(max(turtles$NA_Sales))

#Max NA Sales = 34.02 

print(min(turtles$NA_Sales))

#Min NA Sales = 0 

print(mean(turtles$NA_Sales))

#Mean NA Sales = 2.515966


    #EU Sales Figures 

print(max(turtles$EU_Sales))

#Max EU Sales = 23.8

print(min(turtles$EU_Sales))

#Min EU Sales = 23.8

print(mean(turtles$EU_Sales))

#Mean EU Sales = 1.643778 


    #Global Sales Figures

print(max(turtles$Global_Sales))

#Max Global Sales = 67.85

print(min(turtles$Global_Sales))

#Min Global Sales = 0.01

print(mean(turtles$Global_Sales))
      
#Mean Global Sales = 5.334688 


    #Determine the impact on sales per product_id

prods_GS <- turtles %>% group_by(Product) %>% summarise(GS=sum(Global_Sales))

prodsEU <- turtles %>% group_by(Product) %>% summarise(EU_Sales=sum(EU_Sales))

prodsNA <- turtles %>% group_by(Product) %>% summarise(NA_Sales=sum(NA_Sales))



#Create plots for top 10 best selling products 

tt10_GS <- prods_GS[1:10,]

tt10_EU <- prodsEU[1:10,]

tt10_NA <- prodsNA[1:10,]

#Change Product Data-type to character for ease of visualization

tt10_GS$Product<-as.character(tt10_GS$Product)

tt10_EU$Product<-as.character(tt10_EU$Product)

tt10_NA$Product<-as.character(tt10_NA$Product)

#Create Plots

ggplot(data=tt10_GS, aes(x=Product, y=GS)) +
  geom_bar(stat="identity")

ggplot(data=tt10_EU, aes(x=Product, y=EU_Sales)) +
  geom_bar(stat="identity")

ggplot(data=tt10_NA, aes(x=Product, y=NA_Sales)) +
  geom_bar(stat="identity")



  #Sum of top 10 products by region 

sum(tt10_GS$GS)

#Sum = 311.64 

sum(tt10_EU$EU_Sales)

#Sum = 78.53

sum(tt10_NA$NA_Sales)

#Sum = 172.15 


  #Compare these top 10 sums against sums for all products 

sum(turtles$Global_Sales)

#Sum =1877.8 

sum(turtles$EU_Sales)

#Sum =578.7 

sum(turtles$NA_Sales)

#Sum = 885.62 


                    #Making recommendations to the business


#Create and explore Q-Q plots for all sales data.


qqnorm(turtles3$NA_Sales)

qqnorm(turtles3$EU_Sales)

qqnorm(turtles3$Global_Sales)

  #Perform a Shapiro-Wilk test on all the sales data


shapiro.test((turtles3$NA_Sales))

shapiro.test((turtles3$EU_Sales))

shapiro.test((turtles3$Global_Sales))


  #Determine the Skewness and Kurtosis of all the sales data.

# Import the moments package and library. 


library(tidyverse)

library(moments)

skewness(turtles3$NA_Sales)

kurtosis(turtles3$NA_Sales)

skewness(turtles3$EU_Sales)

kurtosis(turtles3$EU_Sales)

skewness(turtles3$Global_Sales)

kurtosis(turtles3$Global_Sales)


    #Determine if there is any correlation between the sales data columns.


qplot(Global_Sales, NA_Sales, data=turtles3) +  geom_smooth(method="lm") 

qplot(Global_Sales, EU_Sales, data=turtles3) +  geom_smooth(method="lm") 

qplot(EU_Sales, NA_Sales, data=turtles3) +  geom_smooth(method="lm") 


#Result: there is a strong positive correlation between all 3 sales columns.



#Normalize the data 

library(dplyr)

norm1 <- turtles3%>%mutate(across(c(EU_Sales, NA_Sales, Global_Sales),function(x)  sqrt(x)))

#Visualize the result of the transformed data. 

qplot(Global_Sales, NA_Sales, data=norm1) +  geom_smooth(method="lm") 

qplot(EU_Sales, NA_Sales, data=norm1) +  geom_smooth(method="lm") 

#Create a simple linear regression model

modela <- lm(norm1$NA_Sales ~ norm1$EU_Sales)

#View summary stats 

summary(modela)

# Create a visualisation to determine normality of data set.
qqnorm(residuals(modela))
qqline(residuals(modela), col='red') 

#Create a multiple linear regression model 


modelb = lm(Global_Sales~EU_Sales+NA_Sales, data=turtles3)

summary(modelb)

qqline(residuals(modelb), col='blue') 

qqnorm(residuals(modelb))
qqline(residuals(modelb), col='yellow') 


predictTest2 = predict(modelb, newdata=turtles3,
                      interval='confidence')

tail(predictTest)
head(predictTest)

str(norm1)

ls.str(turtles3)

#Compare predicted and observed values 

#Make list of values to be assessed from original data 

vals = c(3.93, 34.02, 2.73, 2.26, 22.08) 

#Make new DF and look for those values in original sales data. 

ft_p1 <- turtles3[turtles3$NA_Sales %in% vals,]

#Remove unnecessary columns for later analysis 

ft_p1 <- select(ft_p1, -Product, -Platform,-Year) 

#View predicted data

head(predictTest2)

#Make DF to concat and select by row numbers from original data 

ft_p2 <- predictTest2[c(1, 10,99,176,211),]

ft_p3 <- data.frame(ft_p2)

#Remove unnecessary columns 

ft_p3 <- select(ft_p3,-lwr,-upr) 

#Rename column 

ft_p3 <- ft_p3 %>% rename(
    "Predicted Global Sales" = fit)

#Concat prediction DF and Original Data 

ft_p4 <- cbind(ft_p1, ft_p3)







