library("ggplot2")
install("ggplot2")
library("ggpubr")
install("ggpubr")
library("tidyverse")
install("tidyverse")
library("broom")
install("broom")
library("AICcmodavg")
install("AICcmodavg")
library("dplyr")
install("dplyr")
library("plotrix")
install("plotrix")
install.packages("BSDA")
library("BSDA")
install.packages("plotrix")
library("plotrix")
install.packages("corrplot")
library("corrplot")
install.packages("stringi")
library("stringi")
install.packages("Hmisc")
library("Hmisc")

#############################################################Question-1###################################################################################################
#read the file
df <- read.csv("Accidents.csv")
df<-read.csv("C:/Users/alienware/Desktop/Accidents.csv",header=TRUE)
#select the police_force,accident_severity and add number_of_accident
Question1_2.data<-subset(df,select=c(Police_Force,Accident_Severity))
Question1_2.data$Number_of_accident<-1
#Count the total number of accidents in each police force
Question1_1.data<- Question1_2.data %>%
  group_by(Police_Force) %>%
  summarise(Number_of_accident=sum(Number_of_accident))
#One-way analysis of variance of Number_of_accident across Police_Force
one.way<-aov(Number_of_accident ~ Police_Force,data=Question1_1.data)
summary(one.way)
#Using ggplot draw the barchart
ggplot(data=Question1_1.data,aes(x=factor(Police_Force),y=Number_of_accident))+geom_bar(stat='identity')+xlab('Police_Force')+ylab('Number_of_accident')+ggtitle("Number of Accidents in different Police Force")

#One-way analysis of variance of Accident_Severity across Police_Force
one.way.1_2<-aov(Accident_Severity ~ Police_Force, data =Question1_2.data)
summary(one.way.1_2)
#chisq.test
chisq.test(table(df$Police_Force,df$Accident_Severity))
#Change the frequency of table statistics into data.frame form and using ggplot draw the bar chart
Question1_2<-as.data.frame(table(df$Police_Force,df$Accident_Severity))
names(Question1_2)<-c('Police_Force','Severity','number')
ggplot(data=Question1_2,aes(x=factor(Police_Force),y=number, fill=Severity))+scale_fill_brewer(palette="Blues")+geom_bar(stat='identity')+xlab('Police_Force')+ylab('number')+ggtitle(" Accidents of different Severity in different Police Force")

#################################################################Question-2##############################################################################################################
#Inserting day name for barplot
df<-df%>%
  mutate(Day_Name=case_when(
    (Day_of_Week==1) ~ "Sunday",
    (Day_of_Week==2) ~ "Monday",
    (Day_of_Week==3) ~ "Tuesday",
    (Day_of_Week==4) ~ "Wednesday",
    (Day_of_Week==5) ~ "Thursday",
    (Day_of_Week==6) ~ "Friday",
    (Day_of_Week==7) ~ "Saturday"))
  
table(df$Day_Name)
bar1<-table(df$Day_Name)
bar1
barplot(bar1,xlab="Day of Week",ylab="Number of Accidents",ylim=c(0,25000),col="blue",main="Number of accidents each day")
#number of accidents on weekdays
weekdays<-c(1710.8,1518,1682.6,1310.4,1688.8,1706.4,1615.2,1561.4,1633.4,1696,1870,21562.6)
#number of accidents on weekend
weekend<-c(1296.5,1075,1196.5,1610.5,1239,1299,1687,1247,1462,1357,1303.5,1328.5)
a<-factor(c(rep(1,12),rep(2,12)))
#Homogeneity of variance test
bartlett.test(x~a)
var.teat(x~a)
#t test
t.test(weekdays,weekend,paired = FALSE)

df_week <- df
#Data Cleaning
df_week[df_week$Day_of_Week<0] <-NA 
df_week <-na.omit(df_week)
#Subset for Weekday
df_weekday <- subset(df_week,(df_week$Day_of_Week>'1' & df_week$Day_of_Week< '7'),select = c(Day_of_Week))
#Subset for Weekend
df_weekend <- subset(df_week,(df_week$Day_of_Week=='1' |df_week$Day_of_Week=='7'),select = c(Day_of_Week))
#Counting number of accidents
weekday_count <- nrow(df_weekday)
weekend_count <- nrow(df_weekend)


Week <- c("Weekday", "Weekend")
Week_Accident_Count <- c(weekday_count,weekend_count)
week_plot_df <- data.frame(Week, Week_Accident_Count)

#Plotting
ggplot(df_week, aes(x=Week, y=Week_Accident_Count))+
  geom_bar(stat='identity', fill='red')+
  coord_flip()+
  geom_text(aes(label=second_column))

#################################################################Question-3##############################################################################################################
df_sev <- df[df$Accident_Severity >-1,]
#Subset for day
df_day <- subset(df,(df_sev$Time<'18:00' & df_sev$Time >'06:00') & df_sev$Accident_Severity==1,select = c(Date,Accident_Severity))
#Subset for night
df_night <- subset(df_sev,(df_sev$Time>='18:00' | df_sev$Time <='06:00') & df_sev$Accident_Severity==1,select = c(Date,Accident_Severity))

#Group for day
df_day_grp <-  df_day %>%
  group_by(Date)%>%
  summarise(Accident_Sum=sum(Accident_Severity))

#Group for night
df_night_grp <-  df_night %>%
  group_by(Date)%>%
  summarise(Accident_Sum=sum(Accident_Severity)) 

day_var <- var(df_day_grp$Accident_Sum)
night_var <- var(df_night_grp$Accident_Sum)

#Z-test
z.test(df_day_grp$Accident_Sum,df_night_grp$Accident_Sum,sigma.x=day_var,sigma.y=night_var,conf.level=0.95)

mean_day=mean(df_day_grp$Accident_Sum)
mean_night=mean(df_night_grp$Accident_Sum)
H <- c(mean_day,mean_night)
M <- c("Day","Night")
#barplot
barplot(H,names.arg=M,xlab="Time",ylab="Accident Mean",ylim=c(0,4),col="magenta",
        main="Day Night chart",border="black")

#################################################################Question-4##############################################################################################################
#Select the Road_type,Accident_severity and add number_of_accident
Question4.data<-subset(df,select=c(Road_Type,Accident_Severity))
Question4.data$Number_of_accident<-1
#One-way analysis of variance about Accident_Severity and Road_type
one.way.4<-aov(Accident_Severity ~ Road_Type, data =Question4.data)
summary(one.way.4)
#chisq.test
chisq.test(table(df$Road_Type,df$Accident_Severity))
#Count the total number of accidents in each Road type
Question4_2.data<- Question4.data %>%
  group_by(Road_Type) %>%
  summarise(Number_of_accident=sum(Number_of_accident))



#One-way analysis of variance about Number_of_accident and Road_type
one.way.4_2<-aov(Number_of_accident~Road_Type,data=Question4_2.data)
summary(one.way.4_2)
#Using the descriptive statistics
sd(table(df$Road_Type))


#Using the ggplot draw the bar chart
Question4_2<-as.data.frame(table(df$Road_Type,df$Accident_Severity))

#Creating a new column for Road type names
Question4_2<-Question4_2%>%
  mutate(Road_Type_Name=case_when(
    (Road_Type==1) ~ "Roundabout",
    (Road_Type==2) ~ "One way street",
    (Road_Type==3) ~ "Dual carriageway",
    (Road_Type==6) ~ "Single carriageway",
    (Road_Type==7) ~ "Slip road",
    (Road_Type==9) ~ "Unknown"))

names(Question4_2)<-c('Road_Type','Severity','number')
ggplot(data=Question4_2 , aes(x =factor(Road_Type_Name), y =number,fill=Severity ))+scale_fill_brewer(palette="Reds")+geom_bar(stat='identity') +xlab('Road Types')+ ylab('Number of Accidents')+ggtitle("The number of Accidents on different Road type")


#Using the pie3D draw the pie chart
Question4_2.data<-Question4_2.data%>%
  mutate(Question4_2.data_Name=case_when(
    (Road_Type==1) ~ "Roundabout",
    (Road_Type==2) ~ "One way street",
    (Road_Type==3) ~ "Dual carriageway",
    (Road_Type==6) ~ "Single carriageway",
    (Road_Type==7) ~ "Slip road",
    (Road_Type==9) ~ "Unknown"))
slices<-(Question4_2.data$Number_of_accident)
label=Question4_2.data$Number_of_accident
pie3D(slices, labels=label,explode=0.1,radius=1,main= "Effect of Road type on Number of Accidents")
legend("topleft", c("Roundabout","One way street","Dual carriageway","Single carriageway","Slip road","Unknown"),cex=0.8, fill=rainbow(length(Question4_2.data$Question4_2.data_Name)))

#################################################################Question-5#########################################################################################################
#subset for reasonable independent factors
df_accident_factors<- subset(df, select=c(Day_of_Week, Road_Class_1st, Road_Type,Speed_limit, Junction_Detail, Junction_Control, Road_Class_2nd, Pedestrian_Crossing_Human_Control,Pedestrian_Crossing_Physical_Facilities, Light_Conditions, Weather_Conditions, Road_Surface_Conditions, Special_Conditions_at_Site, Carriageway_Hazards, Urban_or_Rural_Area))

df_accident_factors[df_accident_factors<0] <-NA
clean_accident_factors_df <-na.omit(df_accident_factors)
clean_accident_factors_df

#converting data to matrix
accident_factors_matrix<- as.matrix(clean_accident_factors_df)
#creating correlation matrix of independent variables
accident_correlation_matrix<-cor(clean_accident_factors_df)


#heatmap of correlation matrix
corrplot(accident_correlation_matrix, type = "upper", order = "hclust", tl.col = "black", tl.strt=45)

clean_accident_factors_df$Accident_Count<-1
#===================================================================================#
group_Road_Type<-clean_accident_factors_df%>%#very less correlation
  group_by(Road_Type)%>%
  summarise('Grouped_count_of_Accidents'=sum(Accident_Count))

cor(group_Road_Type$Road_Type,group_Road_Type$Grouped_count_of_Accidents)

#===================================================================================#

group_Day_of_Week_group<-clean_accident_factors_df%>%#very less correlation
  group_by(Day_of_Week)%>%
  summarise('Grouped_count_of_Accidents'=sum(Accident_Count))

cor(group_Day_of_Week_group$Day_of_Week,group_Day_of_Week_group$Grouped_count_of_Accidents)

#===================================================================================#
group_Road_Class_1st<-clean_accident_factors_df%>%#very less correlation
  group_by(Road_Class_1st)%>%
  summarise('Grouped_count_of_Accidents'=sum(Accident_Count))

cor(group_Road_Class_1st$Road_Class_1st,group_Road_Class_1st$Grouped_count_of_Accidents)
#===================================================================================#
group_Speed_limit<-clean_accident_factors_df%>%#very less correlation
  group_by(Speed_limit)%>%
  summarise('Grouped_count_of_Accidents'=sum(Accident_Count))

cor(group_Speed_limit$Speed_limit,group_Speed_limit$Grouped_count_of_Accidents)
#===================================================================================#
group_Junction_Detail<-clean_accident_factors_df%>% #very less correlation
  group_by(Junction_Detail)%>%
  summarise('Grouped_count_of_Accidents'=sum(Accident_Count))

cor(group_Junction_Detail$Junction_Detail,group_Junction_Detail$Grouped_count_of_Accidents)

#===================================================================================#
group_Light_Conditions<-clean_accident_factors_df%>% #significant
  group_by(Light_Conditions)%>%
  summarise('Grouped_count_of_Accidents'=sum(Accident_Count))

cor(group_Light_Conditions$Light_Conditions,group_Light_Conditions$Grouped_count_of_Accidents)
lm_Light_Conditions<- lm(Grouped_count_of_Accidents ~ Light_Conditions, data=group_Light_Conditions)
summary(lm_Light_Conditions)
#===================================================================================#
group_Road_Surface_Conditions<-clean_accident_factors_df%>% #significant
  group_by(Road_Surface_Conditions)%>%
  summarise('Grouped_count_of_Accidents'=sum(Accident_Count))

cor(group_Road_Surface_Conditions$Road_Surface_Conditions,group_Road_Surface_Conditions$Grouped_count_of_Accidents)
lm_Road_Surface_Conditions<- lm(Grouped_count_of_Accidents ~ Road_Surface_Conditions, data=group_Road_Surface_Conditions)
summary(lm_Road_Surface_Conditions)

#===================================================================================#
group_Weather_Conditions<-clean_accident_factors_df%>% #significant
  group_by(Weather_Conditions)%>%
  summarise('Grouped_count_of_Accidents'=sum(Accident_Count))

cor(group_Weather_Conditions$Weather_Conditions,group_Weather_Conditions$Grouped_count_of_Accidents)
lm_Weather_Conditions<- lm(Grouped_count_of_Accidents ~ Weather_Conditions, data=group_Weather_Conditions)
summary(lm_Weather_Conditions)
#===================================================================================#
group_Pedestrian_Crossing_Physical_Facilities<-clean_accident_factors_df%>% #insignificant
  group_by(Pedestrian_Crossing_Physical_Facilities)%>%
  summarise('Grouped_count_of_Accidents'=sum(Accident_Count))

cor(group_Pedestrian_Crossing_Physical_Facilities$Pedestrian_Crossing_Physical_Facilities,group_Pedestrian_Crossing_Physical_Facilities$Grouped_count_of_Accidents)
lm_Pedestrian_Crossing_Physical_Facilities<- lm(Grouped_count_of_Accidents ~ Pedestrian_Crossing_Physical_Facilities, data=group_Pedestrian_Crossing_Physical_Facilities)
summary(lm_Pedestrian_Crossing_Physical_Facilities)

#===================================================================================#
group_Junction_Control<-clean_accident_factors_df%>% #insignificant
  group_by(Junction_Control)%>%
  summarise('Grouped_count_of_Accidents'=sum(Accident_Count))

cor(group_Junction_Control$Junction_Control,group_Junction_Control$Grouped_count_of_Accidents)
lm_Junction_Control<- lm(Grouped_count_of_Accidents ~ Junction_Control, data=group_Junction_Control)
summary(lm_Junction_Control)

#===================================================================================#
group_Road_Class_2nd<-clean_accident_factors_df%>% #insignificant
  group_by(Road_Class_2nd)%>%
  summarise('Grouped_count_of_Accidents'=sum(Accident_Count))

cor(group_Road_Class_2nd$Road_Class_2nd,group_Road_Class_2nd$Grouped_count_of_Accidents)
lm_Road_Class_2nd<- lm(Grouped_count_of_Accidents ~ Road_Class_2nd, data=group_Road_Class_2nd)
summary(lm_Road_Class_2nd)
#===================================================================================#
group_Pedestrian_Crossing_Human_Control<-clean_accident_factors_df%>% #insignificant
  group_by(Pedestrian_Crossing_Human_Control)%>%
  summarise('Grouped_count_of_Accidents'=sum(Accident_Count))

cor(group_Pedestrian_Crossing_Human_Control$Pedestrian_Crossing_Human_Control,group_Pedestrian_Crossing_Human_Control$Grouped_count_of_Accidents)
lm_Pedestrian_Crossing_Human_Control<- lm(Grouped_count_of_Accidents ~ Pedestrian_Crossing_Human_Control, data=group_Pedestrian_Crossing_Human_Control)
summary(lm_Pedestrian_Crossing_Human_Control)
#===================================================================================#
group_Special_Conditions_at_Site<-clean_accident_factors_df%>% #insignificant
  group_by(Special_Conditions_at_Site)%>%
  summarise('Grouped_count_of_Accidents'=sum(Accident_Count))

cor(group_Special_Conditions_at_Site$Special_Conditions_at_Site,group_Special_Conditions_at_Site$Grouped_count_of_Accidents)
lm_Special_Conditions_at_Site<- lm(Grouped_count_of_Accidents ~ Special_Conditions_at_Site, data=group_Special_Conditions_at_Site)
summary(lm_Special_Conditions_at_Site)
#===================================================================================#
group_Urban_or_Rural_Area <-clean_accident_factors_df%>% #insignificant
  group_by(Urban_or_Rural_Area)%>%
  summarise('Grouped_count_of_Accidents'=sum(Accident_Count))

cor(group_Urban_or_Rural_Area$Urban_or_Rural_Area,group_Urban_or_Rural_Area$Grouped_count_of_Accidents)
lm_Urban_or_Rural_Area<- lm(Grouped_count_of_Accidents ~ Urban_or_Rural_Area, data=group_Urban_or_Rural_Area)
summary(lm_Urban_or_Rural_Area)
#===================================================================================#
######################plotting######################################
graph_light_conditions <- group_Light_Conditions%>%
  mutate(Light_Conditions_Name=case_when(
    (Light_Conditions==1) ~ "Daylight",
    (Light_Conditions==4) ~ "Darkness-Lights Lit",
    (Light_Conditions==5) ~ "Darkness-Lights Unlit",
    (Light_Conditions==6) ~ "Darkness-No Lighting",
    (Light_Conditions==7) ~ "Darkness- Lighting Unknown"
  ))

ggplot(data = graph_light_conditions,aes(x=Light_Conditions_Name, y=Grouped_count_of_Accidents))+
  coord_flip()+
  geom_bar(stat ="identity",fill="orange")

#################################################################Question-6#########################################################################################################
df_accident_severity_analysis<- subset(df, select=c(Day_of_Week, Accident_Severity,Road_Class_1st, Road_Type,Speed_limit, Junction_Detail, Junction_Control, Road_Class_2nd, Pedestrian_Crossing_Human_Control,Pedestrian_Crossing_Physical_Facilities, Light_Conditions, Weather_Conditions, Road_Surface_Conditions, Special_Conditions_at_Site, Carriageway_Hazards, Urban_or_Rural_Area))

df_accident_severity_analysis[df_accident_severity_analysis<0] <-NA


clean_accident_severity_analysis <-na.omit(df_accident_severity_analysis)
clean_accident_severity_analysis

#Dependent Variable as Factor 
clean_accident_severity_analysis$Accident_Severity<- as.factor(clean_accident_severity_analysis$Accident_Severity)
str(clean_accident_severity_analysis)

#Independent Variable as Factor
clean_accident_severity_analysis$Day_of_Week<- as.factor(clean_accident_severity_analysis$Day_of_Week)
clean_accident_severity_analysis$Road_Type<- as.factor(clean_accident_severity_analysis$Road_Type)
clean_accident_severity_analysis$Junction_Detail<- as.factor(clean_accident_severity_analysis$Junction_Detail)
clean_accident_severity_analysis$Junction_Control<- as.factor(clean_accident_severity_analysis$Junction_Control)
clean_accident_severity_analysis$Road_Class_1st<- as.factor(clean_accident_severity_analysis$Road_Class_1st)
clean_accident_severity_analysis$Road_Class_2nd<- as.factor(clean_accident_severity_analysis$Road_Class_2nd)
clean_accident_severity_analysis$Pedestrian_Crossing_Human_Control<- as.factor(clean_accident_severity_analysis$Pedestrian_Crossing_Human_Control)
clean_accident_severity_analysis$Pedestrian_Crossing_Physical_Facilities<- as.factor(clean_accident_severity_analysis$Pedestrian_Crossing_Physical_Facilities)
clean_accident_severity_analysis$Light_Conditions<- as.factor(clean_accident_severity_analysis$Light_Conditions)
clean_accident_severity_analysis$Weather_Conditions<- as.factor(clean_accident_severity_analysis$Weather_Conditions)
clean_accident_severity_analysis$Road_Surface_Conditions<- as.factor(clean_accident_severity_analysis$Road_Surface_Conditions)
clean_accident_severity_analysis$Special_Conditions_at_Site<- as.factor(clean_accident_severity_analysis$Special_Conditions_at_Site)
clean_accident_severity_analysis$Carriageway_Hazards<- as.factor(clean_accident_severity_analysis$Carriageway_Hazards)
clean_accident_severity_analysis$Urban_or_Rural_Area<- as.factor(clean_accident_severity_analysis$Urban_or_Rural_Area)

library(nnet)
clean_accident_severity_analysis$Accident_Severity<-relevel(clean_accident_severity_analysis$Accident_Severity, ref = '1')
logistic_model<- multinom(Accident_Severity~.,data=clean_accident_severity_analysis)
summary(logistic_model)

#2-tailed z-test
z_test<- summary(logistic_model)$coefficients/summary(logistic_model)$standard.errors
p_value<- (1-pnorm(abs(z_test),0,1))*2
p_value
exp(coefficients(logistic_model))
