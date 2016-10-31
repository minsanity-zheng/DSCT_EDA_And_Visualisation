setwd("C:/Users/minzheng/Desktop/Data Science Competency Training/WorkSpace/Assignment 2/DSCT-EDA-And-Visualisation-master")
library(dplyr)
library(ggplot2)
library(reshape)
library(reshape2)
library(tidyr)
library(scales)
library(gridExtra)
# Load Data
GCI<-read.csv(file = "./GCI_dataset.csv", header = TRUE,sep = ";")
names(GCI)<-c("Year","Series.code","Series","Attribute","Cambodia","Indonesia","Malaysia","Philippines","Singapore","Thailand","Vietnam")
GCI$Year<-as.numeric(substr(GCI$Year, 1, 4)) 
# Reshape data
my_data <- GCI %>% 
  gather(Country, "Value", Cambodia:Vietnam) %>%
  spread(Attribute,Value)
my_data$Value[my_data$Value=="<0.1"] <- 0.09
#my_data <- melt(GCI, id=c("Year","Series.code","Series","Attribute"), variable_name = "Country")

# Q1
domestic_value = my_data[(my_data$Series.code==10.01),]
ggplot(domestic_value ,aes(x=Year,y=as.numeric(Value))) + 
    geom_line(position= position_dodge(.1),aes(group=Country,color=Country),size=2)+
    labs(title = "Domestic market size value", x="Year",y="Value") +
    theme(legend.text=element_text(size=20))

ggplot(domestic_value ,aes(x=Year,y=as.numeric(Rank))) + 
    geom_line(position= position_dodge(.1),aes(group=Country,color=Country),size=2)+
    scale_y_reverse()+
    labs(title = "Domestic market size Rank", x="Year",y="Rank")+
    theme(legend.text=element_text(size=20))

foreign_value = my_data[(my_data$Series.code==10.02),]
ggplot(foreign_value ,aes(x=Year,y=as.numeric(Value))) + 
    geom_line(position= position_dodge(.1),aes(group=Country,color=Country),size=2)+
    labs(title = "Foreign market size value", x="Year",y="Value")+
    theme(legend.text=element_text(size=20))

ggplot(foreign_value ,aes(x=Year,y=as.numeric(Rank))) + 
    geom_line(position= position_dodge(.1),aes(group=Country,color=Country),size=2)+
    scale_y_reverse()+
    labs(title = "Foreign market size Rank", x="Year",y="Rank")+
    theme(legend.text=element_text(size=20))

# Q2
Edu_Quality <- my_data[(my_data$Series.code==5.03),]
GDP <- my_data[(my_data$Series.code==0.01),]
GDP$LogValue <- log10(as.numeric(GDP$Value))
GDPP <- my_data[(my_data$Series.code==0.03),]
GDPP$LogValue <- log10(as.numeric(GDPP$Value))
#edu<-acast(EduQuality_value, Country~Year, value.var="Value")
#GDP_value$LogValue<-log10(as.numeric(GDP_value$Value))
#GDP<-acast(GDP_value, Country~Year, value.var="Value")

# Remove the year before 2008
sub_Edu_Quality <- Edu_Quality[(Edu_Quality$Year > 2010 & Edu_Quality$Year<2014),]
sub_GDP<-GDP[GDP$Year>2010,]
sub_GDPP<-GDPP[GDPP$Year>2010,]

ggplot() + 
  geom_line(data = sub_Edu_Quality,
            aes(x=Year,y=as.numeric(Value),group=Country,color=Series),
            position= position_dodge(.1))+
  geom_line(data = sub_GDP,
            aes(x=Year,y=as.numeric(LogValue),group=Country,color=Series),
            position= position_dodge(.1),linetype=5)+
  labs(title = "GDP VS Education Quality", x="Year",y="Rank")+
    facet_wrap(~Country,scales="free",ncol=4)

chisq.test(sub_Edu_Quality$Value,sub_GDP$Value)


ggplot() + 
  geom_line(data = sub_Edu_Quality,
            aes(x=Year,y=as.numeric(Value),group=Country,color=Series),
            position= position_dodge(.1))+
  geom_line(data = sub_GDPP,
            aes(x=Year,y=as.numeric(LogValue),group=Country,color=Series),
            position= position_dodge(.1),linetype=5)+
  labs(title = "GDP/capital VS Education Quality", x="Year",y="Rank")+
  facet_wrap(~Country,scales="free",ncol=4)

ggplot() + 
    geom_line(data = sub_Edu_Quality,
              aes(x=Year,y=as.numeric(Value),group=Country,color=Series),
              position= position_dodge(.1))+
    geom_line(data = sub_GDPP,
              aes(x=Year,y=as.numeric(LogValue),group=Country,color=Series),
              position= position_dodge(.1),linetype=5)+
    labs(title = "GDP/capital VS Education Quality", x="Year",y="Rank")+
    facet_wrap(~Country,scales="free",ncol=4)

# Q3
Primary <- my_data[(my_data$Series.code==4.10),]
Secondary <- my_data[(my_data$Series.code==5.01),]
Tertiary <- my_data[(my_data$Series.code==5.02),]
HIV <- my_data[(my_data$Series.code==4.05),]
DataQ3 <- my_data[(my_data$Series.code==4.10 | my_data$Series.code==5.01 
                      | my_data$Series.code==5.02),]

gg1<-ggplot(DataQ3) +
  geom_bar(aes(as.factor(Year),as.double(Value),color=Series,fill=Series),stat = "identity",position = "dodge")+
  labs(title = "Education level over years", x="Year",y="Percentage %")

gg2<-ggplot(HIV) +
  geom_bar(aes(as.factor(Year),as.double(Value),color=Series,fill=Series),stat = "identity",position = "dodge")+
  labs(title = "HIV pervalence over years", x="Year",y="Percentage %")

grid.arrange(gg1,gg2,nrow=2,ncol=1)

gg3<-ggplot() + 
    geom_line(data = Primary,
              aes(x=Year,y=as.numeric(Value),group=Country,color=Series),
              position= position_dodge(.1),size=2)+
    geom_line(data = Secondary,
              aes(x=Year,y=as.numeric(Value),group=Country,color=Series),
              position= position_dodge(.1),size=2)+
    geom_line(data = Tertiary,
              aes(x=Year,y=as.numeric(Value),group=Country,color=Series),
              position= position_dodge(.1),size=2)+
    labs(title = "GDP/capital VS Education Quality", x="Year",y="Percentage %")+
    facet_wrap(~Country,scales="free",nrow = 1)+
    theme(legend.text=element_text(size=10),axis.text.x = element_text(angle = 45, hjust = 1))

gg4<-ggplot() + 
    geom_line(data = HIV,
              aes(x=Year,y=as.numeric(Value),group=Country,color=Series,ymax=3,ymin=0),
              position= position_dodge(.1),size=1)+
    labs(title = "HIV", x="Year",y="Percentage %")+
    facet_wrap(~Country,scales="free",nrow = 1)+
    theme(legend.text=element_text(size=12),axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(gg3,gg4,nrow=2,ncol=1)


# Q4
dataQ4DR<-my_data[(my_data$Series.code==1.03 | my_data$Series.code==1.16) & my_data$Year>2009,]

dataQ4IR<-my_data[(my_data$Series.code==1.05 | my_data$Series.code==1.16) & my_data$Year>2009,]



gg5<-ggplot(dataQ4DR) +
  geom_line(aes(as.factor(Year),as.double(Value),group=Series,color=Series),stat = "identity")+
  labs(title = "Diversion of Public funds VS Reliability of police Services", 
       x="Year",y="Percentage %")+
  facet_wrap(~Country,scales="free",ncol = 1)

gg6<-ggplot(dataQ4IR) +
  geom_line(aes(as.factor(Year),as.double(Value),group=Series,color=Series),stat = "identity")+
  labs(title = "Irregular payment & bribes VS Reliability of police Services",
       x="Year",y="Percentage %")+
  facet_wrap(~Country,scales="free",ncol = 1)

grid.arrange(gg5,gg6,nrow=1,ncol=2)

Diversion<-dataQ4DR[(dataQ4DR$Series.code==1.03),]
Reliability<-dataQ4DR[(dataQ4DR$Series.code==1.16),]
Irreg_payment<-dataQ4IR[(dataQ4IR$Series.code==1.05),]
chisq.test(Diversion$Value,Reliability$Value)
chisq.test(Irreg_payment$Value,Reliability$Value)

# Q5
dataQ5I<-my_data[(my_data$Series.code==3.03)
                  & my_data$Year>2007 & my_data$Year<2014,]

dataQ5S<-my_data[(my_data$Series.code==8.06)
                  & my_data$Year>2007 & my_data$Year<2014,]

dataQ5P<-my_data[(my_data$Series.code==0.03)
                  & my_data$Year>2007 & my_data$Year<2014,]

gg7<-ggplot(dataQ5I) +
  geom_line(aes(as.factor(Year),as.double(Value),group=Series,color=Series),stat = "identity")+
  labs(title = "Inflation", 
       x="Year",y="Percentage %")+
  facet_wrap(~Country,scales="free",ncol = 1)+
  theme(legend.position="none")


gg8<-ggplot(dataQ5S) +
  geom_line(aes(as.factor(Year),as.double(Value),group=Series,color=Series),stat = "identity")+
  labs(title = "Soundness of bank",
       x="Year",y="Percentage %")+
  facet_wrap(~Country,scales="free",ncol = 1)+
  theme(legend.position="none")


gg9<-ggplot(dataQ5P) +
  geom_line(aes(as.factor(Year),as.double(Value),group=Series,color=Series),stat = "identity")+
  labs(title = "Income",
       x="Year",y="Percentage %")+
  facet_wrap(~Country,scales="free",ncol = 1)+
  theme(legend.position="none")

grid.arrange(gg7,gg8,gg9,nrow=1,ncol=3)