#project demonstration 
#predict future

setwd("C:/R")
climate_data=read.csv("DATA.csv")
gases_data=read.csv("DATA2.csv")
combine_data=read.csv("DATA3.csv")
diff_data=read.csv("DATA4.csv")


print(climate_data)
print(gases_data)

#climate change visualization
library(dplyr)
library(ggplot2)


combine_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=T),stat="identity")
combine_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=TM),stat="identity")
combine_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=Tm2),stat="identity")
combine_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=PP),stat="identity")
combine_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=V),stat="identity")

#gases amt change visualization

gases_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=CO2),stat="identity")
gases_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=CH4),stat="identity")
gases_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=N2O),stat="identity")
gases_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=CFCs),stat="identity")
gases_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=HCFCs),stat="identity")
gases_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=HFCs),stat="identity")

#climate change with gases amt

combine_data %>% ggplot(aes(x=Total))+
        geom_bar(aes(y=T),stat="identity")+
        theme_bw()+
        facet_wrap(.~Year)+
        labs(x="Total emission each year",y="Temperature")
combine_data %>% ggplot(aes(x=Total))+
        geom_bar(aes(y=TM),stat="identity")+
        theme_bw()+
        facet_wrap(.~Year)+
        labs(x="Total emission each year",y="Max Temperature")
combine_data %>% ggplot(aes(x=Total))+
        geom_bar(aes(y=Tm2),stat="identity")+
        theme_bw()+
        facet_wrap(.~Year)+
        labs(x="Total emission each year",y="Min Temperature")
combine_data %>% ggplot(aes(x=Total))+
        geom_bar(aes(y=PP),stat="identity")+
        theme_bw()+
        facet_wrap(.~Year)+
        labs(x="Total emission each year",y="Rainfall Amount")
combine_data %>% ggplot(aes(x=Total))+
        geom_bar(aes(y=V),stat="identity")+
        theme_bw()+
        facet_wrap(.~Year)+
        labs(x="Total emission each year",y="Wind speed")

summary(combine_data$CO2)
summary(combine_data$CH4)
summary(combine_data$N2O)
summary(combine_data$CFCs)
summary(combine_data$HCFCs)
summary(combine_data$HFCs)

#plot for future

diff_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=CO2),stat="identity")
diff_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=CO2),stat="identity")+
        theme_bw()+
        facet_wrap(.~Year)+
        labs(x="years",y="co2 future predictions")

diff_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=CH4),stat="identity")
diff_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=CH4),stat="identity")+
        theme_bw()+
        facet_wrap(.~Year)+
        labs(x="years",y="CH4 future predictions")

diff_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=N2O),stat="identity")
diff_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=N2O),stat="identity")+
        theme_bw()+
        facet_wrap(.~Year)+
        labs(x="years",y="N2O future predictions")

diff_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=CFCs),stat="identity")
diff_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=CFCs),stat="identity")+
        theme_bw()+
        facet_wrap(.~Year)+
        labs(x="years",y="CFC future predictions")

diff_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=HCFCS),stat="identity")
diff_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=HCFCS),stat="identity")+
        theme_bw()+
        facet_wrap(.~Year)+
        labs(x="years",y="HCFC future predictions")

diff_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=CFCs),stat="identity")
diff_data %>% ggplot(aes(x=Year))+
        geom_bar(aes(y=CFCs),stat="identity")+
        theme_bw()+
        facet_wrap(.~Year)+
        labs(x="years",y="CFC future predictions")



#for average annual temperature
#co2
x=combine_data$T
y=combine_data$CO2

#ch4
x=combine_data$T
y=combine_data$CH4

#N2O
x=combine_data$T
y=combine_data$N2O

#CFCS
x=combine_data$T
y=combine_data$CFCs

#HCFCs
x=combine_data$T
y=combine_data$HCFCs

#HFCs
x=combine_data$T
y=combine_data$HFCs

plot(x,y)
cor(x,y)

#testing 

data1<- lm( T ~ CO2,combine_data)
a <- data.frame(CO2=c(2.111))
result <- predict (data1,a)
print (result)

#prediction

data1<- lm(T~CO2,combine_data)
a <- data.frame(CO2=c(diff_data$CO2))
result <- predict (data1,a)
print (result)
diff_data$T_for_CO2=result

data2<-lm(T~CH4,combine_data)
b<-data.frame(CH4=c(diff_data$CH4))
result<-predict(data2,b)
print(result)
diff_data$T_for_CH4=result

data3<-lm(T~N2O,combine_data)
c<-data.frame(N2O=c(diff_data$N2O))
result<-predict(data3,c)
print(result)
diff_data$T_for_N2O=result

data4<-lm(T~CFCs,combine_data)
d<-data.frame(CFCs=c(diff_data$CFCs))
result<-predict(data4,d)
print(result)
diff_data$T_for_CFCs=result

data5<-lm(T~HCFCs,combine_data)
e<-data.frame(HCFCs=c(diff_data$HCFCS))
result<-predict(data5,e)
print(result)
diff_data$T_for_HCFCs=result

data6<-lm(T~HFCs,combine_data)
f<-data.frame(HFCs=c(diff_data$HFCs))
result<-predict(data6,f)
print(result)
diff_data$T_for_HFCs=result

#for rainfall

#CO2
x=combine_data$PP
y=combine_data$CO2
#CH4
x=combine_data$PP
y=combine_data$CH4
#N2O
x=combine_data$PP
y=combine_data$N2O
#CFCS
x=combine_data$PP
y=combine_data$CFCs
#HCFCS
x=combine_data$PP
y=combine_data$HCFCs
#HFCS
x=combine_data$PP
y=combine_data$HFCs

plot(x,y)
cor(x,y)

data6<-lm(PP~CO2,combine_data)
a<-data.frame(CO2=c(diff_data$CO2))
result<-predict(data6,a)
print(result)
diff_data$PP_for_CO2=result

data2<-lm(PP~CH4,combine_data)
b<-data.frame(CH4=c(diff_data$CH4))
result<-predict(data2,b)
print(result)
diff_data$PP_for_CH4=result

data3<-lm(PP~N2O,combine_data)
c<-data.frame(N2O=c(diff_data$N2O))
result<-predict(data3,c)
print(result)
diff_data$PP_for_N2O=result

data4<-lm(PP~CFCs,combine_data)
d<-data.frame(CFCs=c(diff_data$CFCs))
result<-predict(data4,d)
print(result)
diff_data$PP_for_CFCs=result

data5<-lm(PP~HCFCs,combine_data)
e<-data.frame(HCFCs=c(diff_data$HCFCS))
result<-predict(data5,e)
print(result)
diff_data$PP_for_HCFCs=result

data6<-lm(PP~HFCs,combine_data)
f<-data.frame(HFCs=c(diff_data$HFCs))
result<-predict(data6,f)
print(result)
diff_data$PP_for_HFCs=result

#wind speed data

#CO2
x=combine_data$V
y=combine_data$CO2
#CH4
x=combine_data$V
y=combine_data$CH4
#N2O
x=combine_data$V
y=combine_data$N2O
#CFCS
x=combine_data$V
y=combine_data$CFCs
#HCFCS
x=combine_data$V
y=combine_data$HCFCs
#HFCS
x=combine_data$V
y=combine_data$HFCs

plot(x,y)
cor(x,y)

data6<-lm(V~CO2,combine_data)
a<-data.frame(CO2=c(diff_data$CO2))
result<-predict(data6,a)
print(result)
diff_data$V_for_CO2=result

data2<-lm(V~CH4,combine_data)
b<-data.frame(CH4=c(diff_data$CH4))
result<-predict(data2,b)
print(result)
diff_data$V_for_CH4=result

data3<-lm(V~N2O,combine_data)
c<-data.frame(N2O=c(diff_data$N2O))
result<-predict(data3,c)
print(result)
diff_data$V_for_N2O=result

data4<-lm(V~CFCs,combine_data)
d<-data.frame(CFCs=c(diff_data$CFCs))
result<-predict(data4,d)
print(result)
diff_data$V_for_CFCs=result

data5<-lm(V~HCFCs,combine_data)
e<-data.frame(HCFCs=c(diff_data$HCFCS))
result<-predict(data5,e)
print(result)
diff_data$V_for_HCFCs=result

data6<-lm(V~HFCs,combine_data)
f<-data.frame(HFCs=c(diff_data$HFCs))
result<-predict(data6,f)
print(result)
diff_data$V_for_HFCs=result

write.table(diff_data, file="climatedata.csv", sep=",")

#future prediction graphs
#for temp
#CO2
ggplot(combine_data,aes(x=Year,y=T))+geom_line(colour="red",size=1)
ggplot(diff_data,aes(x=Year,y=T_for_CO2))+geom_line(colour="blue",size=1)
#CH4
ggplot(combine_data,aes(x=Year,y=T))+geom_line(colour="red",size=1)
ggplot(diff_data,aes(x=Year,y=T_for_CH4))+geom_line(colour="pink",size=1)
#N2O
ggplot(combine_data,aes(x=Year,y=T))+geom_line(colour="red",size=1)
ggplot(diff_data,aes(x=Year,y=T_for_N2O))+geom_line(colour="green",size=1)
#CFCs
ggplot(combine_data,aes(x=Year,y=T))+geom_line(colour="red",size=1)
ggplot(diff_data,aes(x=Year,y=T_for_CFCs))+geom_line(colour="yellow",size=1)
#HCFCs
ggplot(combine_data,aes(x=Year,y=T))+geom_line(colour="red",size=1)
ggplot(diff_data,aes(x=Year,y=T_for_HCFCs))+geom_line(colour="orange",size=1)
#HFCs
ggplot(combine_data,aes(x=Year,y=T))+geom_line(colour="red",size=1)
ggplot(diff_data,aes(x=Year,y=T_for_HFCs))+geom_line(colour="purple",size=1)

#for rainfall
#CO2
ggplot(combine_data,aes(x=Year,y=PP))+geom_line(colour="red",size=1)
ggplot(diff_data,aes(x=Year,y=PP_for_CO2))+geom_line(colour="blue",size=1)
#CH4
ggplot(combine_data,aes(x=Year,y=PP))+geom_line(colour="red",size=1)
ggplot(diff_data,aes(x=Year,y=PP_for_CH4))+geom_line(colour="pink",size=1)
#N2O
ggplot(combine_data,aes(x=Year,y=PP))+geom_line(colour="red",size=1)
ggplot(diff_data,aes(x=Year,y=PP_for_N2O))+geom_line(colour="green",size=1)
#CFCs
ggplot(combine_data,aes(x=Year,y=PP))+geom_line(colour="red",size=1)
ggplot(diff_data,aes(x=Year,y=PP_for_CFCs))+geom_line(colour="yellow",size=1)
#HCFCs
ggplot(combine_data,aes(x=Year,y=PP))+geom_line(colour="red",size=1)
ggplot(diff_data,aes(x=Year,y=PP_for_HCFCs))+geom_line(colour="orange",size=1)
#HFCs
ggplot(combine_data,aes(x=Year,y=PP))+geom_line(colour="red",size=1)
ggplot(diff_data,aes(x=Year,y=PP_for_HFCs))+geom_line(colour="purple",size=1)

#wind
#CO2
ggplot(combine_data,aes(x=Year,y=V))+geom_line(colour="red",size=1)
ggplot(diff_data,aes(x=Year,y=V_for_CO2))+geom_line(colour="blue",size=1)
#CH4
ggplot(combine_data,aes(x=Year,y=V))+geom_line(colour="red",size=1)
ggplot(diff_data,aes(x=Year,y=V_for_CH4))+geom_line(colour="pink",size=1)
#N2O
ggplot(combine_data,aes(x=Year,y=V))+geom_line(colour="red",size=1)
ggplot(diff_data,aes(x=Year,y=V_for_N2O))+geom_line(colour="green",size=1)
#CFCs
ggplot(combine_data,aes(x=Year,y=V))+geom_line(colour="red",size=1)
ggplot(diff_data,aes(x=Year,y=V_for_CFCs))+geom_line(colour="yellow",size=1)
#HCFCs
ggplot(combine_data,aes(x=Year,y=V))+geom_line(colour="red",size=1)
ggplot(diff_data,aes(x=Year,y=V_for_HCFCs))+geom_line(colour="orange",size=1)
#HFCs
ggplot(combine_data,aes(x=Year,y=V))+geom_line(colour="red",size=1)
ggplot(diff_data,aes(x=Year,y=V_for_HFCs))+geom_line(colour="purple",size=1)



















