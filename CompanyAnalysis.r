#loading the necessary packages
install.packages("plotrix")
library(plotrix)

#opening the dataset
df<-read.csv("fortune_2000_in_2021.csv")
df1<-subset(df,grepl(" M",df$Sales))

#Cleaning Sales column
#function to replace multiple patterns at once
df1$Sales<-gsub("\\$","",gsub(" M","",gsub(",","",df1$Sales)))
df1$Sales<-as.numeric(df1$Sales) #convert to numeric
df1$Sales<-df1$Sales/1000 #convert Million to Billion
#for billions
df2<-subset(df,grepl(" B",df$Sales))
df2$Sales<-gsub("\\$","",gsub(" B","",gsub(",","",df2$Sales)))
df2$Sales<-as.numeric(df2$Sales)

s<-rbind(df1,df2) #cleaned the sales column

#Cleaning Profit column
df3<-subset(s,grepl(" M",s$Profit))
df3$Profit<-gsub("\\$","",gsub(" M","",gsub(",","",df3$Profit)))
df3$Profit<-as.numeric(df3$Profit)
#convert Million to Billion
df3$Profit<-df3$Profit/1000

df4<-subset(s,grepl(" B",s$Profit))
df4$Profit<-gsub("\\$","",gsub(" B","",gsub(",","",df4$Profit)))
df4$Profit<-as.numeric(df4$Profit)

sp<-rbind(df3,df4) #cleaned the sales,profits column

#Cleaning Assets column 
df5<-subset(sp,grepl(" M",sp$Assets))
df5$Assets<-gsub("\\$","",gsub(" M","",gsub(",","",df5$Assets)))
df5$Assets<-as.numeric(df5$Assets)
#convert Million to Billion
df5$Assets<-df5$Assets/1000

df6<-subset(sp,grepl(" B",sp$Assets))
df6$Assets<-gsub("\\$","",gsub(" B","",gsub(",","",df6$Assets)))
df6$Assets<-as.numeric(df6$Assets)

spa<-rbind(df5,df6) 

#Cleaning Market.Value column
df7<-subset(spa,grepl(" M",spa$"Market.Value"))
df7$"Market.Value"<-gsub("\\$","",gsub(" M","",gsub(",","",df7$"Market.Value")))
df7$"Market.Value"<-as.numeric(df7$"Market.Value")
#convert Million to Billion
df7$"Market.Value"<-df7$"Market.Value"/1000

df8<-subset(spa,grepl(" B",spa$"Market.Value"))
df8$"Market.Value"<-gsub("\\$","",gsub(" B","",gsub(",","",df8$"Market.Value")))
df8$"Market.Value"<-as.numeric(df8$"Market.Value")

spam<-rbind(df7,df8) #cleaned the sales,profits,assets and market.value column

#spam is the dataset with cleaned SALES,PROFIT,ASSETS,MARKET.VALUE columns

#1.PLOTTING THE NUMBER OF COMPANIES PER COUNTRY USING  2D PIE CHART

countries<-table(spam$Country) #named vector with country_names(values) and counts
topcountries<-sort(countries,decreasing=TRUE)[1:10]
remcountries<-sort(countries,decreasing=TRUE)[11:length(countries)]
rem<-as.numeric(remcountries)
rem<-c("Others"=sum(rem)) # Others 503 

countries<-c(topcountries,rem)
piepercent<-round(100*countries/sum(countries),1)
par(oma=c(0,0,0,0)) #set outer margins to default
par(mar=c(2,2,2,2)+0.1)
pie(countries,labels=piepercent,cex=0.7,main="Percentage(%) of Countries with Top 2000 companies",col=rainbow(11))
legend("topright",names(countries),fill=rainbow(length(countries)),cex=0.65)


#default
#2.PLOTTING TOP 10 COMPANY SALES USING BARPLOT

#sort the data frame by descending order of Sales

par(oma=c(6,1,0,1)) #Add outer margins
par(mar=c(5,4,4,2)+0.1) #reset inner margins to default
topsales<-spam[order(spam$Sales,decreasing=TRUE),]
barplot(topsales$Sales[1:10],names.arg=topsales$Name[1:10],ylab="Sales",main="Top 10 Sales (in $Billion) by companies 2021",las=2,col=rainbow(10))


#3.PLOTTING TOP 10 COMPANY PROFITS USING BARPLOT

#sort the data frame by descending order of Profit
par(mar=c(5,4,4,2)+0.1) #reset inner margins to default
par(oma=c(10,1,0,1)) #Add outer margins
topprofit<-spam[order(spam$Profit,decreasing=TRUE),]
barplot(topprofit$Profit[1:10],names.arg=topprofit$Name[1:10],ylab="Profit",main="Top 10 Profits (in $Billion) by companies 2021",las=2,col=rainbow(10))


#4.PLOTTING MARKET VALUE OF TOP 10 COMPANIES USING 3D PIE CHART
par(oma=c(0,0,0,0)) #set outer margins to default
par(mar=c(2,2,2,2)+0.1)
topmarket<-spam[order(spam$Market.Value,decreasing=TRUE),]
pie3D(topmarket$Market.Value[1:10],labels=topmarket$Market.Value[1:10],main="Market Value of top 10 companies in $Billion")
legend("topright",topmarket$Name[1:10],fill=rainbow(10),cex=0.45)

#5.PLOTTING ASSETS OF TOP 10 COMPANIES USING 3D PIE CHART
par(oma=c(0,0,0,0)) #set outer margins to default
par(mar=c(2,2,2,2)+0.1)
topassets<-spam[order(spam$Assets,decreasing=TRUE),]
pie3D(topmarket$Assets[1:10],labels=topmarket$Assets[1:10],main="Total Assets of top 10 companies in $Billion")
legend("topright",topassets$Name[1:10],fill=rainbow(10),cex=0.55)


#6.Plotting Rank vs Sales data and linear regression line for the plot.
par(oma=c(0,0,0,0)) #set outer margins to default
par(mar=c(5,4,4,2)+0.1) #reset inner margins to default
rank<-spam$Rank
sales<-spam$Sales
model<-lm(rank~sales,data=spam)
plot(rank~sales,xlab="Sales in Billion($)",ylab="Company rank",main="Rank vs Sales linear model",pch=20,col="blue",cex=0.4)
abline(model,col="red")
mtext(paste("Correlation coefficient=",format(round(cor(rank,sales),3),nsmall=3)),side=3,adj=1,col="red")


#7.Predicting the rank of a company in this dataset by inputting sales data:
predictRank<-function(){
sale<-readline()
sale<-as.numeric(sale)
newsale<-data.frame(sales=sale) #give the sales to predict rank in the given dataset
print(paste("The estimated rank in the dataset is :",round(predict(model,newsale))))
}
predictRank()


