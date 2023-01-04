#problem 1
#reading file into a data frame
Shipments_data = read.csv('ApplianceShipments.csv')
#creating time series data frame
shipments_q = ts(Shipments_data$Shipments, start=c(1985,1), end=c(1989,4), freq = 4)
shipments_q
#create plot using ts data frame
plot(shipments_q, xlab = "Year", ylab = "Shipments", main="Appliance Shipments from 1985 to 1989")
#zooming the y axis
shipments_zoom = ts(Shipments_data$Shipments, start=c(1985,1), end=c(1989,4), freq = 4)
plot(shipments_zoom, xlab = "Year", ylab = "Shipments", main="Appliance Shipments from 1985 to 1989", ylim=c(3500,5000))

#separating data quarter wise
q1 = Shipments_data[grepl("Q1",Shipments_data$Quarter),]
q2 = Shipments_data[grepl("Q2",Shipments_data$Quarter),]
q3 = Shipments_data[grepl("Q3",Shipments_data$Quarter),]
q4 = Shipments_data[grepl("Q4",Shipments_data$Quarter),]
#plot graph for quarterly shipments
plot(ts(q1$Shipments, start=c(1985), end=c(1989)),col="green", xlab = "Year", ylab = "Shipments", ylim=c(3500,5000), main="Quarterly Appliance Shipments from 1985 to 1989")
lines(ts(q2$Shipments, start=c(1985), end=c(1989)), col="blue")
lines(ts(q3$Shipments, start=c(1985), end=c(1989)), col="red")
lines(ts(q4$Shipments, start=c(1985), end=c(1989)))
legend(x="bottomright",cex=0.25,legend = c("Q1","Q2","Q3","Q4"), fill = c("green","blue","red", "black"))


#prepare aggregate data 
year1=Shipments_data[grepl("1985",Shipments_data$Quarter),]
year2=Shipments_data[grepl("1986",Shipments_data$Quarter),]
year3=Shipments_data[grepl("1987",Shipments_data$Quarter),]
year4=Shipments_data[grepl("1988",Shipments_data$Quarter),]
year5=Shipments_data[grepl("1989",Shipments_data$Quarter),]
Total_sum=c(sum(year1$Shipments),sum(year2$Shipments),sum(year3$Shipments),
                 sum(year4$Shipments),sum(year4$Shipments))
all_years=c("1985","1986","1987","1988","1989")

aggregate_data1=data.frame(years=all_years,total=Total_sum)
#plot graph for aggregate data
plot(ts(aggregate_data1$total, start=c(1985), end=c(1989)), xlab = "Year", ylab = "Shipments", main="Aggregate Appliance Shipments from 1985 to 1989")



#problem 2
mower_data=read.csv('RidingMowers.csv')
head(mower_data)
library(ggplot2)
ggplot(mower_data,aes(x=Lot_Size,y=Income, col=Ownership))+geom_point() + labs(x="Lot Size")

#problem 3
Sales_data=read.csv('LaptopSalesJanuary2008.csv')
#get average data
Average_data= aggregate(Sales_data$Retail.Price, by=list(Sales_data$Store.Postcode), FUN=mean)
#plot average data
ggplot(data=Average_data, aes(x=Group.1, y=x)) +
  geom_bar(stat="identity")+ggtitle(label = "Average Retail Price by Store")+
  xlab("Store")+ylab("Average Retail Price")
# compute highest retail price
Average_data_sorted = Average_data[order(Average_data$x,decreasing = TRUE),]
head(Average_data_sorted)
#computing lowest retail price
Average_data_sorted = Average_data[order(Average_data$x,decreasing = FALSE),]
head(Average_data_sorted)

#part b
ggplot(Sales_data) + geom_boxplot(aes(x = Store.Postcode, y = Retail.Price)) + xlab("Stores")+ylab("Retail Price")
#new data frame for store in part a
Stores_price=Sales_data[c(Sales_data$Store.Postcode=="W4 3PH" | Sales_data$Store.Postcode=="N17 6QA"),]
head(Stores_price)
ggplot(Stores_price) + geom_boxplot(aes(x = Store.Postcode, y = Retail.Price)) + xlab("Stores")+ylab("Retail Price")

