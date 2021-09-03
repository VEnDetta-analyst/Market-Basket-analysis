#Required packages
library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)

#read excel file
retail=read_csv('C:/Users/VEDANG SAWANT/Desktop/OnlineRetail.csv')

#complete.cases(data) will return a logical vector indicating which rows have no missing values. Then use the vector to get only rows that are complete using retail[,].
retail=retail[complete.cases(retail),]

#mutate function is from dplyr package. It is used to edit or add new columns to dataframe. Here Description column is being converted to factor column. as.factor converts column to factor column. %>% is an operator with which you may pipe values to another function or expression
retail%>%mutate(Description=as.factor(Description))
retail%>%mutate(Country=as.factor(Country))

#Converts character data to date. Store InvoiceDate as date in new variable
retail$Date=as.Date(retail$InvoiceDate)

#Extract time from InvoiceDate and store in another variable
TransTime=format(retail$InvoiceDate,format="%H:%M:%S")

#Convert and edit InvoiceNo into numeric
InvoiceNo=as.numeric(as.character(retail$InvoiceNo))

#Bind new columns TransTime and InvoiceNo into dataframe retail
cbind(retail,TransTime)
cbind(retail,InvoiceNo)

#Get glimpse of your data
glimpse(retail)

library(plyr)

transactionData <- ddply(retail,c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))
transactionData

#Next, as InvoiceNo and Date will not be of any use in the rule mining, you can set them to NULL.
transactionData$InvoiceNo <- NULL
transactionData$Date <- NULL
colnames(transactionData) <- c("items")
transactionData


#Saving Basket data in csv file.
write.csv(transactionData,'C:/Users/VEDANG SAWANT/Desktop/basketdata.csv',quote = FALSE, row.names = FALSE)

tr=read.transactions('C:/Users/VEDANG SAWANT/Desktop/basketdata.csv',format='basket',sep=',')
tr
summary(tr)

# Create an item frequency plot for the top 20 items
library(RColorBrewer)
#ABSOLUTE frequency plot
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
#RELATIVE frequency plot
itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

#APRORI rule
# Min Support as 0.001, confidence as 0.8.
association=apriori(tr,parameter = list(supp=0.001, conf=0.8, maxlen=10))

#Top 10 rules
inspect(association[1:10])

#Limiting the number and size of rules
shorter.assocation=apriori(tr,parameter = list(sup=0.001,conf=0.8,maxlen=3))

#You can remove rules that are subsets of larger rules
subset.rules=which(colSums(is.subset(association,association))>1)
length(subset.rules)
subset.association=association[-subset.rules]



#. If you want to find out what causes influence on the purchase of item X
#to find what customers buy before buying 'METAL'
association.metal=apriori(tr,parameter = list(sup=0.001,conf=0.8,maxlen=3),appearance = list(default='lhs',rhs='METAL'))
# Here lhs=METAL because you want to find out the probability of that in how many customers buy METAL along with other items
inspect(head(association.metal))


#to find the answer to the question Customers who bought METAL also bought...
metal.association <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(lhs="METAL",default="rhs"))
inspect(head(metal.association))

#Visualizing association rules
subrules=association[quality(association)$confidence>0.4]
plot(subrules)

plot(subrules,method="two-key plot")

top10subRules= head(subrules, n = 10, by = "confidence")
plot(top10subRules,method='graph',engine = 'htmlwidget')
