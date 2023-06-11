mydata = read.csv(file.choose())
view(mydata)
genres = c(unique(mydata$Genre))
year = 2007
t = table(mydata$Genre)
print(t)
pie(t)

print(paste("For Year",year,sep=" "))
print(" ")
for (i in 1:length(genres)){
y = subset(mydata,mydata$Genre==genres[i])
z = subset(y,y$Year==2007)
print(genres[i])
print(" ")

print(summary(z))
hist(z$NA_Sales,main=paste(genres[i],"NA",sep="-"),xlab="NA_Sales",ylab="Frequency")
hist(z$EU_Sales,main=paste(genres[i],"EU",sep="-"),xlab="EU_Sales",ylab="Frequency")
hist(z$JP_Sales,main=paste(genres[i],"JP",sep="-"),xlab="JP_Sales",ylab="Frequency")
hist(z$Other_Sales,main=paste(genres[i],"Other",sep="-"),xlab="Other_Sales",ylab="Frequency")
hist(z$Global_Sales,main=paste(genres[i],"Global",sep="-"),xlab="Global_Sales",ylab="Frequency")

#NA
print(paste("Mean of NA Sales:",mean(z$NA_Sales),sep=" "))
print(paste("Median of NA Sales:",median(z$NA_Sales),sep=" "))
print(paste("Standard Deviation of NA Sales:",sd(z$NA_Sales),sep=" "))
print("_____________________________")
#EU
print(paste("Mean of EU Sales:",mean(z$EU_Sales),sep=" "))
print(paste("Median of EU Sales:",median(z$EU_Sales),sep=" "))
print(paste("Standard Deviation of EU Sales:",sd(z$EU_Sales),sep=" "))
print("_____________________________")
#JP
print(paste("Mean of JP Sales:",mean(z$JP_Sales),sep=" "))
print(paste("Median of JP Sales:",median(z$JP_Sales),sep=" "))
print(paste("Standard Deviation of JP Sales:",sd(z$JP_Sales),sep=" "))
print("_____________________________")
#Other
print(paste("Mean of Other Sales:",mean(z$Other_Sales),sep=" "))
print(paste("Median of Other Sales:",median(z$Other_Sales),sep=" "))
print(paste("Standard Deviation of Other Sales:",sd(z$Other_Sales),sep=" "))
print("_____________________________")
#Global
print(paste("Mean of Global Sales:",mean(z$Global_Sales),sep=" "))
print(paste("Median of Global Sales:",median(z$Global_Sales),sep=" "))
print(paste("Standard Deviation of Global Sales:",sd(z$Global_Sales),sep=" "))
print("_____________________________")
RegModel = lm(z$Global_Sales~z$JP_Sales+z$NA_Sales)
print(RegModel)
}
