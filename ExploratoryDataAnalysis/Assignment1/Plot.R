
# Read data
df<-read.table("./household_power_consumption.txt",header = TRUE,sep = ";")

# Convert string to date format and filter the dataset for dates: 1st & 2nd Feb 2007
df$Date<-as.Date(df$Date, format = "%d/%m/%Y")
df2<-df[format(df$Date, "%Y")=="2007",]
df2<-df2[format(df2$Date, "%m")=="02",]
df2<-df2[(format(df2$Date, "%d")=="02"|format(df2$Date, "%d")=="01"),]

# Plot 1 - Plot Histogram for Global active power to analyde the shape of the usage.
list1<-as.numeric(df2$Global_active_power)
png("plot1.png", width=480, height=480)
hist(l2,col="red",main="Global Active Power", xlab="Global Active Power (kilowatts)")
dev.off()]]


# Plot 2 - Plot line graph to see the trend of the Global active power
plot(l2,type = "l",xlab = "Date",ylab = "Global Active Power(Kilowatt)")
 
# Plot 3 - Plot Submetering1,Submetering2 and Submetering3 on a single plot to see the difference in the trend.
abc<-cbind(as.integer(df3$Sub_metering_1),as.integer(df3$Sub_metering_2),as.integer(df3$Sub_metering_3))
matplot(abc,type="l",xlab="Date",ylab = "Energy in Metering")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, lwd=2.5, col=c("black", "red", "green"))

# plot4 - Plot 4 graphs side by side to get an overage summary of the voltage vs Global active and Global reactive power.
par(mfcol = c(2,2))
plot(l2,type = "l",xlab = "Date",ylab = "Global Active Power(Kilowatt)")
matplot(abc,type="l",xlab="Date",ylab = "Energy in Metering")
plot(as.integer(df3$Voltage),type = "l",xlab = "Date",ylab = "Voltage")
plot(as.integer(df3$Global_reactive_power),type = "l",xlab = "Date",ylab = "Global reactive power(Kilowatts)")