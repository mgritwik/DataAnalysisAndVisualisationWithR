library(plyr)
library(reshape2)

# Read Data
df1 <- readRDS("C:\\Users\\I349794\\Desktop\\InnovationTeam\\CourseraExlporatoryDataAnalysis\\summarySCC_PM25.rds")
df2 <- readRDS("C:\\Users\\I349794\\Desktop\\InnovationTeam\\CourseraExlporatoryDataAnalysis\\Source_Classification_Code.rds")
str(df1)

# Plot 1 - Total PM2.5 Emissions across US over the period 1999-2008
x<-tapply(df1$Emissions,df1$year,sum)
a<-as.integer(names(x))
b<-as.integer(x)
plot(a,b,type = "l",xlab = "Years",ylab = "PM2.5 Totoal Emissions",main = "PM2.5 Trend over the years across USA")

# Plot 2 - Total PM2.5 Emissions in Baltimore,Maryland over the period 1999-2008
df1_temp<-df1[df1$fips=="24510",]
str(df1_temp)
x<-tapply(df1_temp$Emissions,df1_temp$year,sum)
a<-as.integer(names(x))
b<-as.integer(x)
plot(a,b,type = "l",xlab = "Years",ylab = "PM2.5 Totoal Emissions for Baltimore",main = "PM2.5 Trend over the years across Baltimore,Maryland")

# Plot 3 - Total PM2.5 Emissions across US for each "Type" category over the period 1999-2008
df_melt<-df1[,c("type","year","Emissions")]
df_melt<-melt(df_melt,id=c("year","type"))
df_cast<-dcast(df_melt,type+year~variable,sum)
t1<-df_cast[df_cast$type=="NON-ROAD","Emissions"]
t2<-df_cast[df_cast$type=="NONPOINT","Emissions"]
t3<-df_cast[df_cast$type=="ON-ROAD","Emissions"]
t4<-df_cast[df_cast$type=="POINT","Emissions"]
temp<-rbind(t1,t2,t3,t4)
matplot(a,t(temp),type="l",xlab = "Years",ylab = "PM2.5 per Type",main="PM2.5 per Type across years",col=1:4) #transpose of the matrix
legend("topleft", legend = 1:4, col=1:4, pch=1)

# Plot 4 - Total PM2.5 Emissions from coal combustion-related sources
vec1<-grep("coal+",df2$EI.Sector,ignore.case = T,perl = T,value = F)
sub<-df2$SCC[vec1]
df1_temp<-df1[df1$SCC %in% sub,c("Emissions","year")]
x<-tapply(df1_temp$Emissions,df1_temp$year,sum)
a<-as.integer(names(x))
b<-as.integer(x)
plot(a,b,type = "l",xlab = "Years",ylab = "PM2.5 Totoal Emissions",main = "PM2.5 Trend from coal combustion-related sources")

# Plot 5 - Total PM2.5 Emissions from motor vehicle sources in Baltimore
df1_temp<-df1[df1$fips=="24510",]
vec1<-grep("vehicle+",df2$EI.Sector,ignore.case = T,perl = T,value = F)
sub<-df2$SCC[vec1]
df1_temp<-df1_temp[df1_temp$SCC %in% sub,c("Emissions","year")]
head(df1_temp)
x<-tapply(df1_temp$Emissions,df1_temp$year,sum)
a<-as.integer(names(x))
b<-as.integer(x)
plot(a,b,type = "l",xlab = "Years",ylab = "PM2.5 Totoal Emissions",main = "PM2.5 Trend motor vehicle sources changed in Baltimore City")

# Plot 6 - Total PM2.5 Emissions from motor vehicle sources in Baltimore vs Los angeles
df1_temp<-df1[df1$fips=="06037",]
df1_temp<-df1_temp[df1_temp$SCC %in% sub,c("Emissions","year")]
x<-tapply(df1_temp$Emissions,df1_temp$year,sum)
a<-as.integer(names(x))
c<-as.integer(x)
b<-rbind(b,c)
matplot(a,t(b),type="l",xlab = "Years",ylab = "PM2.5 Totoal Emissions",main="PM2.5 from motor vehicle sources-Baltimore vs Los Angeles",col=1:2)
legend("topleft", legend = 1:2, col=1:4, pch=1)