#setting the Working Directory
setwd("E:/M/coursera/data_science_specialization/Rstudio projects/Exploratory Data Analysis/course4")


#Downloading the Data
if(!file.exists("./course4")){dir.create("./course4")}
download.file(url='https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip', destfile='cons.zip', method='auto')
unzip(zipfile='cons.zip')


#Checking the Memory Required
memory_required <- 9 * 2075259 * 8 #bytes/numeric
mem_in_MBs <- memory_required / 2^20  #bytes/MB
mem_in_MBs


#Reading the data
df1 <- read.table("household_power_consumption.txt", sep = ";", header = TRUE)


#Converting the Date Column from factor to date
df1$Date <- as.Date(df1$Date, "%d/%m/%Y")
date1 <- as.Date("01/02/2007", "%d/%m/%Y")
date2 <- as.Date("02/02/2007", "%d/%m/%Y" )


df <-  df1[df1$Date %in% date1:date2, ]
ds <-  df1[df1$Date %in% date1:date2, ]

datetime <- strptime(paste(ds$Date, ds$Time, sep=" "), "%d/%m/%Y %H:%M:%S") 
globalActivePower <- as.numeric(ds$Global_active_power)/500
hist(globalActivePower, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")

nrow(df) 
summary(df)

head(df, 2); tail(df, 2)
head(df, 5)

#Plot 1
png("plot1.png", width = 480, height = 480)
hist(as.numeric(df$Global_active_power)/ 500, col = "red", main = "Global Active Power",
     xlab = "Global Active Power (kilowatts)", ylim = c(0, 1200))
dev.off()


#Plot 2
b <- ts(df$Global_active_power)
png("plot2.png", width = 480, height = 480)
plot(b/500, ylab = "Global Active Power (kilowatts)")
dev.off()


#plot 3
dfc <- df[,c(7:9)]
head(dfc)
d <- ts(dfc)
png("plot3.png", width = 480, height = 480)
plot(d, plot.type = "single", xaxt = "n", ylab = "Energy sub metering", col = c("black", "red", "blue"))
legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col=c("black", "red", "blue"), lty=1, cex=0.8)

#axis(2, df$Date, format(weekdays(df$Date)), cex.axis = .8)
dev.off()


#plot 4
dev.off()
png("plot4.png", width = 480, height = 480)
par(mfrow = c(2,2))

plot(b)

e <- ts(df[,5])
plot(e)

plot(d, plot.type = "single", xaxt = "n", ylab = "Energy sub metering", col = c("black", "red", "blue"))
legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col=c("black", "red", "blue"), bty = "n", lty=1, cex=0.65)

f <- ts(df[,4])
plot(f, ylab = "Global_reactive_power", xlab = "datetime")

dev.off()


