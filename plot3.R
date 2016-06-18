library(dplyr)

## @brief downloading, reading and converting data
# @return data.frame containing 1 column for DateTime (POSIXct) and 7 numeric columns
read_and_prepare_data <- function()
{
  fileName <- "household_power_consumption.txt"
  if(!file.exists(fileName))
  {
    fileUrl <- "http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    download.file(fileUrl, "somezip.zip", method="curl")
    unzip("somezip.zip", exdir=".")
    file.remove("somezip.zip")
  }
  content <- read.csv(file=fileName,
                      sep=";",
                      colClasses = c(rep("character",2), rep("numeric",7)),
                      na.string="?"
                     )
  content <- mutate(content, DateTime = as.POSIXct(strptime(paste(Date, Time, sep=" "), format="%d/%m/%Y %H:%M:%S")))
  content <- select(content, DateTime, Global_active_power:Sub_metering_3)
  content
}

## @brief fitering data in between two days
# @param[in] data     dataset with column 'DateTime' containing time in POSIXct
# @param[in] fromDate character string with format 'YYYY-mm-dd'
# @param[in] toDate   character string with format 'YYYY-mm-dd'
# @return dataset containing only data from the time: fromDate <= time < toDate
filter_timespan <- function(data, fromDate, toDate)
{
  from = as.POSIXct(fromDate)
  to = as.POSIXct(toDate)
  filter(data, from <= DateTime & DateTime < to)
}



data <- read_and_prepare_data()
data <- filter_timespan(data, fromDate="2007-02-01", toDate="2007-02-03")

Sys.setlocale("LC_TIME", "C") # for correct weekdays

png("plot3.png", width=480, height=480, units="px", bg="transparent")
with(data, plot(x=DateTime, y=Sub_metering_1, type="l", ylab="Energy sub metering", xlab=""))
with(data, lines(x=DateTime, y=Sub_metering_2, col="red"))
with(data, lines(x=DateTime, y=Sub_metering_3, col="blue"))
legend("topright", lty=1, legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black","red","blue"))
dev.off()
