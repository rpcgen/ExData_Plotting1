createDataObject <- function(basedir='data') {

    url <- 'http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
    datafile <- sprintf('%s/data.zip', basedir)

    keyfiles <- list(
        hpc = sprintf('%s/household_power_consumption.txt', basedir),
        fhpc = sprintf('%s/filtered_household_power_consumption.txt', basedir)
    )

    getData <- function(...) {

        if (!all(file.exists(unlist(keyfiles)))) {

            if (file.exists(basedir)) {
                unlink(basedir, recursive=T)
            }

            dir.create(basedir)
            download.file(url, datafile, ...)
            unzip(datafile, exdir=basedir)

            data <- read.table(keyfiles$hpc, sep=';', header=T, na.strings='?')
            data <- data[data$Date %in% c('1/2/2007', '2/2/2007'),]

            data$DateTime <- as.POSIXct(paste(data$Date, data$Time), format='%d/%m/%Y %H:%M:%S')

            data <- data[,3:10]
            colnames(data) <- c(
                'GlobalActivePower',
                'GlobalReactivePower',
                'Voltage',
                'GlobalIntensity',
                'SubMetering1',
                'SubMetering2',
                'SubMetering3',
                'DateTime')

            write.csv(data, file=keyfiles$fhpc, row.names=F)

            return(data)
        }
    }

    loadData <- function() {
        dataClasses <- c(
            'numeric',  # Global active power
            'numeric',  # Global reactive power
            'numeric',  # Voltage
            'numeric',  # Global intensity
            'integer',  # Sub metering 1
            'integer',  # Sub metering 2
            'integer',  # Sub metering 3
            'POSIXct')  # Date and time
        data <- read.csv(keyfiles$fhpc, colClasses=dataClasses)
        return(data)
    }

    list(getData = getData, loadData = loadData)
}

hpc <- {
    dataobj <- createDataObject()
    dataobj$getData()
    dataobj$loadData()
}

plot3 <- function() {
    par(mfcol=c(1,1))
    plot(hpc$DateTime, hpc$SubMetering1, type='l', xlab='', ylab='Energy sub metering')
    lines(hpc$SubMetering2)
    lines(hpc$DateTime, hpc$SubMetering2, col='blue')
    lines(hpc$DateTime, hpc$SubMetering3, col='red')
    legend('topright', legend=colnames(hpc)[5:7], col=c('black', 'blue', 'red'), lty=c(1,1))
}

plot3()
png('plot3.png')
plot3()
dev.off()
