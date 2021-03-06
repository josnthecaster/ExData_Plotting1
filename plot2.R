#make a funtion to load the data and not to do it every time
load_data <- function(){
    #I prefer data.table to load fast!
    if(require(data.table)){
        midata <- fread("household_power_consumption.txt")
        #find the dates asked
        subdata <- midata$Date == "1/2/2007" | midata$Date == "2/2/2007"
        midata <- midata[subdata]
        #make it a normal data.frame
        midata <- as.data.frame(midata)
        #change the columns to ther respective kind of values
        for(nombre in colnames(midata)[3:length(colnames(midata))]){
            midata[nombre] <- as.numeric(midata[[nombre]])
        }
        #make a column of times
        midata$DateTime <- paste(midata$Date,midata$Time)
        midata$DateTime <- strptime(midata$DateTime,"%d/%m/%Y %H:%M:%S")
    }else{
        #if data.table is not installed, use read.table...slow
        clases <- c("character","character","numeric","numeric",
                    "numeric","numeric","numeric","numeric","numeric")
        midata <- read.table("household_power_consumption.txt",
                             TRUE,colClasses=clases,sep=";",
                             na.strings = "?")
        subdata <- midata$Date == "1/2/2007" | midata$Date == "2/2/2007"
        midata <- midata[subdata,]
        midata <- as.data.frame(midata)
        #make a column of times
        midata$DateTime <- paste(midata$Date,midata$Time)
        midata$DateTime <- strptime(midata$DateTime,"%d/%m/%Y %H:%M:%S")
    }
    #return the data
    midata
}
#make a funtion to load the plot
plot2 <- function(midata){
    #the ploooooooot (make sure it is the only one)
    par(mfcol = c(1, 1))
    with(midata,plot(DateTime,Global_active_power, type = "l",
                     ylab = "Global Active Power (kilowatts)", xlab = ""))
}
#make a plot and save it
save_plot2 <- function(midata){
    png("plot2.png",480,480)
    plot2(midata)
    dev.off()
}
#if the object exists, just make the graph!
if(sum(as.vector(objects())=="sliced_data") > 0){
    plot2(sliced_data)
    save_plot2(sliced_data)
}else{
    #else load the data and make the plot,
    #this will allow to save a lot of time
    sliced_data <- load_data()
    plot2(sliced_data)
    save_plot2(sliced_data)
}