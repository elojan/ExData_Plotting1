plot4 <- function(directorio) {

  library(dplyr)
  library(ggplot2)
  library(reshape2)
  library(scales)
  library(gridExtra)

  
  setwd(paste0("C:/Users/Lenovo/OneDrive - Universidad de Guayaquil/Ejercicios R/",directorio,"/"))
  data1 <- read.csv("household_power_consumption.txt", na=c("?"), col.names = c("Fecha","hora","gapow","grpow","volt","gwint","sm1","sm2","sm3"), sep = ";")
  data1 <- filter(data1, Fecha %in% c("1/2/2007", "2/2/2007"))
  data2 <- cbind.data.frame(Fecha = as.POSIXct(strptime(paste(data1$Fecha, data1$hora),"%d/%m/%Y %H:%M:%S")), gapow=data1$gapow)
  data3 <- cbind.data.frame(Fecha = as.POSIXct(strptime(paste(data1$Fecha, data1$hora),"%d/%m/%Y %H:%M:%S")), sub_met1=data1$sm1,sub_met2=data1$sm2,sub_met3=data1$sm3)
  data4 <- cbind.data.frame(Fecha = as.POSIXct(strptime(paste(data1$Fecha, data1$hora),"%d/%m/%Y %H:%M:%S")), voltaje = data1$volt)
  data5 <- cbind.data.frame(Fecha = as.POSIXct(strptime(paste(data1$Fecha, data1$hora),"%d/%m/%Y %H:%M:%S")), grpow = data1$grpow)
  
  d <- melt(data3, id.vars="Fecha")
  
  p1 <- ggplot(data2, aes(Fecha, gapow)) + 
    geom_line(stat="identity", na.rm = TRUE) + 
    ggtitle("") + xlab("") + 
    ylab("Global Active Power (kilowatts)") + 
    scale_x_datetime(labels=date_format (("%a")), date_breaks = "1 day") 
   
  
  p2 <- ggplot(d, aes(Fecha,value, col=variable)) + 
       geom_line() +  xlab("") + ylab("Energy sub metering") + 
       scale_x_datetime(labels=date_format (("%a")), date_breaks = "1 day") + 
       scale_colour_manual(values=c(sub_met1="black",sub_met2="red",sub_met3="blue")) +
       theme(legend.position = c(0.85, 0.85),legend.title = element_blank())
  
  p3 <- ggplot(data4, aes(Fecha, voltaje)) + 
    geom_line(stat="identity", na.rm = TRUE) + 
    ggtitle("") + xlab("hora-dia") + 
    ylab("Voltaje") + 
    scale_x_datetime(labels=date_format (("%a")), date_breaks = "1 day") 
  
  p4 <- ggplot(data5, aes(Fecha, grpow)) + 
    geom_line(stat="identity", na.rm = TRUE) + 
    ggtitle("") + xlab("hora-dia") + 
    ylab("Global Reactive Power") + 
    scale_x_datetime(labels=date_format (("%a")), date_breaks = "1 day") 
  
    grid.arrange(p1, p3, p2, p4, nrow = 2, ncol=2)
 
}