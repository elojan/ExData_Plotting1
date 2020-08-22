plot2 <- function(directorio) {
  library(dplyr)
  library(ggplot2)
  library(scales)
  
  setwd(paste0("C:/Users/Lenovo/OneDrive - Universidad de Guayaquil/Ejercicios R/",directorio,"/"))
  data1 <- read.csv("household_power_consumption.txt", na=c("?"), col.names = c("Fecha","hora","gapow","grpow","volt","gwint","sm1","sm2","sm3"), sep = ";")
  data1 <- filter(data1, Fecha %in% c("1/2/2007", "2/2/2007"))
  data2 <- cbind.data.frame(Fecha = as.POSIXct(strptime(paste(data1$Fecha, data1$hora),"%d/%m/%Y %H:%M:%S")), gapow=data1$gapow)
  
  ggplot(data2, aes(Fecha, gapow)) + 
  geom_line(stat="identity", na.rm = TRUE) + 
  ggtitle("Plot 2") + xlab("") + 
  ylab("Global Active Power (kilowatts)") + 
  scale_x_datetime(labels=date_format (("%a")), date_breaks = "1 day") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) + 
  theme(text = element_text(size=18))
  
  
}