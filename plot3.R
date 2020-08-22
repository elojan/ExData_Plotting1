plot3 <- function(directorio) {
  library(dplyr)
 
  setwd(paste0("C:/Users/Lenovo/OneDrive - Universidad de Guayaquil/Ejercicios R/",directorio,"/"))
  data1 <- read.csv("household_power_consumption.txt", na=c("?"), col.names = c("Fecha","hora","gapow","grpow","volt","gwint","sm1","sm2","sm3"), sep = ";")
  data1 <- filter(data1, Fecha %in% c("1/2/2007", "2/2/2007"))
  
  Fecha <- as.POSIXct(strptime(paste(data1$Fecha, data1$hora),"%d/%m/%Y %H:%M:%S"))
  y <- cbind(sub_met1=data1$sm1,sub_met2=data1$sm2,sub_met3=data1$sm3)
  
  matplot(Fecha,y,type="l", lty = 1:1, col = c('black', 'red', 'blue'), xlab=" ", ylab = "Energy sub metering")
  legend("topright", inset=0.01, legend=colnames(y),col = c('black', 'red', 'blue'),pch=1, bg= ("white"))
  
}