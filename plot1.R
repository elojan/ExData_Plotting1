
plot1 <- function(directorio) {
  library(dplyr)
  setwd(paste0("C:/Users/Lenovo/OneDrive - Universidad de Guayaquil/Ejercicios R/",directorio,"/"))
  data1 <- read.csv("household_power_consumption.txt", na=c("?"), col.names = c("Fecha","hora","gapow","grpow","volt","gwint","sm1","sm2","sm3"), sep = ";")
  data_plot1 <- filter(data1, Fecha=="1/2/2007")
  data_plot2 <- filter(data1, Fecha=="2/2/2007")
  data_plot3 <- rbind(data_plot1, data_plot2)
  data_plot <- select(data_plot3, gapow) %>% unlist
  hist(data_plot, main = "Global Active Power", col = "red", xlab = "Global Active Power(kilowatts)")
}