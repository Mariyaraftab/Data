install.packages("fredr")
library(fredr)
library(MFVART)
#df <- read.csv2(url("https://raw.githubusercontent.com/Mariyaraftab/Data/main/data.csv"))
#head(df)
setwd("C:/Users/mrb/Desktop/Data")
DatM_name <- "datam.csv"
DatQ_name <- "dataq.csv"
headers <- read.csv(DatM_name, header = F, nrows = 1, as.is = T,sep = ";", dec = ",")
Monthly <- read.csv(DatM_name, sep = ";", dec = ",",skip = 2, stringsAsFactors = FALSE)
colnames(Monthly) <- headers
headers <- read.csv(DatQ_name, header = F, nrows = 1, as.is = T,sep = ";", dec = ",")
Quarterly <- read.csv(DatQ_name, sep = ";", dec = ",",skip = 2, stringsAsFactors = FALSE)
colnames(Quarterly) <- headers
lag <- 12
t_max <- nrow(Monthly)
Time <- as.Date(Monthly$date, format = "%Y.%m.%d")[(lag+1):t_max]
Var1 <-  diff(Monthly$ipi, lag = lag) 
Var2 <- diff(log(Monthly$CPI), lag = lag) * 100
Var3 <-  diff(Monthly$trebil, lag = lag) 
Var4 <-  diff(Monthly$Govtde, lag = lag) 
Monthly <- data.frame(Time = Time, 
                      IPI = Var1, 
                      CPI = Var2, 
                      trebil = Var3,
                      Govtde = Var4)
# Remove spaces and convert GDP to numeric
GDP <- as.numeric(gsub(" ", "", Quarterly$GDP))
GDP_growth <- 100*diff(log(GDP),lag = 1) # Using 1 lag instead of 4 lags.
Time_q <- as.Date(Quarterly$Date, format = "%Y.%m.%d")[2:nrow(Quarterly)]
Quarterly <- data.frame(Time = Time_q, GDP_growth = GDP_growth)
Data <- merge(x = Monthly, y = Quarterly, by = "Time", all.x = T)
