install.packages("fredr")
library(fredr)
library(MFVART)
#df <- read.csv2(url("https://raw.githubusercontent.com/Mariyaraftab/Data/main/data.csv"))
#head(df)
setwd("C:/Users/mrb/Desktop/Data")
DatM_name <- "datama.csv"
DatQ_name <- "dataqa.csv"
headers <- read.csv(DatM_name, header = F, nrows = 1, as.is = T,sep = ";", dec = ",")
Monthly <- read.csv(DatM_name, sep = ";", dec = ",",skip = 2, stringsAsFactors = FALSE)
colnames(Monthly) <- headers
headers <- read.csv(DatQ_name, header = F, nrows = 1, as.is = T,sep = ";", dec = ",")
Quarterly <- read.csv(DatQ_name, sep = ";", dec = ",",skip = 2, stringsAsFactors = FALSE)
colnames(Quarterly) <- headers
lag <- 12
t_max <- nrow(Monthly)
Time <- as.Date(Monthly$date, format = "%Y.%m.%d")[(lag+1):t_max]
Var1 <-  diff(log(Monthly$ipi), lag = lag)* 100 
#Var1 <-   (Monthly$ipi)[(lag+1):t_max]
Var2 <- diff(log(Monthly$CPI), lag = lag) * 100
#Var2 <-   (Monthly$CPI)[(lag+1):t_max]
Var3 <-   Monthly$trebil[(lag+1):t_max]

Var4 <-  diff(log(Monthly$Govtde), lag = lag) * 100

Monthly <- data.frame(Time = Time, 
                      IPI = Var1,
                      CPI=Var2,
                      trebil = Var3,
                      govtde = Var4)
# Remove spaces and convert GDP to numeric
GDP <- as.numeric(gsub(" ", "", Quarterly$GDP))
GDP_growth <- 100*diff(log(GDP),lag = 1) # Using 1 lag instead of 4 lags.
Time_q <- as.Date(Quarterly$Date, format = "%Y.%m.%d")[2:nrow(Quarterly)]
Quarterly <- data.frame(Time = Time_q, GDP_growth = GDP_growth)
Data <- merge(x = Monthly, y = Quarterly, by = "Time", all.x = T)
adj_lag <- 24-lag
y_all <- as.matrix(Data[adj_lag:nrow(Data),2:6])
y_all[1,5] <- Data[adj_lag-4,6]
y_all[2,5] <- Data[adj_lag-1,6]
y0 <- y_all[1:2,]
y <- y_all[3:nrow(y_all),]
p = 2
K = 5
t_max <- nrow(y)
vars::VARselect( na.omit(y))
prior <- get_prior(y, p = p, dist="Gaussian", SV = F, aggregation = "triangular", idq = c(5))
inits <- get_init(prior)
inits <- get_init(prior, samples = 60, burnin = 10, thin = 2)

Chain1 <- BMFVAR.novol(y, K = K, p = p, dist = "Gaussian", y0 = y0, prior = prior, inits = inits)
B <- matrix(apply(Chain1$mcmc$param, MARGIN = 2, FUN = median)[1:30], nrow = 5)
A = diag(1, K, K)
A[upper.tri(A, diag=FALSE)] <- apply(Chain1$mcmc$param, MARGIN = 2, FUN = median)[31:40]
A <- t(A)
A_inv <- solve(A)
A_inv %*% t(A_inv)
