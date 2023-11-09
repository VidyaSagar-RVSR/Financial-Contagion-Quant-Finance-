common_df <- read.csv("/Users/vidyasagar/Downloads/common_data.csv")
sensex<-common_df[,c("SR.RETURNS")]
gold <-common_df[,c("GP.RETURNS")]
bond <-common_df[,c("Broad.TRI.RETURNS")]
exchange <-common_df[,c("ER.RETURNS")]
time_index <- common_df[,c("Date")]
#DCC-MODEL
model1 <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                     variance.model = list(garchOrder = c(1, 1), model = "sGARCH"),
                     distribution.model = "norm")
modelspec <- dccspec(uspec = multispec(replicate(4, model1)), dccOrder = c(1, 1), distribution = "mvnorm")

modelfit=dccfit(modelspec,data=data.frame(bond,exchange,gold,sensex))
modelfit

#finding dynamic conditonal coorelation
coorelation=rcor(modelfit)
dim(coorelation)

#conditional coorelation on last day
coorelation[,,dim(coorelation)[3]]

dcc_BI_sensex=coorelation[1,4,]
dcc_ER_sensex=coorelation[2,4,]
dcc_GP_sensex=coorelation[3,4,]

#plotting graph
par(mfrow=c(4,1)) #codeline to get all plots in single window

plot.ts(dcc_BI_sensex)
plot.ts(dcc_ER_sensex)
plot.ts(dcc_GP_sensex)


#dynamic conditional covariance
covariance = rcov(modelfit)
covariance[,,dim(covariance)[3]]

dccov_BI_sensex=covariance[1,4,]
dccov_ER_sensex=covariance[2,4,]
dccov_GP_sensex=covariance[3,4,]

#plotting graph
par(mfrow=c(4,1)) #codeline to get all plots in single window

plot.ts(dccov_BI_sensex)
plot.ts(dccov_ER_sensex)
plot.ts(dccov_GP_sensex)







