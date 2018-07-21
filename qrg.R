


### QRG Factors and Benchmarking

## Install required libraries

library(data.table)
library(tidyr)
library(DataExplorer)
library(reshape2)
library(ggplot2)
library(PortfolioAnalytics)
library(corrplot)
library(knitr)
library(MASS)
library(dplyr)
library(caret)
library(leaps)

### Read data and format
bench <- fread("C:/Users/Gurpreet/Documents/IS621/QRG_factors_benchmarks.csv")


## reorder the columns
bench1<- bench[c(13,1:12),]
bench2 <- transpose(bench1)







bench2$Month <- colnames(bench1)
colnames(bench2) <- as.character(unlist(bench2[1,]))  ## assign first row as column names
bench2 <- bench2[-1,]
bench2 <- bench2[,c(14,1:13)]
names(bench2)[1]<- "Months/Factors"



### formatting structure of the variables to numeric

str(bench2)


bench2[,2:14]<-   data.frame(lapply(bench2[,2:14], function(x) as.numeric(as.character(x))))

str(bench2)

kable(head(bench2))

### check for missing values

miss_plot <- plot_missing(bench2)
miss_plot       ## no missing data


### Outliers
m1 <- melt(as.data.frame(bench2))
p <-ggplot(m1, aes(x=variable, y=value)) + geom_boxplot()
p + coord_flip()


### correlation
library(PerformanceAnalytics)
chart.Correlation(bench2[,2:14],cex.cor.scale=2)

corrplot(cor(bench2[,2:14]), method = "number",  number.digits = 2,number.cex = .8)
#
# Models
bench3 <- bench2[,c(2:14)]

library(MASS)
fit1 <- lm(`S&P1500` ~ ., bench3)
fit2 <- lm(`S&P1500` ~ 1, bench3)

##Backward Elimination
back <- stepAIC(fit1,direction="backward")
summary(back)



## Forward Selection
fwd <- stepAIC(fit2,direction="forward", scope=list(upper=fit1,lower=fit2))
summary(fwd)


full <- fit1
summary(full)

##leaps 
lps <- regsubsets(`S&P1500` ~  f1.o+f2.o+f3.o+f4.o+f5.o+f6.o+f7.o+f8.o+f9.o+f10.o+f11.o+f12.o,data= bench3, nvmax = 8)
lps_sum<-summary(lps)
lps_m<- lm(`S&P1500`~f2.o+f3.o+f5.o+f6.o+f7.o+f8.o+f10.o+f12.o,data= bench3,)
summary(lps_m)





##ridge 
library(ridge)
ridge <- linearRidge(`S&P1500`~ ., data=bench3)
summary(ridge)


AIC(back)
AIC(fwd)
AIC(full)
AIC(lps_m)
AIC(ridge$model)


BIC(back)
BIC(fwd)
BIC(full)
BIC(lps_m)

arsq_back <-summary(back)$adj.r.squared
arsq_fwd <-summary(fwd)$adj.r.squared
arsq_full <-summary(full)$adj.r.squared
arsq_lps <-summary(lps_m)$adj.r.squared


arsq <- data.frame(arsq_back,arsq_fwd,arsq_full,arsq_lps)

aic_df <- data.frame(AIC(back), AIC(fwd), AIC(full),AIC(lps_m))
kable(aic_df)
bic_df <- data.frame(BIC(back), BIC(fwd), BIC(full), BIC(lps_m))

(bic_df)

