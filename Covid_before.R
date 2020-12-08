---
title: "Untitled"
author: "Abimbola Olawale"
date: "9/5/2020"
output:
  word_document: default
  pdf_document: default
---

Bringing in the dataset of all the 5 countries before covid19

```{r}
require(rugarch)
require(tseries)
require(fBasics)
require(zoo)
require(lmtest) 
require(forecast)
library(quantmod)
require(rugarch)
require(tseries)
require(fBasics)
require(zoo)
require(lmtest) 
require(forecast)
library(quantmod)
```

```{r}
NIR <- c('NGE')
SA <- c('EZA')
EGPT <- c('EGY')
GAB <- c('EC.PA')
TAZ <- c('TRX')
```

```{r}
Data1 <- new.env()
```

```{r}
NGE_Before <-getSymbols(NIR,
             src="yahoo",
                   from="2019-10-01",
                   to="2020-02-29",
                   env=Data1)
```

```{r}
`NIG Stock Before COVID-19` <- Data1$NGE
class(`NIG Stock Before COVID-19`)
```



```{r}
## SouthAfrica
Data1 <- new.env()

SA_Before <- getSymbols(SA,
                   src="yahoo",
                   from="2019-10-01",
                   to="2020-02-29",
                   env=Data1)
`SA Stock Before COVID-19` <- Data1$EZA
class(`SA Stock Before COVID-19`)

```

```{r}
### Egypt
Data2 <- new.env()

EGPT_Before <-getSymbols(EGPT,
                   src="yahoo",
                   from="2019-10-01",
                   to="2020-02-29",
                   env=Data2)
`EGPT Stock Before COVID-19` <- Data2$EGY
class(`EGPT Stock Before COVID-19`)

```

```{r}
### Tanzania 
Data4 <- new.env()

TAZ_Before <-getSymbols(TAZ,
                   src="yahoo",
                   from="2019-10-01",
                   to="2020-02-29",
                   env=Data4)
`TAZ Stock Before COVID-19` <- Data4$TRX
class(`TAZ Stock Before COVID-19`)

```

```{r}
### Gabon
Data3 <- new.env()

GAB_Before <-getSymbols(GAB,
                   src="yahoo",
                   from="2019-10-01",
                   to="2020-02-29",
                   env=Data3)
`GAB Stock Before COVID-19` <- Data3$EC.PA
na.omit(`GAB Stock Before COVID-19`)
class(`GAB Stock Before COVID-19`)

```


```{r}
Pricee = as.xts(`NIG Stock Before COVID-19`$NGE.Adjusted)
Price =log10(Pricee)
names(Price) = c("price")
`Nigeria Stock Returns Before COVID-19` = dailyReturn(Price)
```

```{r}
Price1 = as.xts(`SA Stock Before COVID-19`$EZA.Adjusted)
names(Price1) = c("price")
`South Africa Stock Returns Before COVID-19` = dailyReturn(Price1)
```

```{r}
Price2 <- as.xts(`GAB Stock Before COVID-19`$EC.PA.Adjusted)
names(Price2) <- c("price")
`Gabon Stock Returns Before COVID-19` = dailyReturn(Price2)
```


```{r}
Price4 = as.xts(`TAZ Stock Before COVID-19`$TRX.Adjusted)
```

```{r}
names(Price4) = c("price")
`Tanzania Stock Returns Before COVID-19` = dailyReturn(Price4)
```

```{r}
Price5 <- as.xts(`EGPT Stock Before COVID-19`$EGY.Adjusted)
names(Price5) <- c("price")
`Egypt Stock Returns Before COVID-19` = dailyReturn(Price5)

```


```{r}
library(moments)
skewness(`Gabon Stock Returns Before COVID-19`)
kurtosis(`Gabon Stock Returns Before COVID-19`)
```

### Discriptive Statistics 
```{r}
plot(`Nigeria Stock Returns Before COVID-19`)
plot(`South Africa Stock Returns Before COVID-19`)
plot(`Gabon Stock Returns Before COVID-19`)
plot(`Tanzania Stock Returns Before COVID-19`)
plot(`Egypt Stock Returns Before COVID-19`)
```
```{r}
library(pastecs)
options(scipen=100)
options(digits=2)
stat.desc(`Gabon Stock Returns Before COVID-19`)
```
```{r}
adf.test(`Nigeria Stock Returns Before COVID-19`)
adf.test(`South Africa Stock Returns Before COVID-19`)
adf.test(`Gabon Stock Returns Before COVID-19`)
adf.test(`Tanzania Stock Returns Before COVID-19`)
adf.test(`Egypt Stock Returns Before COVID-19`)
```


```{r}
Nig_be <- diff(`Nigeria Stock Returns Before COVID-19`, differences = 1)
SA_be<-diff(`South Africa Stock Returns Before COVID-19`, differences = 1)
Gab_be<-diff(`Gabon Stock Returns Before COVID-19`, differences = 1)
Tan_be<-diff(`Tanzania Stock Returns Before COVID-19`, differences = 1)
Egy_be<-diff(`Egypt Stock Returns Before COVID-19`, differences = 1)
library(Hmisc)
#Before
Nig_bef <- impute(Nig_be, mean)
Sa_bef <- impute(SA_be, mean)
Gab_bef <- impute(Gab_be, mean)
Tan_bef <- impute(Tan_be, mean)
Egy_bef <- impute(Egy_be, mean)
```



```{r}
#Now try an Exponential GARCH (EGARCH) model
library(rugarch)
spec_egarch_11 = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model= list(garchOrder = c(1,1), model="eGARCH"))
fit_egarch_11 = ugarchfit(spec_egarch_11, Nig_bef)
fit_egarch_11
```

```{r}
ni.egarch11 <- newsimpact(fit_egarch_11)
plot(ni.egarch11$zx, ni.egarch11$zy, type="l", lwd=2, col="blue", main="EGARCH(1,1) - News Impact",
ylab=ni.egarch11$yexpr, xlab=ni.egarch11$xexpr)

```


```{r}
spec           <- getspec(fit_egarch_11)
setfixed(spec) <- as.list(coef(fit_egarch_11))
garchforecast1 <- ugarchforecast(spec, n.ahead = 100, n.roll = 5, data = Nig_bef, out.sample = 30)

a <- plot(garchforecast1, which = "all")
```

### South AfricA


```{r}
spec_egarch_12 = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model= list(garchOrder = c(1,1), model="eGARCH"))
fit_egarch_12 = ugarchfit(spec_egarch_12, Sa_bef)
fit_egarch_12
```

```{r}
#Sa.egarch12 <- newsimpact(fit_egarch_12)
```

```{r}
#plot(Sa.egarch12$zx, Sa.egarch12$zy, type="l", lwd=2, col="blue", main="EGARCH(1,1) - News Impact", ylab=Sa.egarch11$yexpr,xlab=Sa.egarch12$xexpr)
```


```{r}
spec           <- getspec(fit_egarch_12)
setfixed(spec) <- as.list(coef(fit_egarch_12))
garchforecast2 <- ugarchforecast(spec, n.ahead = 100, n.roll = 5, data = `South Africa Stock Returns Before COVID-19`, out.sample = 30)

a <- plot(garchforecast2, which = "all")
```
## Gabon
```{r}
spec_egarch_13 = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model= list(garchOrder = c(1,1), model="eGARCH"))
fit_egarch_13 = ugarchfit(spec_egarch_13, Gab_bef)
fit_egarch_13
```

```{r}
Ga.egarch13 <- newsimpact(fit_egarch_13)
plot(Ga.egarch13$zx, Ga.egarch13$zy, type="l", lwd=2, col="blue", main="EGARCH(1,1) - News Impact",
ylab=Ga.egarch13$yexpr, xlab=Ga.egarch13$xexpr)
```

```{r}
spec           <- getspec(fit_egarch_13)
setfixed(spec) <- as.list(coef(fit_egarch_13))
garchforecast3 <- ugarchforecast(spec, n.ahead = 100, n.roll = 10, data = Gab_bef, out.sample = 30)
a <- plot(garchforecast3, which = "all")
```
## Tanzania 

```{r}
spec_egarch_14 = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model= list(garchOrder = c(1,1), model="eGARCH"))
fit_egarch_14 = ugarchfit(spec_egarch_14, Tan_bef)
fit_egarch_14
```

```{r}
Ta.egarch14 <- newsimpact(fit_egarch_14)
plot(Ta.egarch14$zx, Ta.egarch14$zy, type="l", lwd=2, col="blue", main="EGARCH(1,1) - News Impact",
ylab=Ta.egarch14$yexpr, xlab=Ta.egarch14$xexpr)
```

```{r}
spec           <- getspec(fit_egarch_14)
setfixed(spec) <- as.list(coef(fit_egarch_14))
garchforecast4 <- ugarchforecast(spec, n.ahead = 100, n.roll = 5, data = Tan_bef , out.sample = 30)

a <- plot(garchforecast2, which = "all")
```

## Egypt 

```{r}
spec_egarch_15 = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model= list(garchOrder = c(1,1), model="eGARCH"))
fit_egarch_15 = ugarchfit(spec_egarch_15, Egy_bef)
fit_egarch_15
```

```{r}
Eg.egarch15 <- newsimpact(fit_egarch_15)
plot(Eg.egarch15$zx, Eg.egarch15$zy, type="l", lwd=2, col="blue", main="EGARCH(1,1) - News Impact",
ylab=Eg.egarch15$yexpr, xlab=Eg.egarch15$xexpr)
```

```{r}
spec           <- getspec(fit_egarch_15)
setfixed(spec) <- as.list(coef(fit_egarch_15))
garchforecast5 <- ugarchforecast(spec, n.ahead = 100, n.roll = 5, data = Egy_bef , out.sample = 30)

a <- plot(garchforecast5, which = "all")
```
