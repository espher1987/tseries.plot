# tseries.plot

Time Series plot

# Description

Package to create basics time series plots using ggplot2.

# Install

    library(devtools)
    install_github("espher1987/tseries.plot")

# Author

Víctor José Espinoza Hernández


# Not real Example, only for testing.

``` r
library(forecast)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
model <- Arima(AirPassengers,c(1,1,1))
summary(model)
#> Series: AirPassengers 
#> ARIMA(1,1,1) 
#> 
#> Coefficients:
#>           ar1     ma1
#>       -0.4741  0.8634
#> s.e.   0.1159  0.0720
#> 
#> sigma^2 = 975.8:  log likelihood = -694.34
#> AIC=1394.68   AICc=1394.86   BIC=1403.57
#> 
#> Training set error measures:
#>                  ME     RMSE      MAE       MPE     MAPE      MASE       ACF1
#> Training set 1.9209 30.91125 24.12176 0.4150742 8.566115 0.7530918 0.03749257
autoplot(model)
```

![](https://i.imgur.com/4eTg7ms.png)

``` r
autoplot(forecast(model))
```

![](https://i.imgur.com/5B46KqM.png)

<sup>Created on 2022-02-05 by the [reprex package](https://reprex.tidyverse.org) (v1.0.0)</sup>
