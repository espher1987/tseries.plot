# tseries.plot

Time Series plot

# Description

Package to create basics time series plots using ggplot2.

# Install

    library(devtools)
    install_github("espher1987/tseries.plot")

# Author

Víctor José Espinoza Hernández

# Example

``` r
library(tseries.plot)
#> Loading required package: magrittr
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
#> Warning in .recacheSubclasses(def@className, def, env): undefined subclass "xts"
#> of class "xtsORzoo"; definition not updated

# paquetería extra a utilizar

library(forecast)
library(strucchange)
#> Loading required package: zoo
#> 
#> Attaching package: 'zoo'
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric
#> Loading required package: sandwich
library(lmtest)
library(broom)
library(knitr)

# Gráfica de la serie original

data("JohnsonJohnson")
x <- JohnsonJohnson

# análisis de la tendencia y el componente estacional

autoplot(x)
```

![](https://i.imgur.com/dfx6cm4.png)

``` r

## Tendencia
tsp.trend.decompose(x)
```

![](https://i.imgur.com/OD3lPS3.png)

``` r
tsp.trend.filter(x,table = F)
```

![](https://i.imgur.com/WRDxEZj.png)

``` r
tsp.year(x,table = F)
```

![](https://i.imgur.com/AStk1Ap.png)

``` r

# Componente Estacional
tsp.figures(x,table = F)
```

![](https://i.imgur.com/WqYyjES.png)

``` r
tsp.range_mean(x)
```

![](https://i.imgur.com/VlCeD2K.png)

    #>    year    mean range
    #> 1  1960  0.6575  0.41
    #> 2  1961  0.6925  0.37
    #> 3  1962  0.7525  0.32
    #> 4  1963  0.8500  0.23
    #> 5  1964  1.0400  0.32
    #> 6  1965  1.2900  0.29
    #> 7  1966  1.5150  0.60
    #> 8  1967  1.7025  0.33
    #> 9  1968  2.0475  0.81
    #> 10 1969  2.3850  0.54
    #> 11 1970  3.3750  0.90
    #> 12 1971  4.0725  0.72
    #> 13 1972  4.8375  0.63
    #> 14 1973  5.8275  1.26
    #> 15 1974  6.3000  1.08
    #> 16 1975  7.1550  1.71
    #> 17 1976  7.9425  2.07
    #> 18 1977  9.5175  1.53
    #> 19 1978 11.2500  3.24
    #> 20 1979 12.9600  4.86
    #> 21 1980 14.6250  4.59
    tsp.season.line(x)
    #> N : 84   start : 1960   end:  1980   frequency : 4

![](https://i.imgur.com/6B2cuaV.png)

``` r
tsp.season.box(x)
#>   season median     mean       sd      var
#> 1      1   2.79 4.791429 4.714860 22.22990
#> 2      2   3.42 4.965714 4.503254 20.27930
#> 3      3   3.69 5.253810 4.698462 22.07554
#> 4      4   3.60 4.188095 3.444917 11.86746
```

![](https://i.imgur.com/utVm5DB.png)

``` r


# Factor variable plot with time series

tsp.series_factor(AirPassengers,
                  factor(rbinom(length(AirPassengers),1,0.2)),
                  color = F)
```

![](https://i.imgur.com/P3Bh3s3.png)

``` r

library(mFilter)
tsp.mfilter(hpfilter(x))
```

![](https://i.imgur.com/cSbR5CX.png)

``` r
library(forecast)
tsp.correlogram(auto.arima(AirPassengers))
#>  lag      acf     pacf   q.stat df p.value
#>    1 -0.00125 -0.00125       NA NA      NA
#>    2  0.03774  0.03774       NA NA      NA
#>    3 -0.07651 -0.07653       NA NA      NA
#>    4 -0.09640 -0.09851  2.47957  1 0.11533
#>    5  0.05423  0.06062  2.92443  2 0.23172
#>    6  0.01194  0.01440  2.94614  3 0.40000
#>    7 -0.07023 -0.09202  3.70300  4 0.44769
#>    8 -0.03165 -0.03443  3.85788  5 0.57005
#>    9  0.14854  0.17509  7.29406  6 0.29451
#>   10 -0.09425 -0.11162  8.68783  7 0.27585
#>   11 -0.00436 -0.04827  8.69083  8 0.36904
#>   12 -0.11970 -0.08031 10.97288  9 0.27757
#>   13  0.04900  0.08616 11.35829 10 0.33029
#>   14  0.02771 -0.01903 11.48248 11 0.40377
#>   15  0.02465 -0.00430 11.58155 12 0.47984
#>   16 -0.12592 -0.11610 14.18593 13 0.36088
#>   17 -0.04205 -0.01283 14.47865 14 0.41469
#>   18 -0.00493 -0.03080 14.48270 15 0.48928
#>   19 -0.16482 -0.18615 19.05190 16 0.26598
#>   20 -0.14309 -0.19889 22.52360 17 0.16541
#>   21 -0.03820  0.02745 22.77296 18 0.19947
#>   22 -0.10696 -0.17583 24.74437 19 0.16913
#>   23  0.25019  0.19176 35.62031 20 0.01704
#>   24  0.11113  0.08464 37.78405 21 0.01366
```

![](https://i.imgur.com/hzkggvZ.png)

``` r

# Detectando Cambios estructurales

library(strucchange)
tsp.break(breakpoints(x~1))
```

![](https://i.imgur.com/Ws5P1JP.png)

``` r
## Using vars package
library(vars)
#> Loading required package: MASS
#> Loading required package: urca
data("Canada")
tsp.break.coef(Canada) %>% 
  kable(digits = 2)
#> Error in full_join(., observed): no se pudo encontrar la función "full_join"

model <- VAR(Canada)
tsp.var.fit(model)
```

![](https://i.imgur.com/FVWkmyw.png)

``` r
tsp.var.resid(model)
```

![](https://i.imgur.com/EvCeOVU.png)

``` r
tsp.var.forecast(model)
```

![](https://i.imgur.com/bzLqXqZ.png)

``` r
tsp.var.irf(irf(model))
```

![](https://i.imgur.com/CYZB35o.png)

``` r
tsp.var.fevd(fevd(model))
```

![](https://i.imgur.com/OuaYQ00.png)

``` r

## Causality Test
# For stationary variables
tsp.granger(VAR(Canada)) %>% 
  kable(digits = 2)
```

| caused | sign | cause | Chisq |  df | p_value | H0        |
|:-------|:-----|:------|------:|----:|--------:|:----------|
| e      | \<-  | prod  | 27.90 |   1 |    0.00 | prod.l1=0 |
| e      | \<-  | rw    |  7.79 |   1 |    0.01 | rw.l1=0   |
| e      | \<-  | U     | 10.01 |   1 |    0.00 | U.l1=0    |
| prod   | \<-  | e     |  0.54 |   1 |    0.46 | e.l1=0    |
| prod   | \<-  | rw    |  0.41 |   1 |    0.52 | rw.l1=0   |
| prod   | \<-  | U     |  1.82 |   1 |    0.18 | U.l1=0    |
| rw     | \<-  | e     |  0.22 |   1 |    0.64 | e.l1=0    |
| rw     | \<-  | prod  |  9.03 |   1 |    0.00 | prod.l1=0 |
| rw     | \<-  | U     |  0.51 |   1 |    0.47 | U.l1=0    |
| U      | \<-  | e     |  9.91 |   1 |    0.00 | e.l1=0    |
| U      | \<-  | prod  | 15.56 |   1 |    0.00 | prod.l1=0 |
| U      | \<-  | rw    | 12.67 |   1 |    0.00 | rw.l1=0   |

``` r
# For non stationary variables
test <- tsp.toda_yamamoto(Canada)
#> [1] "Automatic selection for 'd' using adf test:d_max= 1"
#> [1] "Automatic selection for 'p' using AIC(n) criteria:p= 3"
test$result %>% 
  kable(digits = 2)
```

| caused | sign | cause | chisq |  df |    p | H0                            |
|:-------|:-----|:------|------:|----:|-----:|:------------------------------|
| e      | \<-  | prod  | 10.66 |   3 | 0.01 | prod.l1=0,prod.l2=0,prod.l3=0 |
| e      | \<-  | rw    |  7.55 |   3 | 0.06 | rw.l1=0,rw.l2=0,rw.l3=0       |
| e      | \<-  | U     |  2.41 |   3 | 0.49 | U.l1=0,U.l2=0,U.l3=0          |
| prod   | \<-  | e     |  1.63 |   3 | 0.65 | e.l1=0,e.l2=0,e.l3=0          |
| prod   | \<-  | rw    |  4.41 |   3 | 0.22 | rw.l1=0,rw.l2=0,rw.l3=0       |
| prod   | \<-  | U     |  7.50 |   3 | 0.06 | U.l1=0,U.l2=0,U.l3=0          |
| rw     | \<-  | e     |  1.98 |   3 | 0.58 | e.l1=0,e.l2=0,e.l3=0          |
| rw     | \<-  | prod  |  4.29 |   3 | 0.23 | prod.l1=0,prod.l2=0,prod.l3=0 |
| rw     | \<-  | U     |  1.86 |   3 | 0.60 | U.l1=0,U.l2=0,U.l3=0          |
| U      | \<-  | e     | 26.02 |   3 | 0.00 | e.l1=0,e.l2=0,e.l3=0          |
| U      | \<-  | prod  |  5.20 |   3 | 0.16 | prod.l1=0,prod.l2=0,prod.l3=0 |
| U      | \<-  | rw    |  2.68 |   3 | 0.44 | rw.l1=0,rw.l2=0,rw.l3=0       |

<sup>Created on 2022-10-31 with [reprex v2.0.2.9000](https://reprex.tidyverse.org)</sup>
