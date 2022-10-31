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
plot(x)
```
![](https://i.imgur.com/yBLofXt.png)

```
# Ajuste de tendencia lineal

model <- tslm(x~1+trend)
model %>% 
  tidy() %>% 
  kable(digits = 2)
#> Warning: The `tidy()` method for objects of class `tslm` is not maintained by the broom team, and is only supported through the `lm` tidier method. Please be cautious in interpreting and reporting broom output.
#> 
#> This warning is displayed once per session.
```

| term        | estimate | std.error | statistic | p.value |
|:------------|---------:|----------:|----------:|--------:|
| (Intercept) |    -2.13 |      0.37 |     -5.79 |       0 |
| trend       |     0.16 |      0.01 |     21.69 |       0 |

``` r

plot(x)
lines(fitted(model))
```

![](https://i.imgur.com/r6SrUq0.png)

``` r

# Detección de cambio estructural sobre una constant


bk <- breakpoints(x~1)
plot(x)
lines(fitted(bk))
lines(confint(bk))
```

![](https://i.imgur.com/HxEqH4F.png)

``` r
coeftest(bk) %>% 
  tidy() %>% 
  kable(digits = 2)
```

| term                          | estimate | std.error | statistic | p.value |
|:------------------------------|---------:|----------:|----------:|--------:|
| 1960(1) - 1968(1) (Intercept) |     1.08 |      0.07 |     15.45 |       0 |
| 1968(2) - 1971(1) (Intercept) |     2.78 |      0.17 |     15.96 |       0 |
| 1971(2) - 1974(4) (Intercept) |     5.37 |      0.22 |     24.06 |       0 |
| 1975(1) - 1977(4) (Intercept) |     8.20 |      0.34 |     23.90 |       0 |
| 1978(1) - 1980(4) (Intercept) |    12.95 |      0.63 |     20.54 |       0 |

``` r


# Detección de cambio estructural con constante y tendencia determinista
trend <- seq_along(x)
bk_trend <- breakpoints(x~1+trend)
coeftest(bk_trend) %>% 
  tidy() %>% 
  kable(digits = 2)
```

| term                          | estimate | std.error | statistic | p.value |
|:------------------------------|---------:|----------:|----------:|--------:|
| 1960(1) - 1968(3) (Intercept) |     0.37 |      0.07 |      5.74 |    0.00 |
| 1960(1) - 1968(3) trend       |     0.04 |      0.00 |     13.49 |    0.00 |
| 1968(4) - 1977(4) (Intercept) |    -5.36 |      0.51 |    -10.58 |    0.00 |
| 1968(4) - 1977(4) trend       |     0.20 |      0.01 |     22.10 |    0.00 |
| 1978(1) - 1980(4) (Intercept) |    -8.05 |     13.00 |     -0.62 |    0.55 |
| 1978(1) - 1980(4) trend       |     0.27 |      0.17 |      1.62 |    0.14 |

``` r

plot(x)
lines(fitted(bk_trend))
lines(confint(bk_trend))
```

![](https://i.imgur.com/c8Jhwl6.png)

``` r
coeftest(bk_trend)%>% 
  tidy() %>% 
  kable(digits = 2)
```

| term                          | estimate | std.error | statistic | p.value |
|:------------------------------|---------:|----------:|----------:|--------:|
| 1960(1) - 1968(3) (Intercept) |     0.37 |      0.07 |      5.74 |    0.00 |
| 1960(1) - 1968(3) trend       |     0.04 |      0.00 |     13.49 |    0.00 |
| 1968(4) - 1977(4) (Intercept) |    -5.36 |      0.51 |    -10.58 |    0.00 |
| 1968(4) - 1977(4) trend       |     0.20 |      0.01 |     22.10 |    0.00 |
| 1978(1) - 1980(4) (Intercept) |    -8.05 |     13.00 |     -0.62 |    0.55 |
| 1978(1) - 1980(4) trend       |     0.27 |      0.17 |      1.62 |    0.14 |

``` r


tsp.figures(x,table = F)
```

![](https://i.imgur.com/bEqzeHf.png)

``` r
tsp.range_mean(x)
```

![](https://i.imgur.com/VB4wIwJ.png)

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

![](https://i.imgur.com/fbTdUYi.png)

``` r
tsp.season.box(x)
#>   season median     mean       sd      var
#> 1      1   2.79 4.791429 4.714860 22.22990
#> 2      2   3.42 4.965714 4.503254 20.27930
#> 3      3   3.69 5.253810 4.698462 22.07554
#> 4      4   3.60 4.188095 3.444917 11.86746
```

![](https://i.imgur.com/dmvgVkc.png)

``` r
tsp.trend.decompose(x)
```

![](https://i.imgur.com/dfXYAWX.png)

``` r
tsp.trend.filter(x)
#> 
#> Title:
#>  Hodrick-Prescott Filter 
#> 
#> Call:
#>  hpfilter(x = x, freq = ag$freq, type = ag$type, drift = ag$drift)
#> 
#> Method:
#>  hpfilter
#> 
#> Filter Type:
#>  lambda
#> 
#> Series:
#>  x
#> 
#>             x   Trend      Cycle
#> 1960 Q1  0.71  0.5773  1.327e-01
#> 1960 Q2  0.63  0.5950  3.503e-02
#> 1960 Q3  0.85  0.6127  2.373e-01
#> 1960 Q4  0.44  0.6306 -1.906e-01
#> 1961 Q1  0.61  0.6490 -3.896e-02
#> 1961 Q2  0.69  0.6679  2.211e-02
#> 1961 Q3  0.92  0.6875  2.325e-01
#> 1961 Q4  0.55  0.7079 -1.579e-01
#> 1962 Q1  0.72  0.7294 -9.430e-03
#> 1962 Q2  0.77  0.7522  1.782e-02
#> 1962 Q3  0.92  0.7763  1.437e-01
#> 1962 Q4  0.60  0.8021 -2.021e-01
#> 1963 Q1  0.83  0.8297  3.286e-04
#> 1963 Q2  0.80  0.8593 -5.926e-02
#> 1963 Q3  1.00  0.8910  1.090e-01
#> 1963 Q4  0.77  0.9249 -1.549e-01
#> 1964 Q1  0.92  0.9613 -4.126e-02
#> 1964 Q2  1.00  1.0001 -8.492e-05
#> 1964 Q3  1.24  1.0414  1.986e-01
#> 1964 Q4  1.00  1.0853 -8.534e-02
#> 1965 Q1  1.16  1.1320  2.800e-02
#> 1965 Q2  1.30  1.1815  1.185e-01
#> 1965 Q3  1.45  1.2340  2.160e-01
#> 1965 Q4  1.25  1.2897 -3.973e-02
#> 1966 Q1  1.26  1.3490 -8.900e-02
#> 1966 Q2  1.38  1.4121 -3.213e-02
#> 1966 Q3  1.86  1.4794  3.806e-01
#> 1966 Q4  1.56  1.5510  8.957e-03
#> 1967 Q1  1.53  1.6275 -9.755e-02
#> 1967 Q2  1.59  1.7094 -1.194e-01
#> 1967 Q3  1.83  1.7970  3.297e-02
#> 1967 Q4  1.86  1.8908 -3.079e-02
#> 1968 Q1  1.53  1.9910 -4.610e-01
#> 1968 Q2  2.07  2.0981 -2.815e-02
#> 1968 Q3  2.34  2.2122  1.278e-01
#> 1968 Q4  2.25  2.3332 -8.317e-02
#> 1969 Q1  2.16  2.4613 -3.013e-01
#> 1969 Q2  2.43  2.5965 -1.665e-01
#> 1969 Q3  2.70  2.7388 -3.882e-02
#> 1969 Q4  2.25  2.8880 -6.380e-01
#> 1970 Q1  2.79  3.0437 -2.537e-01
#> 1970 Q2  3.42  3.2054  2.146e-01
#> 1970 Q3  3.69  3.3723  3.177e-01
#> 1970 Q4  3.60  3.5437  5.633e-02
#> 1971 Q1  3.60  3.7191 -1.191e-01
#> 1971 Q2  4.32  3.8981  4.219e-01
#> 1971 Q3  4.32  4.0802  2.398e-01
#> 1971 Q4  4.05  4.2651 -2.151e-01
#> 1972 Q1  4.86  4.4528  4.072e-01
#> 1972 Q2  5.04  4.6431  3.969e-01
#> 1972 Q3  5.04  4.8359  2.041e-01
#> 1972 Q4  4.41  5.0316 -6.216e-01
#> 1973 Q1  5.58  5.2305  3.495e-01
#> 1973 Q2  5.85  5.4327  4.173e-01
#> 1973 Q3  6.57  5.6383  9.317e-01
#> 1973 Q4  5.31  5.8480 -5.380e-01
#> 1974 Q1  6.03  6.0628 -3.275e-02
#> 1974 Q2  6.39  6.2833  1.067e-01
#> 1974 Q3  6.93  6.5104  4.196e-01
#> 1974 Q4  5.85  6.7448 -8.948e-01
#> 1975 Q1  6.93  6.9876 -5.758e-02
#> 1975 Q2  7.74  7.2392  5.008e-01
#> 1975 Q3  7.83  7.5002  3.298e-01
#> 1975 Q4  6.12  7.7712 -1.651e+00
#> 1976 Q1  7.74  8.0533 -3.133e-01
#> 1976 Q2  8.91  8.3464  5.636e-01
#> 1976 Q3  8.28  8.6502 -3.702e-01
#> 1976 Q4  6.84  8.9649 -2.125e+00
#> 1977 Q1  9.54  9.2903  2.497e-01
#> 1977 Q2 10.26  9.6249  6.351e-01
#> 1977 Q3  9.54  9.9675 -4.275e-01
#> 1977 Q4  8.73 10.3171 -1.587e+00
#> 1978 Q1 11.88 10.6726  1.207e+00
#> 1978 Q2 12.06 11.0317  1.028e+00
#> 1978 Q3 12.15 11.3932  7.568e-01
#> 1978 Q4  8.91 11.7561 -2.846e+00
#> 1979 Q1 14.04 12.1203  1.920e+00
#> 1979 Q2 12.96 12.4835  4.765e-01
#> 1979 Q3 14.85 12.8451  2.005e+00
#> 1979 Q4  9.99 13.2043 -3.214e+00
#> 1980 Q1 16.20 13.5619  2.638e+00
#> 1980 Q2 14.67 13.9165  7.535e-01
#> 1980 Q3 16.02 14.2684  1.752e+00
#> 1980 Q4 11.61 14.6184 -3.008e+00
```

![](https://i.imgur.com/kvp0ks9.png)

``` r
tsp.year(x)
#>    year median    mean        sd        var
#> 1  1960  0.670  0.6575 0.1711481 0.02929167
#> 2  1961  0.650  0.6925 0.1621471 0.02629167
#> 3  1962  0.745  0.7525 0.1325079 0.01755833
#> 4  1963  0.815  0.8500 0.1029563 0.01060000
#> 5  1964  1.000  1.0400 0.1385641 0.01920000
#> 6  1965  1.275  1.2900 0.1213809 0.01473333
#> 7  1966  1.470  1.5150 0.2609598 0.06810000
#> 8  1967  1.710  1.7025 0.1668083 0.02782500
#> 9  1968  2.160  2.0475 0.3628016 0.13162500
#> 10 1969  2.340  2.3850 0.2381176 0.05670000
#> 11 1970  3.510  3.3750 0.4058325 0.16470000
#> 12 1971  4.185  4.0725 0.3397425 0.11542500
#> 13 1972  4.950  4.8375 0.2973634 0.08842500
#> 14 1973  5.715  5.8275 0.5418718 0.29362500
#> 15 1974  6.210  6.3000 0.4762352 0.22680000
#> 16 1975  7.335  7.1550 0.7999375 0.63990000
#> 17 1976  8.010  7.9425 0.8768267 0.76882500
#> 18 1977  9.540  9.5175 0.6251600 0.39082500
#> 19 1978 11.970 11.2500 1.5640332 2.44620000
#> 20 1979 13.500 12.9600 2.1259821 4.51980000
#> 21 1980 15.345 14.6250 2.1228047 4.50630000
```

![](https://i.imgur.com/MXXAp0f.png)

``` r

# Factor variable plot with time series

tsp.series_factor(AirPassengers,
                  factor(rbinom(length(AirPassengers),1,0.2)),
                  color = F)
```

![](https://i.imgur.com/D5SvAhV.png)

``` r

library(mFilter)
tsp.mfilter(hpfilter(x))
```

![](https://i.imgur.com/TTpl917.png)

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

![](https://i.imgur.com/bzu2wlV.png)

``` r

# Detectando Cambios estructurales

library(strucchange)
tsp.break(breakpoints(x~1))
```

![](https://i.imgur.com/55nVcvE.png)

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

![](https://i.imgur.com/rW83HDW.png)

``` r
tsp.var.resid(model)
```

![](https://i.imgur.com/P5gMlf5.png)

``` r
tsp.var.forecast(model)
```

![](https://i.imgur.com/caxg4xQ.png)

``` r
tsp.var.irf(irf(model))
```

![](https://i.imgur.com/cJnnn6I.png)

``` r
tsp.var.fevd(fevd(model))
```

![](https://i.imgur.com/RFT8lM1.png)

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
