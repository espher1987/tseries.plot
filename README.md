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

![](https://i.imgur.com/TxaAKeS.png)

``` r

# Detección de cambio estructural sobre una constant


bk <- breakpoints(x~1)
plot(x)
lines(fitted(bk))
lines(confint(bk))
```

![](https://i.imgur.com/FsS1ZkY.png)

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

![](https://i.imgur.com/eTC1t4h.png)

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

![](https://i.imgur.com/IEGIzUu.png)

``` r
tsp.range_mean(x)
```

![](https://i.imgur.com/oVVyzpj.png)

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

![](https://i.imgur.com/9z1a9AP.png)

``` r
tsp.season.box(x)
#>   season median     mean       sd      var
#> 1      1   2.79 4.791429 4.714860 22.22990
#> 2      2   3.42 4.965714 4.503254 20.27930
#> 3      3   3.69 5.253810 4.698462 22.07554
#> 4      4   3.60 4.188095 3.444917 11.86746
```

![](https://i.imgur.com/ADyTZyR.png)

``` r
tsp.trend.decompose(x)
```

![](https://i.imgur.com/GbD3oBV.png)

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

![](https://i.imgur.com/fTYlQLB.png)

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

![](https://i.imgur.com/ktcPmIE.png)

``` r

# Factor variable plot with time series

tsp.series_factor(AirPassengers,
                  factor(rbinom(length(AirPassengers),1,0.2)),
                  color = F)
```

![](https://i.imgur.com/hiVGCj0.png)

``` r

library(mFilter)
tsp.mfilter(hpfilter(x))
```

![](https://i.imgur.com/aAXuUuP.png)

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

![](https://i.imgur.com/QqVWgce.png)

``` r

# Detectando Cambios estructurales

library(strucchange)
tsp.break(breakpoints(x~1))
```

![](https://i.imgur.com/UZYxLTp.png)

``` r
## Using vars package
library(vars)
#> Loading required package: MASS
#> Loading required package: urca
data("Canada")
tsp.break.coef(Canada)
```

![](https://i.imgur.com/rYOtxdv.png)

    #> # A tibble: 38 × 8
    #> # Groups:   variable [4]
    #>    variable term        begin   end     estimate std.error statistic  p.value
    #>    <chr>    <chr>       <chr>   <chr>      <dbl>     <dbl>     <dbl>    <dbl>
    #>  1 e        (Intercept) 1980(1) 1982(4) 931.       1.04      896.    7.41e-26
    #>  2 e        trend       1980(1) 1982(4)  -0.0383   0.141      -0.271 7.92e- 1
    #>  3 e        (Intercept) 1983(1) 1989(3) 920.       0.262    3518.    1.04e-72
    #>  4 e        trend       1983(1) 1989(3)   0.708    0.00964    73.4   1.00e-30
    #>  5 e        (Intercept) 1989(4) 1992(3) 960.       1.55      618.    3.05e-24
    #>  6 e        trend       1989(4) 1992(3)  -0.294    0.0341     -8.62  6.09e- 6
    #>  7 e        (Intercept) 1992(4) 1996(2) 923.       1.43      643.    1.16e-30
    #>  8 e        trend       1992(4) 1996(2)   0.427    0.0242     17.6   1.89e-10
    #>  9 e        (Intercept) 1996(3) 2000(4) 906.       0.671    1351.    6.86e-42
    #> 10 e        trend       1996(3) 2000(4)   0.669    0.00886    75.5   7.44e-22
    #> # … with 28 more rows

    model <- VAR(Canada)
    tsp.var.fit(model)

![](https://i.imgur.com/JGB5o4i.png)

``` r
tsp.var.resid(model)
```

![](https://i.imgur.com/0pMfFK2.png)

``` r
tsp.var.forecast(model)
```

![](https://i.imgur.com/tH0KCsL.png)

``` r
tsp.var.irf(irf(model))
```

![](https://i.imgur.com/M3Xds7y.png)

``` r
tsp.var.fevd(fevd(model))
```

![](https://i.imgur.com/oDBRbN1.png)

``` r

## Causality Test
# For stationary variables
tsp.granger(VAR(Canada))
#> # A tibble: 12 × 7
#>    caused sign  cause  Chisq    df     p_value H0       
#>    <chr>  <chr> <chr>  <dbl> <dbl>       <dbl> <chr>    
#>  1 e      <-    prod  27.9       1 0.000000128 prod.l1=0
#>  2 e      <-    rw     7.79      1 0.00525     rw.l1=0  
#>  3 e      <-    U     10.0       1 0.00156     U.l1=0   
#>  4 prod   <-    e      0.543     1 0.461       e.l1=0   
#>  5 prod   <-    rw     0.414     1 0.520       rw.l1=0  
#>  6 prod   <-    U      1.82      1 0.177       U.l1=0   
#>  7 rw     <-    e      0.224     1 0.636       e.l1=0   
#>  8 rw     <-    prod   9.03      1 0.00265     prod.l1=0
#>  9 rw     <-    U      0.514     1 0.474       U.l1=0   
#> 10 U      <-    e      9.91      1 0.00165     e.l1=0   
#> 11 U      <-    prod  15.6       1 0.0000800   prod.l1=0
#> 12 U      <-    rw    12.7       1 0.000372    rw.l1=0
# For non stationary variables
tsp.toda_yamamoto(Canada)
#> [1] "Automatic selection for 'd' using adf test:d_max= 1"
#> [1] "Automatic selection for 'p' using AIC(n) criteria:p= 3"
#> $model
#> 
#> VAR Estimation Results:
#> ======================= 
#> 
#> Estimated coefficients for equation e: 
#> ====================================== 
#> Call:
#> e = e.l1 + prod.l1 + rw.l1 + U.l1 + e.l2 + prod.l2 + rw.l2 + U.l2 + e.l3 + prod.l3 + rw.l3 + U.l3 + e.l4 + prod.l4 + rw.l4 + U.l4 + const 
#> 
#>          e.l1       prod.l1         rw.l1          U.l1          e.l2 
#>    1.74218942    0.16397789   -0.09304765    0.15368277   -1.12057885 
#>       prod.l2         rw.l2          U.l2          e.l3       prod.l3 
#>   -0.07717556   -0.01144348   -0.13950250    0.44077173   -0.02763613 
#>         rw.l3          U.l3          e.l4       prod.l4         rw.l4 
#>   -0.01814206    0.31611724    0.06329240    0.01019007    0.06422136 
#>          U.l4         const 
#>   -0.01654811 -123.62311872 
#> 
#> 
#> Estimated coefficients for equation prod: 
#> ========================================= 
#> Call:
#> prod = e.l1 + prod.l1 + rw.l1 + U.l1 + e.l2 + prod.l2 + rw.l2 + U.l2 + e.l3 + prod.l3 + rw.l3 + U.l3 + e.l4 + prod.l4 + rw.l4 + U.l4 + const 
#> 
#>          e.l1       prod.l1         rw.l1          U.l1          e.l2 
#>   -0.20322620    1.13754391    0.04008912   -0.67599169   -0.05970155 
#>       prod.l2         rw.l2          U.l2          e.l3       prod.l3 
#>   -0.15157852   -0.23225253    0.75466980    0.41126623    0.07043146 
#>         rw.l3          U.l3          e.l4       prod.l4         rw.l4 
#>    0.06337671    0.56032005    0.05651630   -0.06705154    0.05578441 
#>          U.l4         const 
#>   -0.18902718 -160.55687500 
#> 
#> 
#> Estimated coefficients for equation rw: 
#> ======================================= 
#> Call:
#> rw = e.l1 + prod.l1 + rw.l1 + U.l1 + e.l2 + prod.l2 + rw.l2 + U.l2 + e.l3 + prod.l3 + rw.l3 + U.l3 + e.l4 + prod.l4 + rw.l4 + U.l4 + const 
#> 
#>        e.l1     prod.l1       rw.l1        U.l1        e.l2     prod.l2 
#> -0.55770823 -0.09936109  0.86953646  0.05678868  0.74829149 -0.16621913 
#>       rw.l2        U.l2        e.l3     prod.l3       rw.l3        U.l3 
#> -0.10591991 -0.46784224 -0.44130876  0.26737005 -0.02104599 -0.10471132 
#>        e.l4     prod.l4       rw.l4        U.l4       const 
#>  0.29570621 -0.08708276  0.21411008  0.25881762 15.63551055 
#> 
#> 
#> Estimated coefficients for equation U: 
#> ====================================== 
#> Call:
#> U = e.l1 + prod.l1 + rw.l1 + U.l1 + e.l2 + prod.l2 + rw.l2 + U.l2 + e.l3 + prod.l3 + rw.l3 + U.l3 + e.l4 + prod.l4 + rw.l4 + U.l4 + const 
#> 
#>         e.l1      prod.l1        rw.l1         U.l1         e.l2      prod.l2 
#> -0.700650589 -0.115380960  0.011568059  0.548572786  0.577664271  0.102755196 
#>        rw.l2         U.l2         e.l3      prod.l3        rw.l3         U.l3 
#>  0.057414586 -0.008460697 -0.089153729 -0.012070213 -0.024326133  0.062084973 
#>         e.l4      prod.l4        rw.l4         U.l4        const 
#>  0.122250511  0.018292775 -0.016474575  0.127542873 77.862996925 
#> 
#> 
#> 
#> $result
#> # A tibble: 12 × 7
#>    caused sign  cause chisq    df          p H0                           
#>    <chr>  <chr> <chr> <dbl> <dbl>      <dbl> <chr>                        
#>  1 e      <-    prod  10.7      3 0.0137     prod.l1=0,prod.l2=0,prod.l3=0
#>  2 e      <-    rw     7.55     3 0.0562     rw.l1=0,rw.l2=0,rw.l3=0      
#>  3 e      <-    U      2.41     3 0.492      U.l1=0,U.l2=0,U.l3=0         
#>  4 prod   <-    e      1.63     3 0.653      e.l1=0,e.l2=0,e.l3=0         
#>  5 prod   <-    rw     4.41     3 0.220      rw.l1=0,rw.l2=0,rw.l3=0      
#>  6 prod   <-    U      7.50     3 0.0576     U.l1=0,U.l2=0,U.l3=0         
#>  7 rw     <-    e      1.98     3 0.577      e.l1=0,e.l2=0,e.l3=0         
#>  8 rw     <-    prod   4.29     3 0.232      prod.l1=0,prod.l2=0,prod.l3=0
#>  9 rw     <-    U      1.86     3 0.602      U.l1=0,U.l2=0,U.l3=0         
#> 10 U      <-    e     26.0      3 0.00000944 e.l1=0,e.l2=0,e.l3=0         
#> 11 U      <-    prod   5.20     3 0.158      prod.l1=0,prod.l2=0,prod.l3=0
#> 12 U      <-    rw     2.68     3 0.444      rw.l1=0,rw.l2=0,rw.l3=0
```

<sup>Created on 2022-10-30 with [reprex v2.0.2.9000](https://reprex.tidyverse.org)</sup>

