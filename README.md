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

# A partir de la serie de pasajeros de avión
plot(AirPassengers)
```

![](https://i.imgur.com/p7FYAvk.png)

``` r
# Dentro de los primeros análisis a realizar en una serie de tiempo se encuentra el análisis de los componente de la serie de tiempo:
tsp.trend.decompose(AirPassengers)
```

![](https://i.imgur.com/Obdn10j.png)

``` r
# Análisis del componente estacional
tsp.figures(AirPassengers,table = F)
```

![](https://i.imgur.com/bF95KIG.png)

``` r
tsp.season.box(AirPassengers,table = F)
```

![](https://i.imgur.com/1HcJiDa.png)

``` r
# Además es útil el análisis de la varianza de la serie
tsp.year(AirPassengers,table=F)
```

![](https://i.imgur.com/n9RTMP2.png)

``` r
tsp.range.mean(AirPassengers,table = F)
#> `geom_smooth()` using formula 'y ~ x'
```

![](https://i.imgur.com/QPGsKp9.png)

``` r
# Con la ayuda del paquete mFilter
library(mFilter)
# Es posible suavizar la serie
tsp.mfilter(mFilter(AirPassengers))
```

![](https://i.imgur.com/PKgq4YK.png)

``` r
# Aunque estas son las principales funciones del paquete tambien existen algunas funciones adicionales aplicados al marco de Vectores Autorregresivos (VAR)
# Utilizando el paquete vars
library(vars)
#> Loading required package: MASS
#> Loading required package: strucchange
#> Loading required package: zoo
#> 
#> Attaching package: 'zoo'
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric
#> Loading required package: sandwich
#> Loading required package: urca
#> Loading required package: lmtest

data("Canada")
model<- VAR(Canada,3)

model_irf <- irf(model)
tsp.var.irf(model_irf)
```

![](https://i.imgur.com/UMPbAS4.png)

``` r


model_fevd <- fevd(model)
tsp.var.fevd(model_fevd)
```

![](https://i.imgur.com/l8YRr1v.png)

``` r
# Causalidad en el modelo
# Tomando en cuenta las raices unitarias es posible aplicar el método de Toda y Yamamoto
# Partiendo del supuesto que las series tienen raices unitarias (ambas)
ty <- tsp.toda_yamamoto(Canada,p = 3,d = 1)
ty$result
#> # A tibble: 12 x 7
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

<sup>Created on 2022-02-06 by the [reprex package](https://reprex.tidyverse.org) (v1.0.0)</sup>
