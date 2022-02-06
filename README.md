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
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
# A partir de la serie de pasajeros de avión
plot(AirPassengers)
```

![](https://i.imgur.com/a5iRixR.png)

``` r
# Dentro de los primeros análisis a realizar en una serie de tiempo se encuentra el análisis de los componente de la serie de tiempo:
tsp.trend.decompose(AirPassengers)
```

![](https://i.imgur.com/TNqJHBu.png)

``` r
# Análisis del componente estacional
tsp.figures(AirPassengers,table = F)
```

![](https://i.imgur.com/xhomCaC.png)

``` r
tsp.season.box(AirPassengers,table = F)
```

![](https://i.imgur.com/VMFnwuF.png)

``` r
# Además es útil el análisis de la varianza de la serie
tsp.year(AirPassengers,table=F)
```

![](https://i.imgur.com/dKD2g6t.png)

``` r
tsp.range.mean(AirPassengers,table = F)
#> `geom_smooth()` using formula 'y ~ x'
```

![](https://i.imgur.com/9B4LBDp.png)

``` r
# Con la ayuda del paquete mFilter
library(mFilter)
# Es posible suavizar la serie
tsp.mfilter(mFilter(AirPassengers))
```

![](https://i.imgur.com/8QNEi8Q.png)

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
#> Error in tibble(imp = names(data)[d], lag = 0:{: no se pudo encontrar la función "tibble"

model_fevd <- fevd(model)
tsp.var.fevd(model_fevd)
```

![](https://i.imgur.com/1VqMC0m.png)

``` r
# Causalidad en el modelo
# Tomando en cuenta las raices unitarias es posible aplicar el método de Toda y Yamamoto
# Partiendo del supuesto que las series tienen raices unitarias (ambas)
tsp.toda_yamamoto(Canada,p = 3,d = 1)
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

<sup>Created on 2022-02-05 by the [reprex package](https://reprex.tidyverse.org) (v1.0.0)</sup>
