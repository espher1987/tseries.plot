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

## Univariate time series

``` r
x <- UKDriverDeaths
plot(x)
library(tseries.plot)
#> Loading required package: magrittr
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
```

![](https://i.imgur.com/ivuZUgY.png)

``` r
tsp.figures(x,table = F)
```

![](https://i.imgur.com/iPvZjHy.png)

``` r
tsp.range_mean(x)
```

![](https://i.imgur.com/JdIVcGa.png)

    #>    year     mean range
    #> 1  1969 1662.583   767
    #> 2  1970 1828.250   958
    #> 3  1971 1859.083   614
    #> 4  1972 1961.667  1085
    #> 5  1973 1987.500   473
    #> 6  1974 1788.333   710
    #> 7  1975 1601.083   843
    #> 8  1976 1602.417   965
    #> 9  1977 1613.583   821
    #> 10 1978 1703.417   816
    #> 11 1979 1664.167   780
    #> 12 1980 1577.667   581
    #> 13 1981 1595.750   553
    #> 14 1982 1621.667   714
    #> 15 1983 1289.333   456
    #> 16 1984 1368.417   653
    tsp.season.box(x,table = F)

![](https://i.imgur.com/nfzrxcZ.png)

``` r
tsp.season.line(x)
#> N : 192   start : 1969   end:  1984   frequency : 12
```

![](https://i.imgur.com/tMRiRx8.png)

``` r
tsp.trend.decompose(x)
```

![](https://i.imgur.com/vv1eRHs.png)

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
#>             x Trend    Cycle
#> Jan 1969 1687  1695   -8.483
#> Feb 1969 1508  1702 -193.595
#> Mar 1969 1507  1708 -200.706
#> Apr 1969 1385  1714 -328.816
#> May 1969 1632  1720  -87.921
#> Jun 1969 1511  1726 -215.016
#> Jul 1969 1559  1732 -173.094
#> Aug 1969 1630  1738 -108.147
#> Sep 1969 1579  1744 -165.167
#> Oct 1969 1653  1750  -97.142
#> Nov 1969 2152  1756  395.939
#> Dec 1969 2148  1762  386.087
#> Jan 1970 1752  1768  -15.688
#> Feb 1970 1765  1773   -8.380
#> Mar 1970 1717  1779  -61.983
#> Apr 1970 1558  1784 -226.490
#> May 1970 1575  1790 -214.894
#> Jun 1970 1520  1795 -275.188
#> Jul 1970 1805  1800    4.640
#> Aug 1970 1800  1805   -5.399
#> Sep 1970 1719  1810  -91.293
#> Oct 1970 2008  1815  192.972
#> Nov 1970 2242  1820  422.409
#> Dec 1970 2478  1824  654.028
#> Jan 1971 2030  1828  201.838
#> Feb 1971 1655  1832 -177.158
#> Mar 1971 1693  1836 -142.957
#> Apr 1971 1623  1840 -216.558
#> May 1971 1805  1843  -37.955
#> Jun 1971 1746  1846 -100.143
#> Jul 1971 1795  1849  -54.116
#> Aug 1971 1926  1852   74.133
#> Sep 1971 1619  1854 -235.389
#> Oct 1971 1992  1857  135.326
#> Nov 1971 2233  1859  374.285
#> Dec 1971 2192  1861  331.497
#> Jan 1972 2080  1862  217.965
#> Feb 1972 1768  1863  -95.308
#> Mar 1972 1835  1864  -29.321
#> Apr 1972 1569  1865 -296.074
#> May 1972 1976  1866  110.435
#> Jun 1972 1853  1866  -12.790
#> Jul 1972 1965  1866   99.252
#> Aug 1972 1689  1865 -176.434
#> Sep 1972 1778  1865  -86.847
#> Oct 1972 1976  1864  112.017
#> Nov 1972 2397  1863  534.161
#> Dec 1972 2654  1861  792.590
#> Jan 1973 2097  1860  237.302
#> Feb 1973 1963  1858  105.290
#> Mar 1973 1677  1855 -178.454
#> Apr 1973 1941  1853   88.060
#> May 1973 2003  1850  152.824
#> Jun 1973 1813  1847  -34.172
#> Jul 1973 2012  1844  168.063
#> Aug 1973 1912  1840   71.520
#> Sep 1973 2084  1837  247.186
#> Oct 1973 2080  1833  247.050
#> Nov 1973 2118  1829  289.099
#> Dec 1973 2150  1825  325.316
#> Jan 1974 1608  1820 -212.314
#> Feb 1974 1503  1816 -312.814
#> Mar 1974 1548  1811 -263.200
#> Apr 1974 1382  1806 -424.491
#> May 1974 1731  1802  -70.699
#> Jun 1974 1798  1797    1.165
#> Jul 1974 1779  1792  -12.911
#> Aug 1974 1887  1787  100.065
#> Sep 1974 2004  1782  222.081
#> Oct 1974 2077  1777  300.127
#> Nov 1974 2092  1772  320.189
#> Dec 1974 2051  1767  284.254
#> Jan 1975 1577  1762 -184.698
#> Feb 1975 1356  1757 -400.684
#> Mar 1975 1652  1752  -99.724
#> Apr 1975 1382  1747 -364.833
#> May 1975 1519  1742 -223.025
#> Jun 1975 1421  1737 -316.311
#> Jul 1975 1442  1733 -290.703
#> Aug 1975 1543  1728 -185.206
#> Sep 1975 1656  1724  -67.826
#> Oct 1975 1561  1720 -158.568
#> Nov 1975 1905  1715  189.567
#> Dec 1975 2199  1711  487.575
#> Jan 1976 1473  1708 -234.547
#> Feb 1976 1655  1704  -48.805
#> Mar 1976 1407  1700 -293.206
#> Apr 1976 1395  1697 -301.754
#> May 1976 1530  1693 -163.452
#> Jun 1976 1309  1690 -381.301
#> Jul 1976 1526  1687 -161.300
#> Aug 1976 1327  1684 -357.444
#> Sep 1976 1627  1682  -54.730
#> Oct 1976 1748  1679   68.851
#> Nov 1976 1958  1677  281.307
#> Dec 1976 2274  1674  599.645
#> Jan 1977 1648  1672  -24.128
#> Feb 1977 1401  1670 -269.013
#> Mar 1977 1411  1668 -257.008
#> Apr 1977 1403  1666 -263.110
#> May 1977 1394  1664 -270.314
#> Jun 1977 1520  1663 -142.613
#> Jul 1977 1528  1661 -132.997
#> Aug 1977 1643  1659  -16.457
#> Sep 1977 1515  1658 -142.980
#> Oct 1977 1685  1657   28.444
#> Nov 1977 2000  1655  344.827
#> Dec 1977 2215  1654  561.184
#> Jan 1978 1956  1652  303.522
#> Feb 1978 1462  1651 -189.152
#> Mar 1978 1563  1650  -86.836
#> Apr 1978 1459  1649 -189.525
#> May 1978 1446  1647 -201.214
#> Jun 1978 1622  1646  -23.896
#> Jul 1978 1657  1645   12.437
#> Aug 1978 1638  1643   -5.207
#> Sep 1978 1643  1642    1.180
#> Oct 1978 1683  1640   42.607
#> Nov 1978 2050  1639  411.081
#> Dec 1978 2262  1637  624.612
#> Jan 1979 1813  1636  177.203
#> Feb 1979 1445  1634 -189.145
#> Mar 1979 1762  1632  129.567
#> Apr 1979 1461  1631 -169.663
#> May 1979 1556  1629  -72.834
#> Jun 1979 1431  1627 -195.947
#> Jul 1979 1427  1625 -198.000
#> Aug 1979 1554  1623  -68.991
#> Sep 1979 1645  1621   24.084
#> Oct 1979 1653  1619   34.229
#> Nov 1979 2016  1617  399.449
#> Dec 1979 2207  1614  592.748
#> Jan 1980 1665  1612   53.127
#> Feb 1980 1361  1609 -248.419
#> Mar 1980 1506  1607 -100.892
#> Apr 1980 1360  1604 -244.295
#> May 1980 1453  1602 -148.629
#> Jun 1980 1522  1599  -76.895
#> Jul 1980 1460  1596 -136.089
#> Aug 1980 1552  1593  -41.210
#> Sep 1980 1548  1590  -42.254
#> Oct 1980 1827  1587  239.781
#> Nov 1980 1737  1584  152.901
#> Dec 1980 1941  1581  360.107
#> Jan 1981 1474  1578 -103.599
#> Feb 1981 1458  1574 -116.220
#> Mar 1981 1542  1571  -28.756
#> Apr 1981 1404  1567 -163.209
#> May 1981 1522  1564  -41.576
#> Jun 1981 1385  1560 -174.858
#> Jul 1981 1641  1556   84.947
#> Aug 1981 1510  1552  -42.156
#> Sep 1981 1681  1548  132.833
#> Oct 1981 1938  1544  393.917
#> Nov 1981 1868  1540  328.099
#> Dec 1981 1726  1536  190.377
#> Jan 1982 1456  1531  -75.254
#> Feb 1982 1445  1527  -81.797
#> Mar 1982 1456  1522  -66.259
#> Apr 1982 1365  1518 -152.643
#> May 1982 1487  1513  -25.954
#> Jun 1982 1558  1508   49.808
#> Jul 1982 1488  1503  -15.361
#> Aug 1982 1684  1498  185.536
#> Sep 1982 1594  1494  100.497
#> Oct 1982 1850  1488  361.519
#> Nov 1982 1998  1483  514.596
#> Dec 1982 2079  1478  600.721
#> Jan 1983 1494  1473   20.882
#> Feb 1983 1057  1468 -410.937
#> Mar 1983 1218  1463 -244.751
#> Apr 1983 1168  1458 -289.575
#> May 1983 1236  1452 -216.419
#> Jun 1983 1076  1447 -371.292
#> Jul 1983 1174  1442 -268.203
#> Aug 1983 1139  1437 -298.154
#> Sep 1983 1427  1432   -5.149
#> Oct 1983 1487  1427   59.812
#> Nov 1983 1483  1422   60.729
#> Dec 1983 1513  1417   95.602
#> Jan 1984 1357  1413  -55.570
#> Feb 1984 1165  1408 -242.789
#> Mar 1984 1282  1403 -121.057
#> Apr 1984 1110  1398 -288.372
#> May 1984 1297  1394  -96.733
#> Jun 1984 1185  1389 -204.137
#> Jul 1984 1222  1385 -162.579
#> Aug 1984 1284  1380  -96.053
#> Sep 1984 1444  1376   68.449
#> Oct 1984 1575  1371  203.934
#> Nov 1984 1737  1367  370.410
#> Dec 1984 1763  1362  400.883
```

![](https://i.imgur.com/wGN4n7r.png)

``` r
tsp.year(x)
#>    year median     mean       sd      var
#> 1  1969 1604.5 1662.583 241.9036 58517.36
#> 2  1970 1758.5 1828.250 284.9766 81211.66
#> 3  1971 1800.0 1859.083 214.1437 45857.54
#> 4  1972 1909.0 1961.667 303.1490 91899.33
#> 5  1973 2007.5 1987.500 137.9918 19041.73
#> 6  1974 1788.5 1788.333 241.4074 58277.52
#> 7  1975 1552.0 1601.083 239.9956 57597.90
#> 8  1976 1528.0 1602.417 281.7183 79365.17
#> 9  1977 1524.0 1613.583 256.3316 65705.90
#> 10 1978 1640.5 1703.417 255.3189 65187.72
#> 11 1979 1600.5 1664.167 247.6125 61311.97
#> 12 1980 1535.0 1577.667 181.0767 32788.79
#> 13 1981 1532.0 1595.750 177.9168 31654.39
#> 14 1982 1523.0 1621.667 233.4723 54509.33
#> 15 1983 1227.0 1289.333 177.2242 31408.42
#> 16 1984 1290.5 1368.417 218.0494 47545.54
```

![](https://i.imgur.com/AkEkSHp.png)

```
### Also using mfilter package

``` r
library(mFilter)
tsp.mfilter(hpfilter(x))
```

![](https://i.imgur.com/Fld1sqN.png)

```

## Using forecast package

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

![](https://i.imgur.com/2Mh91dt.png)

```

## Using Strucchange package

``` r
library(strucchange)
#> Loading required package: zoo
#> 
#> Attaching package: 'zoo'
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric
#> Loading required package: sandwich
tsp.break(breakpoints(x~1))
#> Warning: Unknown or uninitialised column: `o`.
#> Warning in min(df$o): ningún argumento finito para min; retornando Inf
#> Warning: Unknown or uninitialised column: `o`.
#> Warning in min(df$o): ningún argumento finito para min; retornando Inf
#> Warning: Unknown or uninitialised column: `o`.
#> Warning in min(df$o): ningún argumento finito para min; retornando Inf
#> Warning: Unknown or uninitialised column: `o`.
#> Warning in max(df$o): ningun argumento finito para max; retornando -Inf
#> Warning: Unknown or uninitialised column: `o`.
#> Warning in max(df$o): ningun argumento finito para max; retornando -Inf
#> Warning: Unknown or uninitialised column: `o`.
#> Warning in max(df$o): ningun argumento finito para max; retornando -Inf
```

![](https://i.imgur.com/aeD0A37.png)

```

## Using vars package

``` r
library(vars)
#> Loading required package: MASS
#> Loading required package: urca
#> Loading required package: lmtest
data("Canada")
model <- VAR(Canada)
tsp.var.fit(model)
```

![](https://i.imgur.com/HjERdsF.png)

``` r
tsp.var.resid(model)
```

![](https://i.imgur.com/1G6gwuT.png)

``` r
tsp.var.forecast(model)
```

![](https://i.imgur.com/KlKuLNp.png)

``` r
tsp.var.irf(irf(model))
```

![](https://i.imgur.com/9B8qG8k.png)

``` r
tsp.var.fevd(fevd(model))
```

![](https://i.imgur.com/15IzOKQ.png)

```
tseries.plot include Toda and Yamamoto causality test for non-stationary time series in VAR model

``` r
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

<sup>Created on 2022-02-11 by the [reprex package](https://reprex.tidyverse.org) (v1.0.0)</sup>
