iteration_and_listcols
================
cathy
2024-10-29

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## here’s some lists

``` r
l = list(
  vec_number = 1:4,
  unif_sample = runif(100),
  mat = matrix(1:8, nrow = 2, ncol = 4, byrow = TRUE),
  summary = summary(rnorm(1000))
)

l
```

    ## $vec_number
    ## [1] 1 2 3 4
    ## 
    ## $unif_sample
    ##   [1] 0.73479054 0.45789551 0.25138127 0.83977857 0.44433435 0.26161740
    ##   [7] 0.84078822 0.73841318 0.88300134 0.78774193 0.37234536 0.25731301
    ##  [13] 0.99400240 0.64750025 0.20209075 0.90054840 0.46927732 0.68368360
    ##  [19] 0.70335607 0.65460592 0.20574237 0.42859466 0.37545788 0.58561372
    ##  [25] 0.02719564 0.11954752 0.30636531 0.45237872 0.69474756 0.37536285
    ##  [31] 0.21318804 0.34114408 0.10467853 0.90686322 0.01655848 0.74559218
    ##  [37] 0.51955726 0.29127679 0.50129647 0.21255392 0.88037485 0.14674954
    ##  [43] 0.97042520 0.81237944 0.09855567 0.51936450 0.52680341 0.83450178
    ##  [49] 0.77131876 0.90793238 0.42632556 0.43043321 0.72298082 0.72203881
    ##  [55] 0.37882354 0.38144243 0.69663796 0.15644384 0.79322552 0.55838608
    ##  [61] 0.73858709 0.04067728 0.60671545 0.26787782 0.05281946 0.33050325
    ##  [67] 0.32172275 0.03384202 0.12640951 0.16365524 0.02525683 0.20588096
    ##  [73] 0.63131480 0.72385122 0.40758820 0.30880069 0.35962558 0.04863190
    ##  [79] 0.34384013 0.26654166 0.74644029 0.88574423 0.92078725 0.61620655
    ##  [85] 0.85620677 0.89268537 0.03818025 0.63044429 0.85268427 0.93231583
    ##  [91] 0.26164067 0.80980238 0.50176830 0.47042575 0.74776546 0.60354763
    ##  [97] 0.57441690 0.90715104 0.47455555 0.53942430
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8
    ## 
    ## $summary
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -3.258467 -0.702987  0.017482 -0.004178  0.666332  3.633961

``` r
l$mat
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8

``` r
l[["mat"]][1,3]
```

    ## [1] 3

``` r
l[[4]]
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -3.258467 -0.702987  0.017482 -0.004178  0.666332  3.633961

make a list

``` r
list_norm = 
  list(
  a = rnorm(20, 0, 5),
  b = rnorm(20, 4, 5),
  c = rnorm(20, 0, 10),
  d = rnorm(20, 4, 10)
)

list_norm[["b"]]
```

    ##  [1]  6.1310912 -3.7637998  1.6968759  1.0438914  1.4043672  1.3301631
    ##  [7]  2.0045765 10.2603633  7.5027707  5.6709456  2.4320238  4.5254528
    ## [13] 11.4931389  9.3447966 10.1504817  7.4208757  2.8555007  2.9980222
    ## [19]  3.4251649  0.5569411

``` r
mean_and_sd = function(x){
  
   mean_x = mean(x)
   sd_x = sd(x)
   
   out_df = 
     tibble(
       mean =mean_x,
       sd = sd_x
     )
  
  return(out_df)
  
}
```

``` r
mean_and_sd(list_norm[["a"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.214  4.73

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.42  3.97

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.621  9.74

``` r
mean_and_sd(list_norm[["d"]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.121  8.37

## Use a for loop

``` r
output =vector("list", length = 4)

for (i in 1:4){
   
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}

 output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.214  4.73
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.42  3.97
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.621  9.74
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.121  8.37

## Do the same thing

but with a ‘map’ instead

``` r
map(list_norm, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.214  4.73
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.42  3.97
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.621  9.74
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.121  8.37

``` r
map(list_norm, median)
```

    ## $a
    ## [1] 0.5313892
    ## 
    ## $b
    ## [1] 3.211594
    ## 
    ## $c
    ## [1] -2.669026
    ## 
    ## $d
    ## [1] 0.2325465

``` r
map(list_norm, IQR)
```

    ## $a
    ## [1] 8.201322
    ## 
    ## $b
    ## [1] 5.817601
    ## 
    ## $c
    ## [1] 8.111709
    ## 
    ## $d
    ## [1] 9.388282
