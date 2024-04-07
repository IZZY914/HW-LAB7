Untitled
================

``` r
load("~/Documents/izzy work/Household_Pulse_data_ph4c2.RData")
```

``` r
require(plyr)
```

    ## Loading required package: plyr

``` r
require(dplyr)
```

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
require(tidyverse)
```

    ## Loading required package: tidyverse

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.5
    ## ✔ ggplot2   3.4.4     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::arrange()   masks plyr::arrange()
    ## ✖ purrr::compact()   masks plyr::compact()
    ## ✖ dplyr::count()     masks plyr::count()
    ## ✖ dplyr::desc()      masks plyr::desc()
    ## ✖ dplyr::failwith()  masks plyr::failwith()
    ## ✖ dplyr::filter()    masks stats::filter()
    ## ✖ dplyr::id()        masks plyr::id()
    ## ✖ dplyr::lag()       masks stats::lag()
    ## ✖ dplyr::mutate()    masks plyr::mutate()
    ## ✖ dplyr::rename()    masks plyr::rename()
    ## ✖ dplyr::summarise() masks plyr::summarise()
    ## ✖ dplyr::summarize() masks plyr::summarize()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
require(ggplot2)
require(stargazer)
```

    ## Loading required package: stargazer
    ## 
    ## Please cite as: 
    ## 
    ##  Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables.
    ##  R package version 5.2.3. https://CRAN.R-project.org/package=stargazer

``` r
load("Household_Pulse_data_ph4c2.RData")
```

``` r
select1 <- (Household_Pulse_data$MHLTH_NEED != "NA")
d_kids <- subset(Household_Pulse_data,select1)
```

``` r
d_kids$MentHealthKids <- as.numeric(
(d_kids$MHLTH_NEED == "all children need mental health treatment") |
(d_kids$MHLTH_NEED == "some but not all children") )
```

``` r
ddply(d_kids,.(RRACE), summarize, avg = mean(MentHealthKids))
```

    ##   RRACE        avg
    ## 1 White 0.19977084
    ## 2 Black 0.12881023
    ## 3 Asian 0.06986532
    ## 4 Other 0.20510949

``` r
ols_out1 <- lm(MentHealthKids ~ -1 + RRACE, data = d_kids)
stargazer(ols_out1, type = "text")
```

    ## 
    ## ================================================
    ##                         Dependent variable:     
    ##                     ----------------------------
    ##                            MentHealthKids       
    ## ------------------------------------------------
    ## RRACEWhite                    0.200***          
    ##                               (0.003)           
    ##                                                 
    ## RRACEBlack                    0.129***          
    ##                               (0.009)           
    ##                                                 
    ## RRACEAsian                    0.070***          
    ##                               (0.011)           
    ##                                                 
    ## RRACEOther                    0.205***          
    ##                               (0.010)           
    ##                                                 
    ## ------------------------------------------------
    ## Observations                   19,429           
    ## R2                             0.192            
    ## Adjusted R2                    0.192            
    ## Residual Std. Error      0.386 (df = 19425)     
    ## F Statistic         1,154.009*** (df = 4; 19425)
    ## ================================================
    ## Note:                *p<0.1; **p<0.05; ***p<0.01

``` r
ols_out1a <- lm(MentHealthKids ~ RRACE, data = d_kids)
stargazer(ols_out1a, type = "Text")
```

``` r
p_avg_byrace <- ggplot(d_kids, aes(x = RRACE, fill = MHLTH_NEED))
p_avg_byrace + geom_bar(position = "fill") +
scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.85)
```

![](Lab7_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ols_out2 <- lm(MentHealthKids ~ RRACE + RHISPANIC, data = d_kids)
stargazer(ols_out2, type = "text")
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                           MentHealthKids       
    ## -----------------------------------------------
    ## RRACEBlack                   -0.072***         
    ##                               (0.009)          
    ##                                                
    ## RRACEAsian                   -0.131***         
    ##                               (0.012)          
    ##                                                
    ## RRACEOther                     0.008           
    ##                               (0.011)          
    ##                                                
    ## RHISPANICHispanic            -0.019**          
    ##                               (0.009)          
    ##                                                
    ## Constant                     0.202***          
    ##                               (0.003)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                  19,429           
    ## R2                             0.009           
    ## Adjusted R2                    0.009           
    ## Residual Std. Error     0.386 (df = 19424)     
    ## F Statistic          44.638*** (df = 4; 19424) 
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
ols_out2a <- lm(MentHealthKids ~ RRACE*RHISPANIC, data = d_kids)
stargazer(ols_out2a, type = "text")
```

    ## 
    ## ========================================================
    ##                                  Dependent variable:    
    ##                              ---------------------------
    ##                                    MentHealthKids       
    ## --------------------------------------------------------
    ## RRACEBlack                            -0.077***         
    ##                                        (0.009)          
    ##                                                         
    ## RRACEAsian                            -0.138***         
    ##                                        (0.012)          
    ##                                                         
    ## RRACEOther                             -0.001           
    ##                                        (0.013)          
    ##                                                         
    ## RHISPANICHispanic                     -0.032***         
    ##                                        (0.010)          
    ##                                                         
    ## RRACEBlack:RHISPANICHispanic           0.062*           
    ##                                        (0.037)          
    ##                                                         
    ## RRACEAsian:RHISPANICHispanic           0.143**          
    ##                                        (0.056)          
    ##                                                         
    ## RRACEOther:RHISPANICHispanic           0.043*           
    ##                                        (0.026)          
    ##                                                         
    ## Constant                              0.204***          
    ##                                        (0.003)          
    ##                                                         
    ## --------------------------------------------------------
    ## Observations                           19,429           
    ## R2                                      0.010           
    ## Adjusted R2                             0.009           
    ## Residual Std. Error              0.386 (df = 19421)     
    ## F Statistic                   27.063*** (df = 7; 19421) 
    ## ========================================================
    ## Note:                        *p<0.1; **p<0.05; ***p<0.01

``` r
anova(ols_out2,ols_out2a)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: MentHealthKids ~ RRACE + RHISPANIC
    ## Model 2: MentHealthKids ~ RRACE * RHISPANIC
    ##   Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
    ## 1  19424 2900.0                              
    ## 2  19421 2898.4  3     1.614 3.6049 0.01279 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
p_avg_byrace <- ggplot(d_kids, aes(x = RRACE, fill = MHLTH_NEED))
p_avg_byrace + geom_bar(position = "fill") +
scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.85) +
facet_grid(~RHISPANIC)
```

![](Lab7_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
ols_out3 <- lm(MentHealthKids ~ RHISPANIC*RRACE*EEDUC*MS + PRIVHLTH, data = d_kids)
stargazer(ols_out3, type = "text")
```

    ## 
    ## ====================================================================================
    ##                                                              Dependent variable:    
    ##                                                          ---------------------------
    ##                                                                MentHealthKids       
    ## ------------------------------------------------------------------------------------
    ## RHISPANICHispanic                                                  1.180**          
    ##                                                                    (0.573)          
    ##                                                                                     
    ## RRACEBlack                                                          0.454           
    ##                                                                    (0.374)          
    ##                                                                                     
    ## RRACEAsian                                                         -0.109           
    ##                                                                    (0.683)          
    ##                                                                                     
    ## RRACEOther                                                         -1.179*          
    ##                                                                    (0.633)          
    ##                                                                                     
    ## EEDUCsome hs                                                      2.166***          
    ##                                                                    (0.742)          
    ##                                                                                     
    ## EEDUCHS diploma                                                    1.207*           
    ##                                                                    (0.690)          
    ##                                                                                     
    ## EEDUCsome coll                                                     1.189*           
    ##                                                                    (0.672)          
    ##                                                                                     
    ## EEDUCassoc deg                                                     1.341*           
    ##                                                                    (0.709)          
    ##                                                                                     
    ## EEDUCbach deg                                                      1.383**          
    ##                                                                    (0.657)          
    ##                                                                                     
    ## EEDUCadv deg                                                       1.207**          
    ##                                                                    (0.505)          
    ##                                                                                     
    ## MSmarried                                                          1.461**          
    ##                                                                    (0.639)          
    ##                                                                                     
    ## MSwidowed                                                          1.171*           
    ##                                                                    (0.672)          
    ##                                                                                     
    ## MSdivorced                                                         1.452**          
    ##                                                                    (0.642)          
    ##                                                                                     
    ## MSseparated                                                       1.763***          
    ##                                                                    (0.651)          
    ##                                                                                     
    ## MSnever                                                            1.310**          
    ##                                                                    (0.627)          
    ##                                                                                     
    ## PRIVHLTHno private health ins                                      0.015*           
    ##                                                                    (0.009)          
    ##                                                                                     
    ## PRIVHLTHNA                                                        -0.026***         
    ##                                                                    (0.009)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack                                       -0.372           
    ##                                                                    (0.289)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian                                       0.818*           
    ##                                                                    (0.493)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther                                        0.291           
    ##                                                                    (0.310)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCsome hs                                    -1.652**          
    ##                                                                    (0.742)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCHS diploma                                  -0.211           
    ##                                                                    (0.766)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCsome coll                                   -1.176*          
    ##                                                                    (0.672)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCassoc deg                                   -1.314*          
    ##                                                                    (0.759)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCbach deg                                   -0.379**          
    ##                                                                    (0.161)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCadv deg                                     -0.207           
    ##                                                                    (0.184)          
    ##                                                                                     
    ## RRACEBlack:EEDUCsome hs                                            -0.043           
    ##                                                                    (0.205)          
    ##                                                                                     
    ## RRACEAsian:EEDUCsome hs                                             0.087           
    ##                                                                    (0.418)          
    ##                                                                                     
    ## RRACEOther:EEDUCsome hs                                             0.067           
    ##                                                                    (0.276)          
    ##                                                                                     
    ## RRACEBlack:EEDUCHS diploma                                         -0.477           
    ##                                                                    (0.501)          
    ##                                                                                     
    ## RRACEAsian:EEDUCHS diploma                                          0.070           
    ##                                                                    (0.407)          
    ##                                                                                     
    ## RRACEOther:EEDUCHS diploma                                          0.045           
    ##                                                                    (0.250)          
    ##                                                                                     
    ## RRACEBlack:EEDUCsome coll                                          -0.436           
    ##                                                                    (0.580)          
    ##                                                                                     
    ## RRACEAsian:EEDUCsome coll                                           0.100           
    ##                                                                    (0.814)          
    ##                                                                                     
    ## RRACEOther:EEDUCsome coll                                           0.165           
    ##                                                                    (0.249)          
    ##                                                                                     
    ## RRACEBlack:EEDUCassoc deg                                          -0.614           
    ##                                                                    (0.828)          
    ##                                                                                     
    ## RRACEAsian:EEDUCassoc deg                                          -0.052           
    ##                                                                    (0.414)          
    ##                                                                                     
    ## RRACEOther:EEDUCassoc deg                                           1.331           
    ##                                                                    (0.863)          
    ##                                                                                     
    ## RRACEBlack:EEDUCbach deg                                           -0.163           
    ##                                                                    (0.192)          
    ##                                                                                     
    ## RRACEAsian:EEDUCbach deg                                           -0.051           
    ##                                                                    (0.408)          
    ##                                                                                     
    ## RRACEOther:EEDUCbach deg                                            0.129           
    ##                                                                    (0.254)          
    ##                                                                                     
    ## RRACEBlack:EEDUCadv deg                                            -0.195           
    ##                                                                    (0.195)          
    ##                                                                                     
    ## RRACEAsian:EEDUCadv deg                                             0.109           
    ##                                                                    (0.415)          
    ##                                                                                     
    ## RRACEOther:EEDUCadv deg                                             0.188           
    ##                                                                    (0.262)          
    ##                                                                                     
    ## RHISPANICHispanic:MSmarried                                       -1.366**          
    ##                                                                    (0.582)          
    ##                                                                                     
    ## RHISPANICHispanic:MSwidowed                                        -1.144           
    ##                                                                    (0.725)          
    ##                                                                                     
    ## RHISPANICHispanic:MSdivorced                                      -1.288**          
    ##                                                                    (0.603)          
    ##                                                                                     
    ## RHISPANICHispanic:MSseparated                                     -1.763***         
    ##                                                                    (0.612)          
    ##                                                                                     
    ## RHISPANICHispanic:MSnever                                          -0.978*          
    ##                                                                    (0.554)          
    ##                                                                                     
    ## RRACEBlack:MSmarried                                               -0.529           
    ##                                                                    (0.419)          
    ##                                                                                     
    ## RRACEAsian:MSmarried                                               -0.045           
    ##                                                                    (0.701)          
    ##                                                                                     
    ## RRACEOther:MSmarried                                                0.891           
    ##                                                                    (0.693)          
    ##                                                                                     
    ## RRACEBlack:MSwidowed                                               -0.418           
    ##                                                                    (0.580)          
    ##                                                                                     
    ## RRACEAsian:MSwidowed                                                0.145           
    ##                                                                    (0.815)          
    ##                                                                                     
    ## RRACEOther:MSwidowed                                               1.681**          
    ##                                                                    (0.724)          
    ##                                                                                     
    ## RRACEBlack:MSdivorced                                              -0.391           
    ##                                                                    (0.335)          
    ##                                                                                     
    ## RRACEAsian:MSdivorced                                              -0.014           
    ##                                                                    (0.560)          
    ##                                                                                     
    ## RRACEOther:MSdivorced                                              1.240*           
    ##                                                                    (0.678)          
    ##                                                                                     
    ## RRACEBlack:MSseparated                                            -1.023**          
    ##                                                                    (0.485)          
    ##                                                                                     
    ## RRACEAsian:MSseparated                                              0.512           
    ##                                                                    (0.797)          
    ##                                                                                     
    ## RRACEOther:MSseparated                                              1.102           
    ##                                                                    (0.704)          
    ##                                                                                     
    ## RRACEBlack:MSnever                                                 -0.411           
    ##                                                                    (0.325)          
    ##                                                                                     
    ## RRACEAsian:MSnever                                                 -0.036           
    ##                                                                    (0.556)          
    ##                                                                                     
    ## RRACEOther:MSnever                                                 1.048*           
    ##                                                                    (0.585)          
    ##                                                                                     
    ## EEDUCsome hs:MSmarried                                            -2.226***         
    ##                                                                    (0.747)          
    ##                                                                                     
    ## EEDUCHS diploma:MSmarried                                          -1.333*          
    ##                                                                    (0.694)          
    ##                                                                                     
    ## EEDUCsome coll:MSmarried                                           -1.273*          
    ##                                                                    (0.676)          
    ##                                                                                     
    ## EEDUCassoc deg:MSmarried                                          -1.413**          
    ##                                                                    (0.713)          
    ##                                                                                     
    ## EEDUCbach deg:MSmarried                                           -1.483**          
    ##                                                                    (0.662)          
    ##                                                                                     
    ## EEDUCadv deg:MSmarried                                            -1.293**          
    ##                                                                    (0.511)          
    ##                                                                                     
    ## EEDUCsome hs:MSwidowed                                            -1.904**          
    ##                                                                    (0.782)          
    ##                                                                                     
    ## EEDUCHS diploma:MSwidowed                                          -1.045           
    ##                                                                    (0.726)          
    ##                                                                                     
    ## EEDUCsome coll:MSwidowed                                           -0.959           
    ##                                                                    (0.709)          
    ##                                                                                     
    ## EEDUCassoc deg:MSwidowed                                           -1.108           
    ##                                                                    (0.745)          
    ##                                                                                     
    ## EEDUCbach deg:MSwidowed                                            -1.120           
    ##                                                                    (0.695)          
    ##                                                                                     
    ## EEDUCadv deg:MSwidowed                                             -0.892           
    ##                                                                    (0.554)          
    ##                                                                                     
    ## EEDUCsome hs:MSdivorced                                           -2.275***         
    ##                                                                    (0.752)          
    ##                                                                                     
    ## EEDUCHS diploma:MSdivorced                                         -1.315*          
    ##                                                                    (0.698)          
    ##                                                                                     
    ## EEDUCsome coll:MSdivorced                                          -1.133*          
    ##                                                                    (0.680)          
    ##                                                                                     
    ## EEDUCassoc deg:MSdivorced                                          -1.342*          
    ##                                                                    (0.716)          
    ##                                                                                     
    ## EEDUCbach deg:MSdivorced                                          -1.353**          
    ##                                                                    (0.665)          
    ##                                                                                     
    ## EEDUCadv deg:MSdivorced                                           -1.086**          
    ##                                                                    (0.515)          
    ##                                                                                     
    ## EEDUCsome hs:MSseparated                                          -2.680***         
    ##                                                                    (0.763)          
    ##                                                                                     
    ## EEDUCHS diploma:MSseparated                                       -1.592**          
    ##                                                                    (0.707)          
    ##                                                                                     
    ## EEDUCsome coll:MSseparated                                        -1.522**          
    ##                                                                    (0.690)          
    ##                                                                                     
    ## EEDUCassoc deg:MSseparated                                        -1.667**          
    ##                                                                    (0.726)          
    ##                                                                                     
    ## EEDUCbach deg:MSseparated                                         -1.631**          
    ##                                                                    (0.675)          
    ##                                                                                     
    ## EEDUCadv deg:MSseparated                                          -1.353**          
    ##                                                                    (0.530)          
    ##                                                                                     
    ## EEDUCsome hs:MSnever                                              -2.130***         
    ##                                                                    (0.737)          
    ##                                                                                     
    ## EEDUCHS diploma:MSnever                                            -1.151*          
    ##                                                                    (0.683)          
    ##                                                                                     
    ## EEDUCsome coll:MSnever                                             -1.147*          
    ##                                                                    (0.665)          
    ##                                                                                     
    ## EEDUCassoc deg:MSnever                                             -1.270*          
    ##                                                                    (0.701)          
    ##                                                                                     
    ## EEDUCbach deg:MSnever                                              -1.257*          
    ##                                                                    (0.650)          
    ##                                                                                     
    ## EEDUCadv deg:MSnever                                              -1.119**          
    ##                                                                    (0.494)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCsome hs                          0.786**          
    ##                                                                    (0.347)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCsome hs                          -0.639           
    ##                                                                    (0.943)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCsome hs                          -0.323           
    ##                                                                    (0.394)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCHS diploma                        0.278           
    ##                                                                    (0.316)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCHS diploma                       -0.891           
    ##                                                                    (0.633)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCHS diploma                       -0.168           
    ##                                                                    (0.332)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCsome coll                        0.531*           
    ##                                                                    (0.318)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCsome coll                        -0.380           
    ##                                                                    (0.574)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCsome coll                        -0.321           
    ##                                                                    (0.327)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCassoc deg                         0.244           
    ##                                                                    (0.405)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCassoc deg                         0.194           
    ##                                                                    (0.639)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCassoc deg                        -0.470           
    ##                                                                    (0.380)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCbach deg                          0.413           
    ##                                                                    (0.332)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCbach deg                         -0.716           
    ##                                                                    (0.634)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCbach deg                         -0.222           
    ##                                                                    (0.354)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCadv deg                          0.686**          
    ##                                                                    (0.341)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCadv deg                          -0.540           
    ##                                                                    (0.880)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCadv deg                          -0.315           
    ##                                                                    (0.391)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:MSmarried                            1.352***          
    ##                                                                    (0.520)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:MSmarried                             -0.149           
    ##                                                                    (0.746)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:MSmarried                             -0.096           
    ##                                                                    (0.457)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:MSwidowed                                              
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:MSwidowed                             -0.377           
    ##                                                                    (0.553)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:MSwidowed                              0.166           
    ##                                                                    (0.717)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:MSdivorced                            -0.589*          
    ##                                                                    (0.339)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:MSdivorced                            -0.697           
    ##                                                                    (0.793)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:MSdivorced                            -0.263           
    ##                                                                    (0.465)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:MSseparated                            0.927           
    ##                                                                    (0.592)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:MSseparated                           -0.435           
    ##                                                                    (0.541)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:MSseparated                            0.771           
    ##                                                                    (0.602)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:MSnever                                                
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:MSnever                                                
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:MSnever                                                
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCsome hs:MSmarried                           1.727**          
    ##                                                                    (0.751)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCHS diploma:MSmarried                         0.362           
    ##                                                                    (0.773)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCsome coll:MSmarried                         1.347**          
    ##                                                                    (0.680)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCassoc deg:MSmarried                         1.464*           
    ##                                                                    (0.767)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCbach deg:MSmarried                         0.549***          
    ##                                                                    (0.190)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCadv deg:MSmarried                           0.358*           
    ##                                                                    (0.210)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCsome hs:MSwidowed                            1.373           
    ##                                                                    (0.899)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCHS diploma:MSwidowed                         0.160           
    ##                                                                    (0.898)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCsome coll:MSwidowed                          1.064           
    ##                                                                    (0.819)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCassoc deg:MSwidowed                          1.395           
    ##                                                                    (0.895)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCbach deg:MSwidowed                           0.484           
    ##                                                                    (0.504)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCadv deg:MSwidowed                            0.371           
    ##                                                                    (0.554)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCsome hs:MSdivorced                          1.927**          
    ##                                                                    (0.779)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCHS diploma:MSdivorced                        0.327           
    ##                                                                    (0.791)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCsome coll:MSdivorced                        1.186*           
    ##                                                                    (0.699)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCassoc deg:MSdivorced                        1.372*           
    ##                                                                    (0.786)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCbach deg:MSdivorced                         0.638**          
    ##                                                                    (0.256)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCadv deg:MSdivorced                           0.355           
    ##                                                                    (0.272)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCsome hs:MSseparated                        2.413***          
    ##                                                                    (0.802)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCHS diploma:MSseparated                       0.658           
    ##                                                                    (0.803)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCsome coll:MSseparated                       1.778**          
    ##                                                                    (0.712)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCassoc deg:MSseparated                       1.971**          
    ##                                                                    (0.801)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCbach deg:MSseparated                       0.915***          
    ##                                                                    (0.309)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCadv deg:MSseparated                         0.700*           
    ##                                                                    (0.366)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCsome hs:MSnever                             1.385*           
    ##                                                                    (0.732)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCHS diploma:MSnever                          -0.030           
    ##                                                                    (0.751)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCsome coll:MSnever                            0.976           
    ##                                                                    (0.657)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCassoc deg:MSnever                            1.095           
    ##                                                                    (0.747)          
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCbach deg:MSnever                                             
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:EEDUCadv deg:MSnever                                              
    ##                                                                                     
    ##                                                                                     
    ## RRACEBlack:EEDUCsome hs:MSmarried                                   0.118           
    ##                                                                    (0.295)          
    ##                                                                                     
    ## RRACEAsian:EEDUCsome hs:MSmarried                                  -0.066           
    ##                                                                    (0.461)          
    ##                                                                                     
    ## RRACEOther:EEDUCsome hs:MSmarried                                   0.237           
    ##                                                                    (0.411)          
    ##                                                                                     
    ## RRACEBlack:EEDUCHS diploma:MSmarried                                0.462           
    ##                                                                    (0.536)          
    ##                                                                                     
    ## RRACEAsian:EEDUCHS diploma:MSmarried                               -0.009           
    ##                                                                    (0.441)          
    ##                                                                                     
    ## RRACEOther:EEDUCHS diploma:MSmarried                                0.312           
    ##                                                                    (0.379)          
    ##                                                                                     
    ## RRACEBlack:EEDUCsome coll:MSmarried                                 0.430           
    ##                                                                    (0.611)          
    ##                                                                                     
    ## RRACEAsian:EEDUCsome coll:MSmarried                                -0.074           
    ##                                                                    (0.830)          
    ##                                                                                     
    ## RRACEOther:EEDUCsome coll:MSmarried                                 0.100           
    ##                                                                    (0.378)          
    ##                                                                                     
    ## RRACEBlack:EEDUCassoc deg:MSmarried                                 0.529           
    ##                                                                    (0.850)          
    ##                                                                                     
    ## RRACEAsian:EEDUCassoc deg:MSmarried                                 0.019           
    ##                                                                    (0.446)          
    ##                                                                                     
    ## RRACEOther:EEDUCassoc deg:MSmarried                                -1.038           
    ##                                                                    (0.909)          
    ##                                                                                     
    ## RRACEBlack:EEDUCbach deg:MSmarried                                  0.190           
    ##                                                                    (0.270)          
    ##                                                                                     
    ## RRACEAsian:EEDUCbach deg:MSmarried                                  0.084           
    ##                                                                    (0.437)          
    ##                                                                                     
    ## RRACEOther:EEDUCbach deg:MSmarried                                  0.129           
    ##                                                                    (0.381)          
    ##                                                                                     
    ## RRACEBlack:EEDUCadv deg:MSmarried                                   0.250           
    ##                                                                    (0.273)          
    ##                                                                                     
    ## RRACEAsian:EEDUCadv deg:MSmarried                                  -0.080           
    ##                                                                    (0.444)          
    ##                                                                                     
    ## RRACEOther:EEDUCadv deg:MSmarried                                   0.110           
    ##                                                                    (0.386)          
    ##                                                                                     
    ## RRACEBlack:EEDUCsome hs:MSwidowed                                   0.259           
    ##                                                                    (0.525)          
    ##                                                                                     
    ## RRACEAsian:EEDUCsome hs:MSwidowed                                                   
    ##                                                                                     
    ##                                                                                     
    ## RRACEOther:EEDUCsome hs:MSwidowed                                                   
    ##                                                                                     
    ##                                                                                     
    ## RRACEBlack:EEDUCHS diploma:MSwidowed                                0.367           
    ##                                                                    (0.678)          
    ##                                                                                     
    ## RRACEAsian:EEDUCHS diploma:MSwidowed                               -0.246           
    ##                                                                    (0.662)          
    ##                                                                                     
    ## RRACEOther:EEDUCHS diploma:MSwidowed                               -0.570           
    ##                                                                    (0.454)          
    ##                                                                                     
    ## RRACEBlack:EEDUCsome coll:MSwidowed                                 0.282           
    ##                                                                    (0.737)          
    ##                                                                                     
    ## RRACEAsian:EEDUCsome coll:MSwidowed                                -0.356           
    ##                                                                    (0.954)          
    ##                                                                                     
    ## RRACEOther:EEDUCsome coll:MSwidowed                                -0.731           
    ##                                                                    (0.460)          
    ##                                                                                     
    ## RRACEBlack:EEDUCassoc deg:MSwidowed                                 0.424           
    ##                                                                    (0.947)          
    ##                                                                                     
    ## RRACEAsian:EEDUCassoc deg:MSwidowed                                -0.215           
    ##                                                                    (0.668)          
    ##                                                                                     
    ## RRACEOther:EEDUCassoc deg:MSwidowed                                -1.488           
    ##                                                                    (0.945)          
    ##                                                                                     
    ## RRACEBlack:EEDUCbach deg:MSwidowed                                 -0.060           
    ##                                                                    (0.496)          
    ##                                                                                     
    ## RRACEAsian:EEDUCbach deg:MSwidowed                                 -0.225           
    ##                                                                    (0.662)          
    ##                                                                                     
    ## RRACEOther:EEDUCbach deg:MSwidowed                                 -0.881*          
    ##                                                                    (0.476)          
    ##                                                                                     
    ## RRACEBlack:EEDUCadv deg:MSwidowed                                  -0.148           
    ##                                                                    (0.506)          
    ##                                                                                     
    ## RRACEAsian:EEDUCadv deg:MSwidowed                                  -0.441           
    ##                                                                    (0.649)          
    ##                                                                                     
    ## RRACEOther:EEDUCadv deg:MSwidowed                                  -0.746           
    ##                                                                    (0.481)          
    ##                                                                                     
    ## RRACEBlack:EEDUCsome hs:MSdivorced                                 -0.069           
    ##                                                                    (0.191)          
    ##                                                                                     
    ## RRACEAsian:EEDUCsome hs:MSdivorced                                 -0.123           
    ##                                                                    (0.326)          
    ##                                                                                     
    ## RRACEOther:EEDUCsome hs:MSdivorced                                 -0.288           
    ##                                                                    (0.422)          
    ##                                                                                     
    ## RRACEBlack:EEDUCHS diploma:MSdivorced                               0.319           
    ##                                                                    (0.476)          
    ##                                                                                     
    ## RRACEAsian:EEDUCHS diploma:MSdivorced                              -0.110           
    ##                                                                    (0.260)          
    ##                                                                                     
    ## RRACEOther:EEDUCHS diploma:MSdivorced                              -0.102           
    ##                                                                    (0.356)          
    ##                                                                                     
    ## RRACEBlack:EEDUCsome coll:MSdivorced                                0.142           
    ##                                                                    (0.558)          
    ##                                                                                     
    ## RRACEAsian:EEDUCsome coll:MSdivorced                               -0.294           
    ##                                                                    (0.735)          
    ##                                                                                     
    ## RRACEOther:EEDUCsome coll:MSdivorced                               -0.283           
    ##                                                                    (0.354)          
    ##                                                                                     
    ## RRACEBlack:EEDUCassoc deg:MSdivorced                                0.411           
    ##                                                                    (0.815)          
    ##                                                                                     
    ## RRACEAsian:EEDUCassoc deg:MSdivorced                               -0.103           
    ##                                                                    (0.293)          
    ##                                                                                     
    ## RRACEOther:EEDUCassoc deg:MSdivorced                               -1.421           
    ##                                                                    (0.900)          
    ##                                                                                     
    ## RRACEBlack:EEDUCbach deg:MSdivorced                                -0.057           
    ##                                                                    (0.113)          
    ##                                                                                     
    ## RRACEAsian:EEDUCbach deg:MSdivorced                                 0.001           
    ##                                                                    (0.229)          
    ##                                                                                     
    ## RRACEOther:EEDUCbach deg:MSdivorced                                -0.107           
    ##                                                                    (0.362)          
    ##                                                                                     
    ## RRACEBlack:EEDUCadv deg:MSdivorced                                                  
    ##                                                                                     
    ##                                                                                     
    ## RRACEAsian:EEDUCadv deg:MSdivorced                                                  
    ##                                                                                     
    ##                                                                                     
    ## RRACEOther:EEDUCadv deg:MSdivorced                                 -0.345           
    ##                                                                    (0.366)          
    ##                                                                                     
    ## RRACEBlack:EEDUCsome hs:MSseparated                                 0.547           
    ##                                                                    (0.407)          
    ##                                                                                     
    ## RRACEAsian:EEDUCsome hs:MSseparated                                -0.495           
    ##                                                                    (0.938)          
    ##                                                                                     
    ## RRACEOther:EEDUCsome hs:MSseparated                                -0.032           
    ##                                                                    (0.574)          
    ##                                                                                     
    ## RRACEBlack:EEDUCHS diploma:MSseparated                              0.966           
    ##                                                                    (0.597)          
    ##                                                                                     
    ## RRACEAsian:EEDUCHS diploma:MSseparated                                              
    ##                                                                                     
    ##                                                                                     
    ## RRACEOther:EEDUCHS diploma:MSseparated                              0.491           
    ##                                                                    (0.457)          
    ##                                                                                     
    ## RRACEBlack:EEDUCsome coll:MSseparated                               0.909           
    ##                                                                    (0.663)          
    ##                                                                                     
    ## RRACEAsian:EEDUCsome coll:MSseparated                              -0.749           
    ##                                                                    (0.940)          
    ##                                                                                     
    ## RRACEOther:EEDUCsome coll:MSseparated                               0.159           
    ##                                                                    (0.417)          
    ##                                                                                     
    ## RRACEBlack:EEDUCassoc deg:MSseparated                               1.231           
    ##                                                                    (0.894)          
    ##                                                                                     
    ## RRACEAsian:EEDUCassoc deg:MSseparated                              -0.607           
    ##                                                                    (0.701)          
    ##                                                                                     
    ## RRACEOther:EEDUCassoc deg:MSseparated                              -1.511           
    ##                                                                    (0.958)          
    ##                                                                                     
    ## RRACEBlack:EEDUCbach deg:MSseparated                               0.648*           
    ##                                                                    (0.392)          
    ##                                                                                     
    ## RRACEAsian:EEDUCbach deg:MSseparated                               -0.669           
    ##                                                                    (0.622)          
    ##                                                                                     
    ## RRACEOther:EEDUCbach deg:MSseparated                               -0.260           
    ##                                                                    (0.425)          
    ##                                                                                     
    ## RRACEBlack:EEDUCadv deg:MSseparated                                 0.475           
    ##                                                                    (0.385)          
    ##                                                                                     
    ## RRACEAsian:EEDUCadv deg:MSseparated                                -0.702           
    ##                                                                    (0.618)          
    ##                                                                                     
    ## RRACEOther:EEDUCadv deg:MSseparated                                -0.206           
    ##                                                                    (0.466)          
    ##                                                                                     
    ## RRACEBlack:EEDUCsome hs:MSnever                                                     
    ##                                                                                     
    ##                                                                                     
    ## RRACEAsian:EEDUCsome hs:MSnever                                                     
    ##                                                                                     
    ##                                                                                     
    ## RRACEOther:EEDUCsome hs:MSnever                                                     
    ##                                                                                     
    ##                                                                                     
    ## RRACEBlack:EEDUCHS diploma:MSnever                                  0.386           
    ##                                                                    (0.467)          
    ##                                                                                     
    ## RRACEAsian:EEDUCHS diploma:MSnever                                                  
    ##                                                                                     
    ##                                                                                     
    ## RRACEOther:EEDUCHS diploma:MSnever                                                  
    ##                                                                                     
    ##                                                                                     
    ## RRACEBlack:EEDUCsome coll:MSnever                                   0.366           
    ##                                                                    (0.551)          
    ##                                                                                     
    ## RRACEAsian:EEDUCsome coll:MSnever                                  -0.056           
    ##                                                                    (0.719)          
    ##                                                                                     
    ## RRACEOther:EEDUCsome coll:MSnever                                                   
    ##                                                                                     
    ##                                                                                     
    ## RRACEBlack:EEDUCassoc deg:MSnever                                   0.529           
    ##                                                                    (0.807)          
    ##                                                                                     
    ## RRACEAsian:EEDUCassoc deg:MSnever                                                   
    ##                                                                                     
    ##                                                                                     
    ## RRACEOther:EEDUCassoc deg:MSnever                                  -1.206           
    ##                                                                    (0.823)          
    ##                                                                                     
    ## RRACEBlack:EEDUCbach deg:MSnever                                                    
    ##                                                                                     
    ##                                                                                     
    ## RRACEAsian:EEDUCbach deg:MSnever                                                    
    ##                                                                                     
    ##                                                                                     
    ## RRACEOther:EEDUCbach deg:MSnever                                                    
    ##                                                                                     
    ##                                                                                     
    ## RRACEBlack:EEDUCadv deg:MSnever                                                     
    ##                                                                                     
    ##                                                                                     
    ## RRACEAsian:EEDUCadv deg:MSnever                                                     
    ##                                                                                     
    ##                                                                                     
    ## RRACEOther:EEDUCadv deg:MSnever                                                     
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCsome hs:MSmarried               -1.879***         
    ##                                                                    (0.597)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCsome hs:MSmarried                                 
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCsome hs:MSmarried                 0.211           
    ##                                                                    (0.546)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCHS diploma:MSmarried             -1.034*          
    ##                                                                    (0.554)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCHS diploma:MSmarried              0.443           
    ##                                                                    (0.859)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCHS diploma:MSmarried             -0.161           
    ##                                                                    (0.479)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCsome coll:MSmarried             -1.608***         
    ##                                                                    (0.548)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCsome coll:MSmarried              -0.175           
    ##                                                                    (0.705)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCsome coll:MSmarried               0.198           
    ##                                                                    (0.474)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCassoc deg:MSmarried             -1.235**          
    ##                                                                    (0.612)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCassoc deg:MSmarried              -0.850           
    ##                                                                    (0.894)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCassoc deg:MSmarried               0.263           
    ##                                                                    (0.519)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCbach deg:MSmarried              -1.510***         
    ##                                                                    (0.562)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCbach deg:MSmarried                0.014           
    ##                                                                    (0.859)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCbach deg:MSmarried                0.100           
    ##                                                                    (0.493)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCadv deg:MSmarried               -1.705***         
    ##                                                                    (0.563)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCadv deg:MSmarried                                 
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCadv deg:MSmarried                 0.215           
    ##                                                                    (0.521)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCsome hs:MSwidowed                                 
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCsome hs:MSwidowed                                 
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCsome hs:MSwidowed                                 
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCHS diploma:MSwidowed                              
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCHS diploma:MSwidowed                              
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCHS diploma:MSwidowed              0.122           
    ##                                                                    (0.802)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCsome coll:MSwidowed                               
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCsome coll:MSwidowed                               
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCsome coll:MSwidowed              -0.022           
    ##                                                                    (0.776)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCassoc deg:MSwidowed                               
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCassoc deg:MSwidowed                               
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCassoc deg:MSwidowed                               
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCbach deg:MSwidowed                                
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCbach deg:MSwidowed                                
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCbach deg:MSwidowed               -0.394           
    ##                                                                    (0.871)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCadv deg:MSwidowed                                 
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCadv deg:MSwidowed                                 
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCadv deg:MSwidowed                 0.073           
    ##                                                                    (0.856)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCsome hs:MSdivorced               -0.080           
    ##                                                                    (0.581)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCsome hs:MSdivorced                                
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCsome hs:MSdivorced                0.128           
    ##                                                                    (0.619)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCHS diploma:MSdivorced             0.616           
    ##                                                                    (0.419)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCHS diploma:MSdivorced             0.766           
    ##                                                                    (0.930)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCHS diploma:MSdivorced             0.252           
    ##                                                                    (0.511)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCsome coll:MSdivorced              0.592           
    ##                                                                    (0.402)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCsome coll:MSdivorced              0.696           
    ##                                                                    (0.790)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCsome coll:MSdivorced              0.349           
    ##                                                                    (0.496)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCassoc deg:MSdivorced                              
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCassoc deg:MSdivorced                              
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCassoc deg:MSdivorced              0.452           
    ##                                                                    (0.553)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCbach deg:MSdivorced              1.002**          
    ##                                                                    (0.432)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCbach deg:MSdivorced               0.342           
    ##                                                                    (0.974)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCbach deg:MSdivorced               0.157           
    ##                                                                    (0.531)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCadv deg:MSdivorced                                
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCadv deg:MSdivorced                                
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCadv deg:MSdivorced                0.101           
    ##                                                                    (0.553)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCsome hs:MSseparated                               
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCsome hs:MSseparated                               
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCsome hs:MSseparated              -0.431           
    ##                                                                    (0.831)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCHS diploma:MSseparated           -0.829           
    ##                                                                    (0.732)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCHS diploma:MSseparated                            
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCHS diploma:MSseparated           -0.921           
    ##                                                                    (0.681)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCsome coll:MSseparated            -0.934           
    ##                                                                    (0.657)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCsome coll:MSseparated                             
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCsome coll:MSseparated            -1.079*          
    ##                                                                    (0.650)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCassoc deg:MSseparated            -0.664           
    ##                                                                    (0.735)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCassoc deg:MSseparated                             
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCassoc deg:MSseparated            -0.681           
    ##                                                                    (0.808)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCbach deg:MSseparated             -1.171           
    ##                                                                    (0.753)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCbach deg:MSseparated                              
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCbach deg:MSseparated             -0.587           
    ##                                                                    (0.696)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCadv deg:MSseparated              -0.798           
    ##                                                                    (0.722)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCadv deg:MSseparated                               
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCadv deg:MSseparated              -0.338           
    ##                                                                    (0.756)          
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCsome hs:MSnever                                   
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCsome hs:MSnever                                   
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCsome hs:MSnever                                   
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCHS diploma:MSnever                                
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCHS diploma:MSnever                                
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCHS diploma:MSnever                                
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCsome coll:MSnever                                 
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCsome coll:MSnever                                 
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCsome coll:MSnever                                 
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCassoc deg:MSnever                                 
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCassoc deg:MSnever                                 
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCassoc deg:MSnever                                 
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCbach deg:MSnever                                  
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCbach deg:MSnever                                  
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCbach deg:MSnever                                  
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEBlack:EEDUCadv deg:MSnever                                   
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEAsian:EEDUCadv deg:MSnever                                   
    ##                                                                                     
    ##                                                                                     
    ## RHISPANICHispanic:RRACEOther:EEDUCadv deg:MSnever                                   
    ##                                                                                     
    ##                                                                                     
    ## Constant                                                           -1.180*          
    ##                                                                    (0.634)          
    ##                                                                                     
    ## ------------------------------------------------------------------------------------
    ## Observations                                                       19,429           
    ## R2                                                                  0.035           
    ## Adjusted R2                                                         0.022           
    ## Residual Std. Error                                          0.384 (df = 19161)     
    ## F Statistic                                              2.609*** (df = 267; 19161) 
    ## ====================================================================================
    ## Note:                                                    *p<0.1; **p<0.05; ***p<0.01

``` r
ols_out3a <- lm(MentHealthKids ~ RHISPANIC*RRACE*EEDUC + MS + PRIVHLTH, data = d_kids)
```

``` r
anova(ols_out2a,ols_out3a)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: MentHealthKids ~ RRACE * RHISPANIC
    ## Model 2: MentHealthKids ~ RHISPANIC * RRACE * EEDUC + MS + PRIVHLTH
    ##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
    ## 1  19421 2898.4                                  
    ## 2  19366 2863.6 55    34.746 4.2723 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Practice

``` r
ols_out1a <- lm(MentHealthKids ~ RRACE, data = d_kids)
stargazer(ols_out1a, type = "Text")
```

``` r
p_avg_byrace <- ggplot(d_kids, aes(x = RRACE, fill = INCOME))
p_avg_byrace + geom_bar(position = "fill") +
scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.85)
```

![](Lab7_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
ols_out1a <- lm(MentHealthKids ~ RRACE, data = d_kids)
stargazer(ols_out1a, type = "Text")
```

``` r
p_avg_byrace <- ggplot(d_kids, aes(x = RRACE, fill = EEDUC))
p_avg_byrace + geom_bar(position = "fill") +
scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.85) +
facet_grid(~RHISPANIC)
```

![](Lab7_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
ols_out1a <- lm(MentHealthKids ~ PRIVHLTH, data = d_kids)
stargazer(ols_out1a, type = "Text")
```

``` r
p_avg_byrace <- ggplot(d_kids, aes(x = PRIVHLTH, fill = INCOME))
p_avg_byrace + geom_bar(position = "fill") +
scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.85)
```

![](Lab7_files/figure-gfm/unnamed-chunk-22-1.png)<!-- --> HW This
article is about associations between parental educational attainment
and youth outcomes and role of race/ethnicity. The question researchers
have is whether parental educational fulfillment is associated with the
similar amount of health benefits for a variety of ethnic groups. This
study had 10,619 youth ranging from age 12 to 17 years who were
participants from a wave 1 of the population assessment of tobacco and
health study. Variables that were measured were aggression, tobacco
dependence, psychological distress and chronic medical conditions.
Findings were that parental educational fulfillment was associated with
fewer health benefits for hispanics as opposed to non hispanic
counterparts. These results demonstrate that closing the gap in health
disparities between ethnic groups goes further simplifying balancing
socioeconomic resources. This article states the importance of mental
health in children. These two dependents are important for my group
project because we want to find how race, social anxiety, and social
behavior impact children and the aid they receive. Mental health in
children can impact their relationship, education, physical health,
chronic disease, and health behaviors. Public health surveillance of
children’s mental health plays an important role in monitoring trends,
informing prevention, identifying differences, intervention efforts and
informing prevention. According to the data, mental disorders often
start in early childhood and affect children in many aspects of their
life. Assari, S., Caldwell, C. H., & Bazargan, M. (2019). Association
Between Parental Educational Attainment and Youth Outcomes and Role of
Race/Ethnicity. JAMA network open, 2(11), e1916018.
<https://doi.org/10.1001/jamanetworkopen.2019.16018> Bitsko, R. H.,
Claussen, A. H., Lichstein, J., Black, L. I., Jones, S. E., Danielson,
M. L., Hoenig, J. M., Davis Jack, S. P., Brody, D. J., Gyawali, S.,
Maenner, M. J., Warner, M., Holland, K. M., Perou, R., Crosby, A. E.,
Blumberg, S. J., Avenevoli, S., Kaminski, J. W., Ghandour, R. M., &
Contributor (2022). Mental Health Surveillance Among Children - United
States, 2013-2019. MMWR supplements, 71(2), 1–42.
<https://doi.org/10.15585/mmwr.su7102a1>
