
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ramber

<!-- badges: start -->
<!-- badges: end -->

The goal of ramber alllows access to AMBER database, which is a
comprehensive European wide database for barriers including dams, weirs,
culverts, and fords along river networks. The database has a citizen
science option the barrier tracker, which allows users to capture and
records barriers. These are submitted to AMBER through a mobile app and
then the correctness evalutated by AMBER experts.

## Installation

You can install the development version of ramber like so:

``` r
# install.packages("devtools")

if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

remotes::install_github("AnthonyBasooma/ramber")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ramber)
## basic example code

getdams <- get_barrieratlas(type = 'dam', country = "AUSTRIA")

head(getdams)
#>                                          GUID BasinName
#> 282484 {D5A1218F-9DFE-4507-85AD-4AF1FD5B8E65}    DANUBE
#> 322866 {4742BCC5-36BC-4593-B6EF-AAAAF773C098}    DANUBE
#> 392707 {8CE2B446-96A3-4D58-8A29-5979602ECECB}    DANUBE
#> 392830 {C9EBB9BD-F175-4445-8343-5E783AA4B0C8}    DANUBE
#> 392832 {D1C2EC0A-44DB-4255-8CE1-5FB09E7FE160}    DANUBE
#> 392853 {78261215-1833-497E-91E2-A05C319E3076}    DANUBE
#>                               RiverName                        DBName
#> 282484                          ILZBACH AMBER_AUSTRIA_Stream_barriers
#> 322866                         KOTHBACH AMBER_AUSTRIA_Stream_barriers
#> 392707          KAUTZENBACH -  REUTBACH AMBER_AUSTRIA_Stream_barriers
#> 392830  THAYA -  DEUTSCHE THAYA -  DYJE AMBER_AUSTRIA_Stream_barriers
#> 392832                            THAYA            AMBER_Danube_ICPDR
#> 392853 MORAVSKA DYJE -  MAHRISCHE THAYA AMBER_AUSTRIA_Stream_barriers
#>        Longitude_WGS84 Latitude_WGS84 LabelAtlas Country       ZHYD     Outlet
#> 282484        15.90932       47.08102        DAM AUSTRIA F030003222 FSO0000001
#> 322866        14.85511       47.08100        DAM AUSTRIA F030003246 FSO0000001
#> 392707        15.23361       48.93541        DAM AUSTRIA F020003461 FSO0000001
#> 392830        15.32637       48.91365        DAM AUSTRIA F020003292 FSO0000001
#> 392832        15.32640       48.91360        DAM AUSTRIA F020003292 FSO0000001
#> 392853        15.47387       48.91199        DAM AUSTRIA F020002836 FSO0000001
#>          HClass Height type   basin_id
#> 282484     <0.5   0.25  dam F030003327
#> 322866 0.5 to 1   1.00  dam F030002673
#> 392707     <NA>     NA  dam F020003454
#> 392830 0.5 to 1   1.00  dam F020003454
#> 392832 0.5 to 1   1.00  dam F020003454
#> 392853   2 to 5   2.70  dam F020003454
```
