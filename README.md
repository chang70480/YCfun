Download and start
------------------

``` r
#Download from github
library(devtools)
install_github("chang70480/YCfun")
#wait fot installing

library(YCfun)

#check other packages have been installed.
YCstart()
```

Cheat sheet
-----------

<img src="inst/pk_test/cheat_sheet.png" width="100%" />

Read data
---------

``` r
DT <- makeDT_lab(path = system.file("extdata/tscs151.sav",package = "YCfun"))
## re-encoding from CP950  
  
DT$v6a
```

    ##  [1]  8 19 20  8  8 19  4  4 19  8 11  4  8 19  4  6  6 10  6  8 20  3  4
    ## [24] 20  8  4 20 20 11 10 20 19 11 18 19 19  8 20 19 19  3 19  8 19  4 19
    ## [47]  8 12 11  3
    ##  [ reached getOption("max.print") -- omitted 1984 entries ]
    ## attr(,"value.labels")
    ##                  遺漏值                    拒答                  不知道 
    ##                    "99"                    "98"                    "97" 
    ##                    跳答                    其他           博士(續答6a2) 
    ##                    "96"                    "22"                    "21" 
    ##           碩士(續答6a2)           大學(續答6a2) 技術學院、科大(續答6a2) 
    ##                    "20"                    "19"                    "18" 
    ##         軍警官學校/大學                空中大學            空中行(商)專 
    ##                    "17"                    "16"                    "15" 
    ##              軍警專科班              軍警專修班                    三專 
    ##                    "14"                    "13"                    "12" 
    ##                    二專                    五專                士官學校 
    ##                    "11"                    "10"                     "9" 
    ##           高職(續答6a1)       綜合高中(續答6a1)           高中(續答6a1) 
    ##                     "8"                     "7"                     "6" 
    ##                    初職                國(初)中                    小學 
    ##                     "5"                     "4"                     "3" 
    ## 自修(識字、私塾)(跳答7)       無(不識字)(跳答7) 
    ##                     "2"                     "1" 
    ## attr(,"variable.labels")
    ## [1] "6a.請問您的教育程度是?"
    ## attr(,"table")
    ## 
    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
    ##   95   16  289  251    6   86   16  428    4   57  130   18    3    8    4 
    ##   16   17   18   19   20   21   22   97 <NA> 
    ##    5    7  207  271  111   17    3    2    0

Variables table
---------------

``` r
DT_var <- makeDT_var(DT)
DT_var[20:30,]
```

    ## # A tibble: 11 x 2
    ##    題號    內容                                             
    ##    <chr>   <chr>                                            
    ##  1 v1      1.性別                                           
    ##  2 v2y     2.請問您是什麼時候出生的(以身分證上的為主)?民國年
    ##  3 v2m     2.請問您是什麼時候出生的(以身分證上的為主)?月    
    ##  4 v2a     2.請問您大約幾歲?                                
    ##  5 v3city  3.請問您出生在什麼地方?省/縣/市                  
    ##  6 kv3city 3.請問您出生在什麼地方?其他                      
    ##  7 v3zip   3.請問您出生在什麼地方?鄉/鎮/市/區               
    ##  8 v4      4.請問您父親是哪裡人?                            
    ##  9 kv4     4.請問您父親是哪裡人?其他                        
    ## 10 v5      5.請問您母親是哪裡人?                            
    ## 11 kv5     5.請問您母親是哪裡人?其他

Recoding
--------

This function is based on sjmisc package.   rec\_new can recode in four datatypes, including numeric, factor, character, logical.

### numeric to numeric

``` r
rec_new(DT$v6a,"1,2=1;3=2;4,5=3;6,7,8,9=4;10:15=5;16:19=6;20=7;21=8;97=NA;else=copy")
```

    ##  [1] 4 6 7 4 4 6 3 3 6 4 5 3 4 6 3 4 4 5 4 4 7 2 3 7 4 3 7 7 5 5 7 6 5 6 6
    ## [36] 6 4 7 6 6 2 6 4 6 3 6 4 5 5 2
    ##  [ reached getOption("max.print") -- omitted 1984 entries ]
    ## attr(,"variable.labels")
    ## [1] "6a.請問您的教育程度是?"
    ## attr(,"old.table")
    ## 
    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
    ##   95   16  289  251    6   86   16  428    4   57  130   18    3    8    4 
    ##   16   17   18   19   20   21   22   97 <NA> 
    ##    5    7  207  271  111   17    3    2    0 
    ## attr(,"new_labels")
    ## [1] "1,2=1;3=2;4,5=3;6,7,8,9=4;10:15=5;16:19=6;20=7;21=8;97=NA;else=copy   class:numeric"
    ## attr(,"table")
    ## temp.vec
    ##    1    2    3    4    5    6    7    8   22 <NA> 
    ##  111  289  257  534  220  490  111   17    3    2

### numeric to factor, and can set reference group=4

``` r
rec_new(DT$v6a,"1,2=1;3=2;4,5=3;6,7,8,9=4;10:15=5;16:19=6;20=7;21=8;else=NA",'f',ref = 4)
```

    ##  [1] 4 6 7 4 4 6 3 3 6 4 5 3 4 6 3 4 4 5 4 4 7 2 3 7 4 3 7 7 5 5 7 6 5 6 6
    ## [36] 6 4 7 6 6 2 6 4 6 3 6 4 5 5 2
    ##  [ reached getOption("max.print") -- omitted 1984 entries ]
    ## attr(,"variable.labels")
    ## [1] 6a.請問您的教育程度是?
    ## attr(,"old.table")
    ## 
    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
    ##   95   16  289  251    6   86   16  428    4   57  130   18    3    8    4 
    ##   16   17   18   19   20   21   22   97 <NA> 
    ##    5    7  207  271  111   17    3    2    0 
    ## attr(,"new_labels")
    ## [1] 1,2=1;3=2;4,5=3;6,7,8,9=4;10:15=5;16:19=6;20=7;21=8;else=NA   class:factor
    ## attr(,"table")
    ## temp.vec
    ##    4    1    2    3    5    6    7    8 <NA> 
    ##  534  111  289  257  220  490  111   17    5 
    ## Levels: 4 1 2 3 5 6 7 8

``` r
rec_new(DT$v6a,"1,2=no;3:5=low;6,7,8,9=mid;10:21=high;else=NA",'f',ref = "mid")
```

    ##  [1] mid  high high mid  mid  high low  low  high mid  high low  mid  high
    ## [15] low  mid  mid  high mid  mid  high low  low  high mid  low  high high
    ## [29] high high high high high high high high mid  high high high low  high
    ## [43] mid  high low  high mid  high high low 
    ##  [ reached getOption("max.print") -- omitted 1984 entries ]
    ## attr(,"variable.labels")
    ## [1] 6a.請問您的教育程度是?
    ## attr(,"old.table")
    ## 
    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
    ##   95   16  289  251    6   86   16  428    4   57  130   18    3    8    4 
    ##   16   17   18   19   20   21   22   97 <NA> 
    ##    5    7  207  271  111   17    3    2    0 
    ## attr(,"new_labels")
    ## [1] 1,2=no;3:5=low;6,7,8,9=mid;10:21=high;else=NA   class:factor
    ## attr(,"table")
    ## temp.vec
    ##  mid high  low   no <NA> 
    ##  534  838  546  111    5 
    ## Levels: mid high low no

### numeric to character

``` r
DT$neweduc <- rec_new(DT$v6a,"1,2=no;3:5=low;6,7,8,9=mid;10:21=high;else=NA",'c')
DT$neweduc
```

    ##  [1] "mid"  "high" "high" "mid"  "mid"  "high" "low"  "low"  "high" "mid" 
    ## [11] "high" "low"  "mid"  "high" "low"  "mid"  "mid"  "high" "mid"  "mid" 
    ## [21] "high" "low"  "low"  "high" "mid"  "low"  "high" "high" "high" "high"
    ## [31] "high" "high" "high" "high" "high" "high" "mid"  "high" "high" "high"
    ## [41] "low"  "high" "mid"  "high" "low"  "high" "mid"  "high" "high" "low" 
    ##  [ reached getOption("max.print") -- omitted 1984 entries ]
    ## attr(,"variable.labels")
    ## [1] "6a.請問您的教育程度是?"
    ## attr(,"old.table")
    ## 
    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
    ##   95   16  289  251    6   86   16  428    4   57  130   18    3    8    4 
    ##   16   17   18   19   20   21   22   97 <NA> 
    ##    5    7  207  271  111   17    3    2    0 
    ## attr(,"new_labels")
    ## [1] "1,2=no;3:5=low;6,7,8,9=mid;10:21=high;else=NA   class:character"
    ## attr(,"table")
    ## temp.vec
    ## high  low  mid   no <NA> 
    ##  838  546  534  111    5

### character to numeric

``` r
DT$newedun <- rec_new(DT$neweduc,"no=1;low=2;mid=3;high=4")
DT$newedun
```

    ##  [1] 3 4 4 3 3 4 2 2 4 3 4 2 3 4 2 3 3 4 3 3 4 2 2 4 3 2 4 4 4 4 4 4 4 4 4
    ## [36] 4 3 4 4 4 2 4 3 4 2 4 3 4 4 2
    ##  [ reached getOption("max.print") -- omitted 1984 entries ]
    ## attr(,"variable.labels")
    ## [1] "6a.請問您的教育程度是?"
    ## attr(,"old.table")
    ## temp.vec
    ## high  low  mid   no <NA> 
    ##  838  546  534  111    5 
    ## attr(,"new_labels")
    ## [1] "no=1;low=2;mid=3;high=4   class:numeric"
    ## attr(,"table")
    ## temp.vec
    ##    1    2    3    4 <NA> 
    ##  111  546  534  838    5
