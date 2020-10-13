Code
================
Helena
13/10/2020

# Let’s get started

First load the necessary packages (these contain the functions we will
use) and the data. (Don’t laugh at the file path - I forgot R Studio
automatically creates a folder and by the time I remembered I couldn’t
correct it).

``` r
library(dslabs)
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.0
    ## ✓ tidyr   1.1.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ─────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(ggplot2)
library(zoo)
```

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(ggthemes)

cases <- read.csv("~/Desktop/uk_covid19_2/uk_covid19_2/2020-10-10/data_2020-Oct-09.csv")
```

What’s the sum of new cases over the past week and the sum of cases over
the week before?

``` r
sum(cases[(1:7),5])
```

    ## [1] 108535

``` r
sum(cases[(8:14),5])
```

    ## [1] 43912

That’s all I was doing with the uk data, I just use their charts which
is a cop out really.

# More importantly - Wales

I do hope you will be impressed with this section though. First we load
the data from Public Health Wales. Then tell the computer to treat
‘Specimen.date’ as a date.

``` r
w_cases <- read.csv("~/Desktop/uk_covid19_2/uk_covid19_2/2020-10-10/Rapid COVID-19 surveillance data(1).csv", header=TRUE)
w_cases$Specimen.date <- 
  as.Date(w_cases$Specimen.date, format = "%d/%m/%Y")
```

Next pull out the data for Anglesey, Gwynedd and Conwy and put them in
their own tables.

``` r
a_cases <- filter(w_cases, Local.Authority == "Isle of Anglesey")
a_cases
```

    ##      Local.Authority Specimen.date Cases..new. Cumulative.cases
    ## 1   Isle of Anglesey    2020-10-08           0              541
    ## 2   Isle of Anglesey    2020-10-07           3              541
    ## 3   Isle of Anglesey    2020-10-06           3              538
    ## 4   Isle of Anglesey    2020-10-05           1              535
    ## 5   Isle of Anglesey    2020-10-04           3              534
    ## 6   Isle of Anglesey    2020-10-03           3              531
    ## 7   Isle of Anglesey    2020-10-02           4              528
    ## 8   Isle of Anglesey    2020-10-01           4              524
    ## 9   Isle of Anglesey    2020-09-30           0              520
    ## 10  Isle of Anglesey    2020-09-29           0              520
    ## 11  Isle of Anglesey    2020-09-28           3              520
    ## 12  Isle of Anglesey    2020-09-27           0              517
    ## 13  Isle of Anglesey    2020-09-26           1              517
    ## 14  Isle of Anglesey    2020-09-25           2              516
    ## 15  Isle of Anglesey    2020-09-24           1              514
    ## 16  Isle of Anglesey    2020-09-23           3              513
    ## 17  Isle of Anglesey    2020-09-22           0              510
    ## 18  Isle of Anglesey    2020-09-21           0              510
    ## 19  Isle of Anglesey    2020-09-20           4              510
    ## 20  Isle of Anglesey    2020-09-19           4              506
    ## 21  Isle of Anglesey    2020-09-18           4              502
    ## 22  Isle of Anglesey    2020-09-17           2              498
    ## 23  Isle of Anglesey    2020-09-16           5              496
    ## 24  Isle of Anglesey    2020-09-15           2              491
    ## 25  Isle of Anglesey    2020-09-14           4              489
    ## 26  Isle of Anglesey    2020-09-13           1              485
    ## 27  Isle of Anglesey    2020-09-12           1              484
    ## 28  Isle of Anglesey    2020-09-11           0              483
    ## 29  Isle of Anglesey    2020-09-10           4              483
    ## 30  Isle of Anglesey    2020-09-09           1              479
    ## 31  Isle of Anglesey    2020-09-08           5              478
    ## 32  Isle of Anglesey    2020-09-07           3              473
    ## 33  Isle of Anglesey    2020-09-06           0              470
    ## 34  Isle of Anglesey    2020-09-05           0              470
    ## 35  Isle of Anglesey    2020-09-04           1              470
    ## 36  Isle of Anglesey    2020-09-03           1              469
    ## 37  Isle of Anglesey    2020-09-02           0              468
    ## 38  Isle of Anglesey    2020-09-01           0              468
    ## 39  Isle of Anglesey    2020-08-31           0              468
    ## 40  Isle of Anglesey    2020-08-30           0              468
    ## 41  Isle of Anglesey    2020-08-29           0              468
    ## 42  Isle of Anglesey    2020-08-28           1              468
    ## 43  Isle of Anglesey    2020-08-27           0              467
    ## 44  Isle of Anglesey    2020-08-26           0              467
    ## 45  Isle of Anglesey    2020-08-25           0              467
    ## 46  Isle of Anglesey    2020-08-24           0              467
    ## 47  Isle of Anglesey    2020-08-23           0              467
    ## 48  Isle of Anglesey    2020-08-22           0              467
    ## 49  Isle of Anglesey    2020-08-21           1              467
    ## 50  Isle of Anglesey    2020-08-20           0              466
    ## 51  Isle of Anglesey    2020-08-19           0              466
    ## 52  Isle of Anglesey    2020-08-18           1              466
    ## 53  Isle of Anglesey    2020-08-17           0              465
    ## 54  Isle of Anglesey    2020-08-16           0              465
    ## 55  Isle of Anglesey    2020-08-15           0              465
    ## 56  Isle of Anglesey    2020-08-14           3              465
    ## 57  Isle of Anglesey    2020-08-13           1              462
    ## 58  Isle of Anglesey    2020-08-12           0              461
    ## 59  Isle of Anglesey    2020-08-11           0              461
    ## 60  Isle of Anglesey    2020-08-10           0              461
    ## 61  Isle of Anglesey    2020-08-09           0              461
    ## 62  Isle of Anglesey    2020-08-08           0              461
    ## 63  Isle of Anglesey    2020-08-07           0              461
    ## 64  Isle of Anglesey    2020-08-06           0              461
    ## 65  Isle of Anglesey    2020-08-05           1              461
    ## 66  Isle of Anglesey    2020-08-04           0              460
    ## 67  Isle of Anglesey    2020-08-03           0              460
    ## 68  Isle of Anglesey    2020-08-02           0              460
    ## 69  Isle of Anglesey    2020-08-01           0              460
    ## 70  Isle of Anglesey    2020-07-31           0              460
    ## 71  Isle of Anglesey    2020-07-30           1              460
    ## 72  Isle of Anglesey    2020-07-29           0              459
    ## 73  Isle of Anglesey    2020-07-28           1              459
    ## 74  Isle of Anglesey    2020-07-27           1              458
    ## 75  Isle of Anglesey    2020-07-26           0              457
    ## 76  Isle of Anglesey    2020-07-25           0              457
    ## 77  Isle of Anglesey    2020-07-24           1              457
    ## 78  Isle of Anglesey    2020-07-23           0              456
    ## 79  Isle of Anglesey    2020-07-22           1              456
    ## 80  Isle of Anglesey    2020-07-21           0              455
    ## 81  Isle of Anglesey    2020-07-20           0              455
    ## 82  Isle of Anglesey    2020-07-19           0              455
    ## 83  Isle of Anglesey    2020-07-18           1              455
    ## 84  Isle of Anglesey    2020-07-17           0              454
    ## 85  Isle of Anglesey    2020-07-16           0              454
    ## 86  Isle of Anglesey    2020-07-15           0              454
    ## 87  Isle of Anglesey    2020-07-14           1              454
    ## 88  Isle of Anglesey    2020-07-13           1              453
    ## 89  Isle of Anglesey    2020-07-12           1              452
    ## 90  Isle of Anglesey    2020-07-11           2              451
    ## 91  Isle of Anglesey    2020-07-10           0              449
    ## 92  Isle of Anglesey    2020-07-09           0              449
    ## 93  Isle of Anglesey    2020-07-08           0              449
    ## 94  Isle of Anglesey    2020-07-07           1              449
    ## 95  Isle of Anglesey    2020-07-06           1              448
    ## 96  Isle of Anglesey    2020-07-05           0              447
    ## 97  Isle of Anglesey    2020-07-04           0              447
    ## 98  Isle of Anglesey    2020-07-03           1              447
    ## 99  Isle of Anglesey    2020-07-02           1              446
    ## 100 Isle of Anglesey    2020-07-01           1              445
    ## 101 Isle of Anglesey    2020-06-30           1              444
    ## 102 Isle of Anglesey    2020-06-29           1              443
    ## 103 Isle of Anglesey    2020-06-28           0              442
    ## 104 Isle of Anglesey    2020-06-27           0              442
    ## 105 Isle of Anglesey    2020-06-26           1              442
    ## 106 Isle of Anglesey    2020-06-25           1              441
    ## 107 Isle of Anglesey    2020-06-24           3              440
    ## 108 Isle of Anglesey    2020-06-23           9              437
    ## 109 Isle of Anglesey    2020-06-22           5              428
    ## 110 Isle of Anglesey    2020-06-21           1              423
    ## 111 Isle of Anglesey    2020-06-20           7              422
    ## 112 Isle of Anglesey    2020-06-19          54              415
    ## 113 Isle of Anglesey    2020-06-18           6              361
    ## 114 Isle of Anglesey    2020-06-17          12              355
    ## 115 Isle of Anglesey    2020-06-16          12              343
    ## 116 Isle of Anglesey    2020-06-15          14              331
    ## 117 Isle of Anglesey    2020-06-14           8              317
    ## 118 Isle of Anglesey    2020-06-13          10              309
    ## 119 Isle of Anglesey    2020-06-12          19              299
    ## 120 Isle of Anglesey    2020-06-11           6              280
    ## 121 Isle of Anglesey    2020-06-10          10              274
    ## 122 Isle of Anglesey    2020-06-09           6              264
    ## 123 Isle of Anglesey    2020-06-08           3              258
    ## 124 Isle of Anglesey    2020-06-07           5              255
    ## 125 Isle of Anglesey    2020-06-06           5              250
    ## 126 Isle of Anglesey    2020-06-05           1              245
    ## 127 Isle of Anglesey    2020-06-04           1              244
    ## 128 Isle of Anglesey    2020-06-03           3              243
    ## 129 Isle of Anglesey    2020-06-02           1              240
    ## 130 Isle of Anglesey    2020-06-01           0              239
    ## 131 Isle of Anglesey    2020-05-31           2              239
    ## 132 Isle of Anglesey    2020-05-30           4              237
    ## 133 Isle of Anglesey    2020-05-29           7              233
    ## 134 Isle of Anglesey    2020-05-28           6              226
    ## 135 Isle of Anglesey    2020-05-27           2              220
    ## 136 Isle of Anglesey    2020-05-26           7              218
    ## 137 Isle of Anglesey    2020-05-25           3              211
    ## 138 Isle of Anglesey    2020-05-24           1              208
    ## 139 Isle of Anglesey    2020-05-23           5              207
    ## 140 Isle of Anglesey    2020-05-22           3              202
    ## 141 Isle of Anglesey    2020-05-21           4              199
    ## 142 Isle of Anglesey    2020-05-20           5              195
    ## 143 Isle of Anglesey    2020-05-19           6              190
    ## 144 Isle of Anglesey    2020-05-18           3              184
    ## 145 Isle of Anglesey    2020-05-17           6              181
    ## 146 Isle of Anglesey    2020-05-16           5              175
    ## 147 Isle of Anglesey    2020-05-15          15              170
    ## 148 Isle of Anglesey    2020-05-14           3              155
    ## 149 Isle of Anglesey    2020-05-13           5              152
    ## 150 Isle of Anglesey    2020-05-12           3              147
    ## 151 Isle of Anglesey    2020-05-11           7              144
    ## 152 Isle of Anglesey    2020-05-10           5              137
    ## 153 Isle of Anglesey    2020-05-09           1              132
    ## 154 Isle of Anglesey    2020-05-08           2              131
    ## 155 Isle of Anglesey    2020-05-07           7              129
    ## 156 Isle of Anglesey    2020-05-06          11              122
    ## 157 Isle of Anglesey    2020-05-05           6              111
    ## 158 Isle of Anglesey    2020-05-04           7              105
    ## 159 Isle of Anglesey    2020-05-03           1               98
    ## 160 Isle of Anglesey    2020-05-02           1               97
    ## 161 Isle of Anglesey    2020-05-01           3               96
    ## 162 Isle of Anglesey    2020-04-30           4               93
    ## 163 Isle of Anglesey    2020-04-29           4               89
    ## 164 Isle of Anglesey    2020-04-28           3               85
    ## 165 Isle of Anglesey    2020-04-27           1               82
    ## 166 Isle of Anglesey    2020-04-26           2               81
    ## 167 Isle of Anglesey    2020-04-25           4               79
    ## 168 Isle of Anglesey    2020-04-24           4               75
    ## 169 Isle of Anglesey    2020-04-23           5               71
    ## 170 Isle of Anglesey    2020-04-22           6               66
    ## 171 Isle of Anglesey    2020-04-21           2               60
    ## 172 Isle of Anglesey    2020-04-20           3               58
    ## 173 Isle of Anglesey    2020-04-19           4               55
    ## 174 Isle of Anglesey    2020-04-18           2               51
    ## 175 Isle of Anglesey    2020-04-17           4               49
    ## 176 Isle of Anglesey    2020-04-16           1               45
    ## 177 Isle of Anglesey    2020-04-15           4               44
    ## 178 Isle of Anglesey    2020-04-14           4               40
    ## 179 Isle of Anglesey    2020-04-13           4               36
    ## 180 Isle of Anglesey    2020-04-12           2               32
    ## 181 Isle of Anglesey    2020-04-11           3               30
    ## 182 Isle of Anglesey    2020-04-10           0               27
    ## 183 Isle of Anglesey    2020-04-09           5               27
    ## 184 Isle of Anglesey    2020-04-08           2               22
    ## 185 Isle of Anglesey    2020-04-07           2               20
    ## 186 Isle of Anglesey    2020-04-06           3               18
    ## 187 Isle of Anglesey    2020-04-05           0               15
    ## 188 Isle of Anglesey    2020-04-04           2               15
    ## 189 Isle of Anglesey    2020-04-03           1               13
    ## 190 Isle of Anglesey    2020-04-02           1               12
    ## 191 Isle of Anglesey    2020-04-01           0               11
    ## 192 Isle of Anglesey    2020-03-31           3               11
    ## 193 Isle of Anglesey    2020-03-30           6                8
    ## 194 Isle of Anglesey    2020-03-29           0                2
    ## 195 Isle of Anglesey    2020-03-28           0                2
    ## 196 Isle of Anglesey    2020-03-27           0                2
    ## 197 Isle of Anglesey    2020-03-26           0                2
    ## 198 Isle of Anglesey    2020-03-25           0                2
    ## 199 Isle of Anglesey    2020-03-24           0                2
    ## 200 Isle of Anglesey    2020-03-23           0                2
    ## 201 Isle of Anglesey    2020-03-22           0                2
    ## 202 Isle of Anglesey    2020-03-21           0                2
    ## 203 Isle of Anglesey    2020-03-20           0                2
    ## 204 Isle of Anglesey    2020-03-19           0                2
    ## 205 Isle of Anglesey    2020-03-18           0                2
    ## 206 Isle of Anglesey    2020-03-17           0                2
    ## 207 Isle of Anglesey    2020-03-16           0                2
    ## 208 Isle of Anglesey    2020-03-15           0                2
    ## 209 Isle of Anglesey    2020-03-14           1                2
    ## 210 Isle of Anglesey    2020-03-13           0                1
    ## 211 Isle of Anglesey    2020-03-12           0                1
    ## 212 Isle of Anglesey    2020-03-11           1                1
    ## 213 Isle of Anglesey    2020-03-10           0                0
    ## 214 Isle of Anglesey    2020-03-09           0                0
    ## 215 Isle of Anglesey    2020-03-08           0                0
    ## 216 Isle of Anglesey    2020-03-07           0                0
    ## 217 Isle of Anglesey    2020-03-06           0                0
    ## 218 Isle of Anglesey    2020-03-05           0                0
    ## 219 Isle of Anglesey    2020-03-04           0                0
    ## 220 Isle of Anglesey    2020-03-03           0                0
    ## 221 Isle of Anglesey    2020-03-02           0                0
    ## 222 Isle of Anglesey    2020-03-01           0                0
    ## 223 Isle of Anglesey    2020-02-29           0                0
    ## 224 Isle of Anglesey    2020-02-28           0                0
    ## 225 Isle of Anglesey    2020-02-27           0                0
    ## 226 Isle of Anglesey    2020-02-26           0                0
    ## 227 Isle of Anglesey    2020-02-25           0                0
    ## 228 Isle of Anglesey    2020-02-24           0                0
    ## 229 Isle of Anglesey    2020-02-23           0                0
    ## 230 Isle of Anglesey    2020-02-22           0                0
    ## 231 Isle of Anglesey    2020-02-21           0                0
    ## 232 Isle of Anglesey    2020-02-20           0                0
    ## 233 Isle of Anglesey    2020-02-19           0                0
    ## 234 Isle of Anglesey    2020-02-18           0                0
    ## 235 Isle of Anglesey    2020-02-17           0                0
    ## 236 Isle of Anglesey    2020-02-16           0                0
    ## 237 Isle of Anglesey    2020-02-15           0                0
    ## 238 Isle of Anglesey    2020-02-14           0                0
    ## 239 Isle of Anglesey    2020-02-13           0                0
    ## 240 Isle of Anglesey    2020-02-12           0                0
    ## 241 Isle of Anglesey    2020-02-11           0                0
    ## 242 Isle of Anglesey    2020-02-10           0                0
    ## 243 Isle of Anglesey    2020-02-09           0                0
    ## 244 Isle of Anglesey    2020-02-08           0                0
    ## 245 Isle of Anglesey    2020-02-07           0                0
    ## 246 Isle of Anglesey    2020-02-06           0                0
    ## 247 Isle of Anglesey    2020-02-05           0                0
    ## 248 Isle of Anglesey    2020-02-04           0                0
    ## 249 Isle of Anglesey    2020-02-03           0                0
    ##     Cumulative.incidence.per.100.000.population Testing.episodes..new.
    ## 1                                         772.4                      1
    ## 2                                         772.4                     26
    ## 3                                         768.1                     69
    ## 4                                         763.8                    101
    ## 5                                         762.4                     50
    ## 6                                         758.1                     64
    ## 7                                         753.8                    112
    ## 8                                         748.1                    139
    ## 9                                         742.4                    134
    ## 10                                        742.4                    109
    ## 11                                        742.4                    128
    ## 12                                        738.1                     51
    ## 13                                        738.1                     62
    ## 14                                        736.7                     99
    ## 15                                        733.8                     98
    ## 16                                        732.4                    100
    ## 17                                        728.1                    133
    ## 18                                        728.1                    154
    ## 19                                        728.1                     70
    ## 20                                        722.4                    116
    ## 21                                        716.7                    119
    ## 22                                        711.0                    119
    ## 23                                        708.1                    170
    ## 24                                        701.0                    145
    ## 25                                        698.1                    147
    ## 26                                        692.4                     82
    ## 27                                        691.0                     80
    ## 28                                        689.6                    123
    ## 29                                        689.6                    136
    ## 30                                        683.9                    132
    ## 31                                        682.4                    153
    ## 32                                        675.3                    115
    ## 33                                        671.0                     57
    ## 34                                        671.0                     53
    ## 35                                        671.0                     83
    ## 36                                        669.6                     72
    ## 37                                        668.2                    138
    ## 38                                        668.2                    108
    ## 39                                        668.2                     60
    ## 40                                        668.2                     43
    ## 41                                        668.2                     42
    ## 42                                        668.2                     68
    ## 43                                        666.7                    133
    ## 44                                        666.7                    141
    ## 45                                        666.7                    100
    ## 46                                        666.7                     61
    ## 47                                        666.7                     32
    ## 48                                        666.7                     37
    ## 49                                        666.7                     75
    ## 50                                        665.3                     69
    ## 51                                        665.3                    136
    ## 52                                        665.3                     79
    ## 53                                        663.9                    104
    ## 54                                        663.9                     35
    ## 55                                        663.9                     27
    ## 56                                        663.9                     66
    ## 57                                        659.6                     50
    ## 58                                        658.2                     64
    ## 59                                        658.2                     49
    ## 60                                        658.2                     69
    ## 61                                        658.2                     41
    ## 62                                        658.2                     27
    ## 63                                        658.2                     61
    ## 64                                        658.2                     85
    ## 65                                        658.2                     77
    ## 66                                        656.7                     68
    ## 67                                        656.7                     76
    ## 68                                        656.7                     32
    ## 69                                        656.7                     41
    ## 70                                        656.7                     58
    ## 71                                        656.7                     50
    ## 72                                        655.3                     66
    ## 73                                        655.3                     47
    ## 74                                        653.9                    109
    ## 75                                        652.5                     44
    ## 76                                        652.5                     42
    ## 77                                        652.5                     68
    ## 78                                        651.0                     46
    ## 79                                        651.0                     87
    ## 80                                        649.6                     66
    ## 81                                        649.6                     79
    ## 82                                        649.6                     37
    ## 83                                        649.6                     37
    ## 84                                        648.2                     43
    ## 85                                        648.2                    126
    ## 86                                        648.2                    105
    ## 87                                        648.2                    117
    ## 88                                        646.7                     63
    ## 89                                        645.3                     78
    ## 90                                        643.9                     59
    ## 91                                        641.0                     54
    ## 92                                        641.0                     98
    ## 93                                        641.0                     76
    ## 94                                        641.0                     81
    ## 95                                        639.6                     59
    ## 96                                        638.2                     43
    ## 97                                        638.2                     18
    ## 98                                        638.2                     38
    ## 99                                        636.8                     91
    ## 100                                       635.3                     83
    ## 101                                       633.9                     69
    ## 102                                       632.5                     76
    ## 103                                       631.0                     39
    ## 104                                       631.0                     33
    ## 105                                       631.0                     76
    ## 106                                       629.6                     71
    ## 107                                       628.2                    107
    ## 108                                       623.9                    174
    ## 109                                       611.1                    109
    ## 110                                       603.9                     86
    ## 111                                       602.5                     93
    ## 112                                       592.5                    255
    ## 113                                       515.4                     79
    ## 114                                       506.8                     89
    ## 115                                       489.7                    128
    ## 116                                       472.6                     75
    ## 117                                       452.6                     70
    ## 118                                       441.2                     64
    ## 119                                       426.9                    121
    ## 120                                       399.8                     63
    ## 121                                       391.2                     61
    ## 122                                       376.9                     69
    ## 123                                       368.3                     52
    ## 124                                       364.1                     51
    ## 125                                       356.9                     69
    ## 126                                       349.8                     85
    ## 127                                       348.4                     80
    ## 128                                       346.9                     59
    ## 129                                       342.6                     89
    ## 130                                       341.2                    161
    ## 131                                       341.2                     21
    ## 132                                       338.4                    101
    ## 133                                       332.7                     95
    ## 134                                       322.7                     77
    ## 135                                       314.1                    154
    ## 136                                       311.2                    171
    ## 137                                       301.2                     88
    ## 138                                       297.0                     63
    ## 139                                       295.5                     65
    ## 140                                       288.4                    111
    ## 141                                       284.1                     78
    ## 142                                       278.4                     53
    ## 143                                       271.3                     19
    ## 144                                       262.7                     27
    ## 145                                       258.4                     59
    ## 146                                       249.8                     57
    ## 147                                       242.7                     61
    ## 148                                       221.3                     24
    ## 149                                       217.0                     29
    ## 150                                       209.9                     27
    ## 151                                       205.6                     26
    ## 152                                       195.6                     20
    ## 153                                       188.5                     11
    ## 154                                       187.0                     15
    ## 155                                       184.2                     49
    ## 156                                       174.2                     58
    ## 157                                       158.5                     39
    ## 158                                       149.9                     44
    ## 159                                       139.9                     11
    ## 160                                       138.5                     21
    ## 161                                       137.1                     29
    ## 162                                       132.8                     29
    ## 163                                       127.1                     29
    ## 164                                       121.4                     20
    ## 165                                       117.1                     17
    ## 166                                       115.6                      9
    ## 167                                       112.8                     13
    ## 168                                       107.1                      8
    ## 169                                       101.4                     21
    ## 170                                        94.2                     18
    ## 171                                        85.7                      7
    ## 172                                        82.8                     16
    ## 173                                        78.5                      5
    ## 174                                        72.8                      9
    ## 175                                        70.0                     17
    ## 176                                        64.2                     12
    ## 177                                        62.8                      9
    ## 178                                        57.1                      7
    ## 179                                        51.4                      7
    ## 180                                        45.7                      6
    ## 181                                        42.8                      5
    ## 182                                        38.5                      0
    ## 183                                        38.5                     13
    ## 184                                        31.4                      9
    ## 185                                        28.6                      8
    ## 186                                        25.7                      9
    ## 187                                        21.4                      3
    ## 188                                        21.4                      3
    ## 189                                        18.6                      6
    ## 190                                        17.1                      4
    ## 191                                        15.7                      2
    ## 192                                        15.7                      8
    ## 193                                        11.4                     12
    ## 194                                         2.9                      1
    ## 195                                         2.9                      4
    ## 196                                         2.9                      4
    ## 197                                         2.9                      5
    ## 198                                         2.9                      4
    ## 199                                         2.9                      2
    ## 200                                         2.9                      4
    ## 201                                         2.9                      2
    ## 202                                         2.9                      3
    ## 203                                         2.9                      3
    ## 204                                         2.9                      1
    ## 205                                         2.9                      1
    ## 206                                         2.9                      5
    ## 207                                         2.9                      3
    ## 208                                         2.9                      2
    ## 209                                         2.9                      6
    ## 210                                         1.4                      3
    ## 211                                         1.4                      1
    ## 212                                         1.4                      3
    ## 213                                         0.0                      0
    ## 214                                         0.0                      0
    ## 215                                         0.0                      0
    ## 216                                         0.0                      0
    ## 217                                         0.0                      0
    ## 218                                         0.0                      0
    ## 219                                         0.0                      0
    ## 220                                         0.0                      0
    ## 221                                         0.0                      1
    ## 222                                         0.0                      0
    ## 223                                         0.0                      1
    ## 224                                         0.0                      2
    ## 225                                         0.0                     10
    ## 226                                         0.0                      0
    ## 227                                         0.0                      0
    ## 228                                         0.0                      0
    ## 229                                         0.0                      0
    ## 230                                         0.0                      0
    ## 231                                         0.0                      0
    ## 232                                         0.0                      0
    ## 233                                         0.0                      0
    ## 234                                         0.0                      0
    ## 235                                         0.0                      0
    ## 236                                         0.0                      0
    ## 237                                         0.0                      2
    ## 238                                         0.0                      0
    ## 239                                         0.0                      0
    ## 240                                         0.0                      0
    ## 241                                         0.0                      0
    ## 242                                         0.0                      0
    ## 243                                         0.0                      0
    ## 244                                         0.0                      0
    ## 245                                         0.0                      0
    ## 246                                         0.0                      0
    ## 247                                         0.0                      0
    ## 248                                         0.0                      0
    ## 249                                         0.0                      0
    ##     Cumulative.testing.episodes
    ## 1                         12734
    ## 2                         12733
    ## 3                         12707
    ## 4                         12638
    ## 5                         12537
    ## 6                         12487
    ## 7                         12423
    ## 8                         12311
    ## 9                         12172
    ## 10                        12038
    ## 11                        11929
    ## 12                        11801
    ## 13                        11750
    ## 14                        11688
    ## 15                        11589
    ## 16                        11491
    ## 17                        11391
    ## 18                        11258
    ## 19                        11104
    ## 20                        11034
    ## 21                        10918
    ## 22                        10799
    ## 23                        10680
    ## 24                        10510
    ## 25                        10365
    ## 26                        10218
    ## 27                        10136
    ## 28                        10056
    ## 29                         9933
    ## 30                         9797
    ## 31                         9665
    ## 32                         9512
    ## 33                         9397
    ## 34                         9340
    ## 35                         9287
    ## 36                         9204
    ## 37                         9132
    ## 38                         8994
    ## 39                         8886
    ## 40                         8826
    ## 41                         8783
    ## 42                         8741
    ## 43                         8673
    ## 44                         8540
    ## 45                         8399
    ## 46                         8299
    ## 47                         8238
    ## 48                         8206
    ## 49                         8169
    ## 50                         8094
    ## 51                         8025
    ## 52                         7889
    ## 53                         7810
    ## 54                         7706
    ## 55                         7671
    ## 56                         7644
    ## 57                         7578
    ## 58                         7528
    ## 59                         7464
    ## 60                         7415
    ## 61                         7346
    ## 62                         7305
    ## 63                         7278
    ## 64                         7217
    ## 65                         7132
    ## 66                         7055
    ## 67                         6987
    ## 68                         6911
    ## 69                         6879
    ## 70                         6838
    ## 71                         6780
    ## 72                         6730
    ## 73                         6664
    ## 74                         6617
    ## 75                         6508
    ## 76                         6464
    ## 77                         6422
    ## 78                         6354
    ## 79                         6308
    ## 80                         6221
    ## 81                         6155
    ## 82                         6076
    ## 83                         6039
    ## 84                         6002
    ## 85                         5959
    ## 86                         5833
    ## 87                         5728
    ## 88                         5611
    ## 89                         5548
    ## 90                         5470
    ## 91                         5411
    ## 92                         5357
    ## 93                         5259
    ## 94                         5183
    ## 95                         5102
    ## 96                         5043
    ## 97                         5000
    ## 98                         4982
    ## 99                         4944
    ## 100                        4853
    ## 101                        4770
    ## 102                        4701
    ## 103                        4625
    ## 104                        4586
    ## 105                        4553
    ## 106                        4477
    ## 107                        4406
    ## 108                        4299
    ## 109                        4125
    ## 110                        4016
    ## 111                        3930
    ## 112                        3837
    ## 113                        3582
    ## 114                        3503
    ## 115                        3414
    ## 116                        3286
    ## 117                        3211
    ## 118                        3141
    ## 119                        3077
    ## 120                        2956
    ## 121                        2893
    ## 122                        2832
    ## 123                        2763
    ## 124                        2711
    ## 125                        2660
    ## 126                        2591
    ## 127                        2506
    ## 128                        2426
    ## 129                        2367
    ## 130                        2278
    ## 131                        2117
    ## 132                        2096
    ## 133                        1995
    ## 134                        1900
    ## 135                        1823
    ## 136                        1669
    ## 137                        1498
    ## 138                        1410
    ## 139                        1347
    ## 140                        1282
    ## 141                        1171
    ## 142                        1093
    ## 143                        1040
    ## 144                        1021
    ## 145                         994
    ## 146                         935
    ## 147                         878
    ## 148                         817
    ## 149                         793
    ## 150                         764
    ## 151                         737
    ## 152                         711
    ## 153                         691
    ## 154                         680
    ## 155                         665
    ## 156                         616
    ## 157                         558
    ## 158                         519
    ## 159                         475
    ## 160                         464
    ## 161                         443
    ## 162                         414
    ## 163                         385
    ## 164                         356
    ## 165                         336
    ## 166                         319
    ## 167                         310
    ## 168                         297
    ## 169                         289
    ## 170                         268
    ## 171                         250
    ## 172                         243
    ## 173                         227
    ## 174                         222
    ## 175                         213
    ## 176                         196
    ## 177                         184
    ## 178                         175
    ## 179                         168
    ## 180                         161
    ## 181                         155
    ## 182                         150
    ## 183                         150
    ## 184                         137
    ## 185                         128
    ## 186                         120
    ## 187                         111
    ## 188                         108
    ## 189                         105
    ## 190                          99
    ## 191                          95
    ## 192                          93
    ## 193                          85
    ## 194                          73
    ## 195                          72
    ## 196                          68
    ## 197                          64
    ## 198                          59
    ## 199                          55
    ## 200                          53
    ## 201                          49
    ## 202                          47
    ## 203                          44
    ## 204                          41
    ## 205                          40
    ## 206                          39
    ## 207                          34
    ## 208                          31
    ## 209                          29
    ## 210                          23
    ## 211                          20
    ## 212                          19
    ## 213                          16
    ## 214                          16
    ## 215                          16
    ## 216                          16
    ## 217                          16
    ## 218                          16
    ## 219                          16
    ## 220                          16
    ## 221                          16
    ## 222                          15
    ## 223                          15
    ## 224                          14
    ## 225                          12
    ## 226                           2
    ## 227                           2
    ## 228                           2
    ## 229                           2
    ## 230                           2
    ## 231                           2
    ## 232                           2
    ## 233                           2
    ## 234                           2
    ## 235                           2
    ## 236                           2
    ## 237                           2
    ## 238                           0
    ## 239                           0
    ## 240                           0
    ## 241                           0
    ## 242                           0
    ## 243                           0
    ## 244                           0
    ## 245                           0
    ## 246                           0
    ## 247                           0
    ## 248                           0
    ## 249                           0

``` r
g_cases <- filter(w_cases, Local.Authority == "Gwynedd")
g_cases
```

    ##     Local.Authority Specimen.date Cases..new. Cumulative.cases
    ## 1           Gwynedd    2020-10-08           1              833
    ## 2           Gwynedd    2020-10-07          15              832
    ## 3           Gwynedd    2020-10-06          11              817
    ## 4           Gwynedd    2020-10-05          16              806
    ## 5           Gwynedd    2020-10-04           7              790
    ## 6           Gwynedd    2020-10-03          18              783
    ## 7           Gwynedd    2020-10-02          16              765
    ## 8           Gwynedd    2020-10-01          27              749
    ## 9           Gwynedd    2020-09-30          16              722
    ## 10          Gwynedd    2020-09-29          11              706
    ## 11          Gwynedd    2020-09-28           4              695
    ## 12          Gwynedd    2020-09-27           2              691
    ## 13          Gwynedd    2020-09-26           1              689
    ## 14          Gwynedd    2020-09-25           3              688
    ## 15          Gwynedd    2020-09-24           5              685
    ## 16          Gwynedd    2020-09-23           2              680
    ## 17          Gwynedd    2020-09-22           3              678
    ## 18          Gwynedd    2020-09-21           3              675
    ## 19          Gwynedd    2020-09-20           0              672
    ## 20          Gwynedd    2020-09-19           1              672
    ## 21          Gwynedd    2020-09-18           1              671
    ## 22          Gwynedd    2020-09-17           4              670
    ## 23          Gwynedd    2020-09-16           2              666
    ## 24          Gwynedd    2020-09-15           3              664
    ## 25          Gwynedd    2020-09-14           1              661
    ## 26          Gwynedd    2020-09-13           1              660
    ## 27          Gwynedd    2020-09-12           2              659
    ## 28          Gwynedd    2020-09-11           4              657
    ## 29          Gwynedd    2020-09-10           4              653
    ## 30          Gwynedd    2020-09-09           0              649
    ## 31          Gwynedd    2020-09-08           8              649
    ## 32          Gwynedd    2020-09-07           3              641
    ## 33          Gwynedd    2020-09-06           2              638
    ## 34          Gwynedd    2020-09-05           2              636
    ## 35          Gwynedd    2020-09-04           2              634
    ## 36          Gwynedd    2020-09-03           2              632
    ## 37          Gwynedd    2020-09-02           0              630
    ## 38          Gwynedd    2020-09-01           0              630
    ## 39          Gwynedd    2020-08-31           4              630
    ## 40          Gwynedd    2020-08-30           2              626
    ## 41          Gwynedd    2020-08-29           0              624
    ## 42          Gwynedd    2020-08-28           0              624
    ## 43          Gwynedd    2020-08-27           3              624
    ## 44          Gwynedd    2020-08-26           3              621
    ## 45          Gwynedd    2020-08-25           0              618
    ## 46          Gwynedd    2020-08-24           0              618
    ## 47          Gwynedd    2020-08-23           0              618
    ## 48          Gwynedd    2020-08-22           0              618
    ## 49          Gwynedd    2020-08-21           3              618
    ## 50          Gwynedd    2020-08-20           1              615
    ## 51          Gwynedd    2020-08-19           5              614
    ## 52          Gwynedd    2020-08-18           2              609
    ## 53          Gwynedd    2020-08-17           0              607
    ## 54          Gwynedd    2020-08-16           1              607
    ## 55          Gwynedd    2020-08-15           0              606
    ## 56          Gwynedd    2020-08-14           1              606
    ## 57          Gwynedd    2020-08-13           1              605
    ## 58          Gwynedd    2020-08-12           1              604
    ## 59          Gwynedd    2020-08-11           4              603
    ## 60          Gwynedd    2020-08-10           0              599
    ## 61          Gwynedd    2020-08-09           0              599
    ## 62          Gwynedd    2020-08-08           2              599
    ## 63          Gwynedd    2020-08-07           0              597
    ## 64          Gwynedd    2020-08-06           1              597
    ## 65          Gwynedd    2020-08-05           1              596
    ## 66          Gwynedd    2020-08-04           1              595
    ## 67          Gwynedd    2020-08-03           0              594
    ## 68          Gwynedd    2020-08-02           0              594
    ## 69          Gwynedd    2020-08-01           1              594
    ## 70          Gwynedd    2020-07-31           0              593
    ## 71          Gwynedd    2020-07-30           0              593
    ## 72          Gwynedd    2020-07-29           1              593
    ## 73          Gwynedd    2020-07-28           0              592
    ## 74          Gwynedd    2020-07-27           0              592
    ## 75          Gwynedd    2020-07-26           0              592
    ## 76          Gwynedd    2020-07-25           2              592
    ## 77          Gwynedd    2020-07-24           0              590
    ## 78          Gwynedd    2020-07-23           1              590
    ## 79          Gwynedd    2020-07-22           0              589
    ## 80          Gwynedd    2020-07-21           1              589
    ## 81          Gwynedd    2020-07-20           0              588
    ## 82          Gwynedd    2020-07-19           0              588
    ## 83          Gwynedd    2020-07-18           0              588
    ## 84          Gwynedd    2020-07-17           0              588
    ## 85          Gwynedd    2020-07-16           1              588
    ## 86          Gwynedd    2020-07-15           2              587
    ## 87          Gwynedd    2020-07-14           0              585
    ## 88          Gwynedd    2020-07-13           0              585
    ## 89          Gwynedd    2020-07-12           2              585
    ## 90          Gwynedd    2020-07-11           1              583
    ## 91          Gwynedd    2020-07-10           6              582
    ## 92          Gwynedd    2020-07-09           2              576
    ## 93          Gwynedd    2020-07-08           0              574
    ## 94          Gwynedd    2020-07-07           1              574
    ## 95          Gwynedd    2020-07-06           1              573
    ## 96          Gwynedd    2020-07-05           0              572
    ## 97          Gwynedd    2020-07-04           0              572
    ## 98          Gwynedd    2020-07-03           0              572
    ## 99          Gwynedd    2020-07-02           1              572
    ## 100         Gwynedd    2020-07-01           1              571
    ## 101         Gwynedd    2020-06-30           0              570
    ## 102         Gwynedd    2020-06-29           3              570
    ## 103         Gwynedd    2020-06-28           0              567
    ## 104         Gwynedd    2020-06-27           1              567
    ## 105         Gwynedd    2020-06-26           2              566
    ## 106         Gwynedd    2020-06-25           3              564
    ## 107         Gwynedd    2020-06-24           2              561
    ## 108         Gwynedd    2020-06-23           7              559
    ## 109         Gwynedd    2020-06-22           4              552
    ## 110         Gwynedd    2020-06-21           3              548
    ## 111         Gwynedd    2020-06-20           6              545
    ## 112         Gwynedd    2020-06-19          27              539
    ## 113         Gwynedd    2020-06-18           6              512
    ## 114         Gwynedd    2020-06-17           4              506
    ## 115         Gwynedd    2020-06-16           0              502
    ## 116         Gwynedd    2020-06-15           3              502
    ## 117         Gwynedd    2020-06-14           3              499
    ## 118         Gwynedd    2020-06-13           4              496
    ## 119         Gwynedd    2020-06-12           1              492
    ## 120         Gwynedd    2020-06-11           3              491
    ## 121         Gwynedd    2020-06-10           5              488
    ## 122         Gwynedd    2020-06-09           4              483
    ## 123         Gwynedd    2020-06-08           3              479
    ## 124         Gwynedd    2020-06-07           1              476
    ## 125         Gwynedd    2020-06-06           4              475
    ## 126         Gwynedd    2020-06-05           7              471
    ## 127         Gwynedd    2020-06-04           2              464
    ## 128         Gwynedd    2020-06-03           5              462
    ## 129         Gwynedd    2020-06-02           3              457
    ## 130         Gwynedd    2020-06-01           7              454
    ## 131         Gwynedd    2020-05-31           1              447
    ## 132         Gwynedd    2020-05-30           9              446
    ## 133         Gwynedd    2020-05-29           6              437
    ## 134         Gwynedd    2020-05-28          13              431
    ## 135         Gwynedd    2020-05-27           5              418
    ## 136         Gwynedd    2020-05-26           8              413
    ## 137         Gwynedd    2020-05-25           3              405
    ## 138         Gwynedd    2020-05-24           0              402
    ## 139         Gwynedd    2020-05-23           9              402
    ## 140         Gwynedd    2020-05-22           3              393
    ## 141         Gwynedd    2020-05-21           5              390
    ## 142         Gwynedd    2020-05-20           5              385
    ## 143         Gwynedd    2020-05-19           3              380
    ## 144         Gwynedd    2020-05-18           9              377
    ## 145         Gwynedd    2020-05-17          13              368
    ## 146         Gwynedd    2020-05-16          10              355
    ## 147         Gwynedd    2020-05-15           9              345
    ## 148         Gwynedd    2020-05-14           7              336
    ## 149         Gwynedd    2020-05-13           7              329
    ## 150         Gwynedd    2020-05-12           2              322
    ## 151         Gwynedd    2020-05-11           5              320
    ## 152         Gwynedd    2020-05-10           5              315
    ## 153         Gwynedd    2020-05-09           5              310
    ## 154         Gwynedd    2020-05-08           7              305
    ## 155         Gwynedd    2020-05-07          10              298
    ## 156         Gwynedd    2020-05-06          15              288
    ## 157         Gwynedd    2020-05-05          11              273
    ## 158         Gwynedd    2020-05-04           9              262
    ## 159         Gwynedd    2020-05-03           0              253
    ## 160         Gwynedd    2020-05-02           3              253
    ## 161         Gwynedd    2020-05-01           4              250
    ## 162         Gwynedd    2020-04-30           5              246
    ## 163         Gwynedd    2020-04-29           5              241
    ## 164         Gwynedd    2020-04-28           5              236
    ## 165         Gwynedd    2020-04-27           7              231
    ## 166         Gwynedd    2020-04-26           9              224
    ## 167         Gwynedd    2020-04-25           6              215
    ## 168         Gwynedd    2020-04-24           8              209
    ## 169         Gwynedd    2020-04-23           9              201
    ## 170         Gwynedd    2020-04-22           6              192
    ## 171         Gwynedd    2020-04-21           6              186
    ## 172         Gwynedd    2020-04-20          10              180
    ## 173         Gwynedd    2020-04-19           6              170
    ## 174         Gwynedd    2020-04-18          17              164
    ## 175         Gwynedd    2020-04-17           7              147
    ## 176         Gwynedd    2020-04-16          12              140
    ## 177         Gwynedd    2020-04-15           6              128
    ## 178         Gwynedd    2020-04-14          12              122
    ## 179         Gwynedd    2020-04-13          27              110
    ## 180         Gwynedd    2020-04-12           7               83
    ## 181         Gwynedd    2020-04-11           9               76
    ## 182         Gwynedd    2020-04-10           6               67
    ## 183         Gwynedd    2020-04-09          14               61
    ## 184         Gwynedd    2020-04-08           5               47
    ## 185         Gwynedd    2020-04-07           7               42
    ## 186         Gwynedd    2020-04-06           6               35
    ## 187         Gwynedd    2020-04-05           0               29
    ## 188         Gwynedd    2020-04-04           4               29
    ## 189         Gwynedd    2020-04-03           6               25
    ## 190         Gwynedd    2020-04-02           1               19
    ## 191         Gwynedd    2020-04-01           0               18
    ## 192         Gwynedd    2020-03-31           2               18
    ## 193         Gwynedd    2020-03-30           1               16
    ## 194         Gwynedd    2020-03-29           1               15
    ## 195         Gwynedd    2020-03-28           3               14
    ## 196         Gwynedd    2020-03-27           1               11
    ## 197         Gwynedd    2020-03-26           2               10
    ## 198         Gwynedd    2020-03-25           2                8
    ## 199         Gwynedd    2020-03-24           2                6
    ## 200         Gwynedd    2020-03-23           0                4
    ## 201         Gwynedd    2020-03-22           0                4
    ## 202         Gwynedd    2020-03-21           0                4
    ## 203         Gwynedd    2020-03-20           0                4
    ## 204         Gwynedd    2020-03-19           1                4
    ## 205         Gwynedd    2020-03-18           2                3
    ## 206         Gwynedd    2020-03-17           0                1
    ## 207         Gwynedd    2020-03-16           1                1
    ## 208         Gwynedd    2020-03-15           0                0
    ## 209         Gwynedd    2020-03-14           0                0
    ## 210         Gwynedd    2020-03-13           0                0
    ## 211         Gwynedd    2020-03-12           0                0
    ## 212         Gwynedd    2020-03-11           0                0
    ## 213         Gwynedd    2020-03-10           0                0
    ## 214         Gwynedd    2020-03-09           0                0
    ## 215         Gwynedd    2020-03-08           0                0
    ## 216         Gwynedd    2020-03-07           0                0
    ## 217         Gwynedd    2020-03-06           0                0
    ## 218         Gwynedd    2020-03-05           0                0
    ## 219         Gwynedd    2020-03-04           0                0
    ## 220         Gwynedd    2020-03-03           0                0
    ## 221         Gwynedd    2020-03-02           0                0
    ## 222         Gwynedd    2020-03-01           0                0
    ## 223         Gwynedd    2020-02-29           0                0
    ## 224         Gwynedd    2020-02-28           0                0
    ## 225         Gwynedd    2020-02-27           0                0
    ## 226         Gwynedd    2020-02-26           0                0
    ## 227         Gwynedd    2020-02-25           0                0
    ## 228         Gwynedd    2020-02-24           0                0
    ## 229         Gwynedd    2020-02-23           0                0
    ## 230         Gwynedd    2020-02-22           0                0
    ## 231         Gwynedd    2020-02-21           0                0
    ## 232         Gwynedd    2020-02-20           0                0
    ## 233         Gwynedd    2020-02-19           0                0
    ## 234         Gwynedd    2020-02-18           0                0
    ## 235         Gwynedd    2020-02-17           0                0
    ## 236         Gwynedd    2020-02-16           0                0
    ## 237         Gwynedd    2020-02-15           0                0
    ## 238         Gwynedd    2020-02-14           0                0
    ## 239         Gwynedd    2020-02-13           0                0
    ## 240         Gwynedd    2020-02-12           0                0
    ## 241         Gwynedd    2020-02-11           0                0
    ## 242         Gwynedd    2020-02-10           0                0
    ## 243         Gwynedd    2020-02-09           0                0
    ## 244         Gwynedd    2020-02-08           0                0
    ## 245         Gwynedd    2020-02-07           0                0
    ## 246         Gwynedd    2020-02-06           0                0
    ## 247         Gwynedd    2020-02-05           0                0
    ## 248         Gwynedd    2020-02-04           0                0
    ## 249         Gwynedd    2020-02-03           0                0
    ##     Cumulative.incidence.per.100.000.population Testing.episodes..new.
    ## 1                                         668.8                      3
    ## 2                                         668.0                     79
    ## 3                                         655.9                    166
    ## 4                                         647.1                    174
    ## 5                                         634.2                    119
    ## 6                                         628.6                    144
    ## 7                                         614.2                    274
    ## 8                                         601.3                    319
    ## 9                                         579.6                    190
    ## 10                                        566.8                    195
    ## 11                                        558.0                    238
    ## 12                                        554.8                     93
    ## 13                                        553.1                     92
    ## 14                                        552.3                    163
    ## 15                                        549.9                    216
    ## 16                                        545.9                    199
    ## 17                                        544.3                    148
    ## 18                                        541.9                    188
    ## 19                                        539.5                    112
    ## 20                                        539.5                    101
    ## 21                                        538.7                    183
    ## 22                                        537.9                    241
    ## 23                                        534.7                    218
    ## 24                                        533.1                    232
    ## 25                                        530.7                    256
    ## 26                                        529.9                    166
    ## 27                                        529.1                    115
    ## 28                                        527.5                    225
    ## 29                                        524.2                    263
    ## 30                                        521.0                    201
    ## 31                                        521.0                    202
    ## 32                                        514.6                    174
    ## 33                                        512.2                     81
    ## 34                                        510.6                     78
    ## 35                                        509.0                    164
    ## 36                                        507.4                    212
    ## 37                                        505.8                    158
    ## 38                                        505.8                    209
    ## 39                                        505.8                    128
    ## 40                                        502.6                     71
    ## 41                                        501.0                     85
    ## 42                                        501.0                    137
    ## 43                                        501.0                    309
    ## 44                                        498.6                    177
    ## 45                                        496.1                    127
    ## 46                                        496.1                    120
    ## 47                                        496.1                     57
    ## 48                                        496.1                     68
    ## 49                                        496.1                    119
    ## 50                                        493.7                    214
    ## 51                                        492.9                    199
    ## 52                                        488.9                    138
    ## 53                                        487.3                    209
    ## 54                                        487.3                     72
    ## 55                                        486.5                     88
    ## 56                                        486.5                    142
    ## 57                                        485.7                    106
    ## 58                                        484.9                    151
    ## 59                                        484.1                    146
    ## 60                                        480.9                    179
    ## 61                                        480.9                     41
    ## 62                                        480.9                     69
    ## 63                                        479.3                    100
    ## 64                                        479.3                    130
    ## 65                                        478.5                    166
    ## 66                                        477.7                    116
    ## 67                                        476.9                    116
    ## 68                                        476.9                     60
    ## 69                                        476.9                     52
    ## 70                                        476.1                     92
    ## 71                                        476.1                    117
    ## 72                                        476.1                     98
    ## 73                                        475.3                    103
    ## 74                                        475.3                    174
    ## 75                                        475.3                     67
    ## 76                                        475.3                     61
    ## 77                                        473.7                    123
    ## 78                                        473.7                    102
    ## 79                                        472.9                    194
    ## 80                                        472.9                    163
    ## 81                                        472.1                    152
    ## 82                                        472.1                     68
    ## 83                                        472.1                     51
    ## 84                                        472.1                     90
    ## 85                                        472.1                    117
    ## 86                                        471.3                    126
    ## 87                                        469.7                    107
    ## 88                                        469.7                    122
    ## 89                                        469.7                    126
    ## 90                                        468.0                    107
    ## 91                                        467.2                    101
    ## 92                                        462.4                    231
    ## 93                                        460.8                    150
    ## 94                                        460.8                    117
    ## 95                                        460.0                    129
    ## 96                                        459.2                     37
    ## 97                                        459.2                     48
    ## 98                                        459.2                    144
    ## 99                                        459.2                    139
    ## 100                                       458.4                    101
    ## 101                                       457.6                    102
    ## 102                                       457.6                     92
    ## 103                                       455.2                     47
    ## 104                                       455.2                     40
    ## 105                                       454.4                     90
    ## 106                                       452.8                     92
    ## 107                                       450.4                    105
    ## 108                                       448.8                    111
    ## 109                                       443.2                    141
    ## 110                                       439.9                     67
    ## 111                                       437.5                    109
    ## 112                                       432.7                    151
    ## 113                                       411.0                     83
    ## 114                                       406.2                     70
    ## 115                                       403.0                    120
    ## 116                                       403.0                    156
    ## 117                                       400.6                     58
    ## 118                                       398.2                     62
    ## 119                                       395.0                     93
    ## 120                                       394.2                     70
    ## 121                                       391.8                    120
    ## 122                                       387.8                    119
    ## 123                                       384.6                    115
    ## 124                                       382.1                     72
    ## 125                                       381.3                     84
    ## 126                                       378.1                    208
    ## 127                                       372.5                    133
    ## 128                                       370.9                    240
    ## 129                                       366.9                    147
    ## 130                                       364.5                    217
    ## 131                                       358.9                     46
    ## 132                                       358.1                    160
    ## 133                                       350.8                    202
    ## 134                                       346.0                    215
    ## 135                                       335.6                    141
    ## 136                                       331.6                     97
    ## 137                                       325.1                     77
    ## 138                                       322.7                     85
    ## 139                                       322.7                    133
    ## 140                                       315.5                     49
    ## 141                                       313.1                    203
    ## 142                                       309.1                    175
    ## 143                                       305.1                    114
    ## 144                                       302.7                     73
    ## 145                                       295.4                    125
    ## 146                                       285.0                    128
    ## 147                                       277.0                    104
    ## 148                                       269.7                     39
    ## 149                                       264.1                     35
    ## 150                                       258.5                     40
    ## 151                                       256.9                     47
    ## 152                                       252.9                     47
    ## 153                                       248.9                     28
    ## 154                                       244.9                     48
    ## 155                                       239.2                     92
    ## 156                                       231.2                    122
    ## 157                                       219.2                     59
    ## 158                                       210.3                     41
    ## 159                                       203.1                     25
    ## 160                                       203.1                     33
    ## 161                                       200.7                     54
    ## 162                                       197.5                     49
    ## 163                                       193.5                     42
    ## 164                                       189.5                     29
    ## 165                                       185.5                     26
    ## 166                                       179.8                     20
    ## 167                                       172.6                     17
    ## 168                                       167.8                     41
    ## 169                                       161.4                     32
    ## 170                                       154.1                     32
    ## 171                                       149.3                     21
    ## 172                                       144.5                     23
    ## 173                                       136.5                     17
    ## 174                                       131.7                     26
    ## 175                                       118.0                     20
    ## 176                                       112.4                     21
    ## 177                                       102.8                     18
    ## 178                                        97.9                     32
    ## 179                                        88.3                     37
    ## 180                                        66.6                     16
    ## 181                                        61.0                     19
    ## 182                                        53.8                     16
    ## 183                                        49.0                     26
    ## 184                                        37.7                     12
    ## 185                                        33.7                     15
    ## 186                                        28.1                     17
    ## 187                                        23.3                      1
    ## 188                                        23.3                     12
    ## 189                                        20.1                     22
    ## 190                                        15.3                     13
    ## 191                                        14.5                      6
    ## 192                                        14.5                     11
    ## 193                                        12.8                     14
    ## 194                                        12.0                      7
    ## 195                                        11.2                      9
    ## 196                                         8.8                      6
    ## 197                                         8.0                      6
    ## 198                                         6.4                      9
    ## 199                                         4.8                     10
    ## 200                                         3.2                      3
    ## 201                                         3.2                      4
    ## 202                                         3.2                      6
    ## 203                                         3.2                      6
    ## 204                                         3.2                      4
    ## 205                                         2.4                     10
    ## 206                                         0.8                     10
    ## 207                                         0.8                      1
    ## 208                                         0.0                      2
    ## 209                                         0.0                      4
    ## 210                                         0.0                      1
    ## 211                                         0.0                      5
    ## 212                                         0.0                      1
    ## 213                                         0.0                      1
    ## 214                                         0.0                      0
    ## 215                                         0.0                      0
    ## 216                                         0.0                      0
    ## 217                                         0.0                      0
    ## 218                                         0.0                      2
    ## 219                                         0.0                      2
    ## 220                                         0.0                      1
    ## 221                                         0.0                      0
    ## 222                                         0.0                      0
    ## 223                                         0.0                      2
    ## 224                                         0.0                      3
    ## 225                                         0.0                      7
    ## 226                                         0.0                      1
    ## 227                                         0.0                      0
    ## 228                                         0.0                      0
    ## 229                                         0.0                      0
    ## 230                                         0.0                      1
    ## 231                                         0.0                      0
    ## 232                                         0.0                      0
    ## 233                                         0.0                      0
    ## 234                                         0.0                      0
    ## 235                                         0.0                      0
    ## 236                                         0.0                      0
    ## 237                                         0.0                      1
    ## 238                                         0.0                      0
    ## 239                                         0.0                      0
    ## 240                                         0.0                      0
    ## 241                                         0.0                      0
    ## 242                                         0.0                      0
    ## 243                                         0.0                      0
    ## 244                                         0.0                      0
    ## 245                                         0.0                      0
    ## 246                                         0.0                      0
    ## 247                                         0.0                      0
    ## 248                                         0.0                      0
    ## 249                                         0.0                      0
    ##     Cumulative.testing.episodes
    ## 1                         21019
    ## 2                         21016
    ## 3                         20937
    ## 4                         20771
    ## 5                         20597
    ## 6                         20478
    ## 7                         20334
    ## 8                         20060
    ## 9                         19741
    ## 10                        19551
    ## 11                        19356
    ## 12                        19118
    ## 13                        19025
    ## 14                        18933
    ## 15                        18770
    ## 16                        18554
    ## 17                        18355
    ## 18                        18207
    ## 19                        18019
    ## 20                        17907
    ## 21                        17806
    ## 22                        17623
    ## 23                        17382
    ## 24                        17164
    ## 25                        16932
    ## 26                        16676
    ## 27                        16510
    ## 28                        16395
    ## 29                        16170
    ## 30                        15907
    ## 31                        15706
    ## 32                        15504
    ## 33                        15330
    ## 34                        15249
    ## 35                        15171
    ## 36                        15007
    ## 37                        14795
    ## 38                        14637
    ## 39                        14428
    ## 40                        14300
    ## 41                        14229
    ## 42                        14144
    ## 43                        14007
    ## 44                        13698
    ## 45                        13521
    ## 46                        13394
    ## 47                        13274
    ## 48                        13217
    ## 49                        13149
    ## 50                        13030
    ## 51                        12816
    ## 52                        12617
    ## 53                        12479
    ## 54                        12270
    ## 55                        12198
    ## 56                        12110
    ## 57                        11968
    ## 58                        11862
    ## 59                        11711
    ## 60                        11565
    ## 61                        11386
    ## 62                        11345
    ## 63                        11276
    ## 64                        11176
    ## 65                        11046
    ## 66                        10880
    ## 67                        10764
    ## 68                        10648
    ## 69                        10588
    ## 70                        10536
    ## 71                        10444
    ## 72                        10327
    ## 73                        10229
    ## 74                        10126
    ## 75                         9952
    ## 76                         9885
    ## 77                         9824
    ## 78                         9701
    ## 79                         9599
    ## 80                         9405
    ## 81                         9242
    ## 82                         9090
    ## 83                         9022
    ## 84                         8971
    ## 85                         8881
    ## 86                         8764
    ## 87                         8638
    ## 88                         8531
    ## 89                         8409
    ## 90                         8283
    ## 91                         8176
    ## 92                         8075
    ## 93                         7844
    ## 94                         7694
    ## 95                         7577
    ## 96                         7448
    ## 97                         7411
    ## 98                         7363
    ## 99                         7219
    ## 100                        7080
    ## 101                        6979
    ## 102                        6877
    ## 103                        6785
    ## 104                        6738
    ## 105                        6698
    ## 106                        6608
    ## 107                        6516
    ## 108                        6411
    ## 109                        6300
    ## 110                        6159
    ## 111                        6092
    ## 112                        5983
    ## 113                        5832
    ## 114                        5749
    ## 115                        5679
    ## 116                        5559
    ## 117                        5403
    ## 118                        5345
    ## 119                        5283
    ## 120                        5190
    ## 121                        5120
    ## 122                        5000
    ## 123                        4881
    ## 124                        4766
    ## 125                        4694
    ## 126                        4610
    ## 127                        4402
    ## 128                        4269
    ## 129                        4029
    ## 130                        3882
    ## 131                        3665
    ## 132                        3619
    ## 133                        3459
    ## 134                        3257
    ## 135                        3042
    ## 136                        2901
    ## 137                        2804
    ## 138                        2727
    ## 139                        2642
    ## 140                        2509
    ## 141                        2460
    ## 142                        2257
    ## 143                        2082
    ## 144                        1968
    ## 145                        1895
    ## 146                        1770
    ## 147                        1642
    ## 148                        1538
    ## 149                        1499
    ## 150                        1464
    ## 151                        1424
    ## 152                        1377
    ## 153                        1330
    ## 154                        1302
    ## 155                        1254
    ## 156                        1162
    ## 157                        1040
    ## 158                         981
    ## 159                         940
    ## 160                         915
    ## 161                         882
    ## 162                         828
    ## 163                         779
    ## 164                         737
    ## 165                         708
    ## 166                         682
    ## 167                         662
    ## 168                         645
    ## 169                         604
    ## 170                         572
    ## 171                         540
    ## 172                         519
    ## 173                         496
    ## 174                         479
    ## 175                         453
    ## 176                         433
    ## 177                         412
    ## 178                         394
    ## 179                         362
    ## 180                         325
    ## 181                         309
    ## 182                         290
    ## 183                         274
    ## 184                         248
    ## 185                         236
    ## 186                         221
    ## 187                         204
    ## 188                         203
    ## 189                         191
    ## 190                         169
    ## 191                         156
    ## 192                         150
    ## 193                         139
    ## 194                         125
    ## 195                         118
    ## 196                         109
    ## 197                         103
    ## 198                          97
    ## 199                          88
    ## 200                          78
    ## 201                          75
    ## 202                          71
    ## 203                          65
    ## 204                          59
    ## 205                          55
    ## 206                          45
    ## 207                          35
    ## 208                          34
    ## 209                          32
    ## 210                          28
    ## 211                          27
    ## 212                          22
    ## 213                          21
    ## 214                          20
    ## 215                          20
    ## 216                          20
    ## 217                          20
    ## 218                          20
    ## 219                          18
    ## 220                          16
    ## 221                          15
    ## 222                          15
    ## 223                          15
    ## 224                          13
    ## 225                          10
    ## 226                           3
    ## 227                           2
    ## 228                           2
    ## 229                           2
    ## 230                           2
    ## 231                           1
    ## 232                           1
    ## 233                           1
    ## 234                           1
    ## 235                           1
    ## 236                           1
    ## 237                           1
    ## 238                           0
    ## 239                           0
    ## 240                           0
    ## 241                           0
    ## 242                           0
    ## 243                           0
    ## 244                           0
    ## 245                           0
    ## 246                           0
    ## 247                           0
    ## 248                           0
    ## 249                           0

``` r
c_cases <- filter(w_cases, Local.Authority == "Conwy")
c_cases
```

    ##     Local.Authority Specimen.date Cases..new. Cumulative.cases
    ## 1             Conwy    2020-10-08           0             1008
    ## 2             Conwy    2020-10-07           5             1008
    ## 3             Conwy    2020-10-06          30             1003
    ## 4             Conwy    2020-10-05          11              973
    ## 5             Conwy    2020-10-04           7              962
    ## 6             Conwy    2020-10-03          11              955
    ## 7             Conwy    2020-10-02          12              944
    ## 8             Conwy    2020-10-01          12              932
    ## 9             Conwy    2020-09-30          11              920
    ## 10            Conwy    2020-09-29          12              909
    ## 11            Conwy    2020-09-28          15              897
    ## 12            Conwy    2020-09-27           7              882
    ## 13            Conwy    2020-09-26           8              875
    ## 14            Conwy    2020-09-25          12              867
    ## 15            Conwy    2020-09-24           6              855
    ## 16            Conwy    2020-09-23           5              849
    ## 17            Conwy    2020-09-22          10              844
    ## 18            Conwy    2020-09-21           9              834
    ## 19            Conwy    2020-09-20           4              825
    ## 20            Conwy    2020-09-19           8              821
    ## 21            Conwy    2020-09-18           7              813
    ## 22            Conwy    2020-09-17           5              806
    ## 23            Conwy    2020-09-16           5              801
    ## 24            Conwy    2020-09-15           2              796
    ## 25            Conwy    2020-09-14           7              794
    ## 26            Conwy    2020-09-13           2              787
    ## 27            Conwy    2020-09-12           2              785
    ## 28            Conwy    2020-09-11           6              783
    ## 29            Conwy    2020-09-10           6              777
    ## 30            Conwy    2020-09-09           7              771
    ## 31            Conwy    2020-09-08           6              764
    ## 32            Conwy    2020-09-07           7              758
    ## 33            Conwy    2020-09-06           2              751
    ## 34            Conwy    2020-09-05           1              749
    ## 35            Conwy    2020-09-04           0              748
    ## 36            Conwy    2020-09-03           4              748
    ## 37            Conwy    2020-09-02           2              744
    ## 38            Conwy    2020-09-01           1              742
    ## 39            Conwy    2020-08-31           3              741
    ## 40            Conwy    2020-08-30           2              738
    ## 41            Conwy    2020-08-29           1              736
    ## 42            Conwy    2020-08-28           1              735
    ## 43            Conwy    2020-08-27           1              734
    ## 44            Conwy    2020-08-26           0              733
    ## 45            Conwy    2020-08-25           1              733
    ## 46            Conwy    2020-08-24           0              732
    ## 47            Conwy    2020-08-23           0              732
    ## 48            Conwy    2020-08-22           0              732
    ## 49            Conwy    2020-08-21           0              732
    ## 50            Conwy    2020-08-20           1              732
    ## 51            Conwy    2020-08-19           1              731
    ## 52            Conwy    2020-08-18           1              730
    ## 53            Conwy    2020-08-17           0              729
    ## 54            Conwy    2020-08-16           1              729
    ## 55            Conwy    2020-08-15           0              728
    ## 56            Conwy    2020-08-14           0              728
    ## 57            Conwy    2020-08-13           0              728
    ## 58            Conwy    2020-08-12           1              728
    ## 59            Conwy    2020-08-11           1              727
    ## 60            Conwy    2020-08-10           1              726
    ## 61            Conwy    2020-08-09           1              725
    ## 62            Conwy    2020-08-08           0              724
    ## 63            Conwy    2020-08-07           2              724
    ## 64            Conwy    2020-08-06           1              722
    ## 65            Conwy    2020-08-05           0              721
    ## 66            Conwy    2020-08-04           0              721
    ## 67            Conwy    2020-08-03           0              721
    ## 68            Conwy    2020-08-02           0              721
    ## 69            Conwy    2020-08-01           1              721
    ## 70            Conwy    2020-07-31           2              720
    ## 71            Conwy    2020-07-30           2              718
    ## 72            Conwy    2020-07-29           0              716
    ## 73            Conwy    2020-07-28           0              716
    ## 74            Conwy    2020-07-27           0              716
    ## 75            Conwy    2020-07-26           0              716
    ## 76            Conwy    2020-07-25           1              716
    ## 77            Conwy    2020-07-24           1              715
    ## 78            Conwy    2020-07-23           2              714
    ## 79            Conwy    2020-07-22           0              712
    ## 80            Conwy    2020-07-21           1              712
    ## 81            Conwy    2020-07-20           1              711
    ## 82            Conwy    2020-07-19           1              710
    ## 83            Conwy    2020-07-18           1              709
    ## 84            Conwy    2020-07-17           1              708
    ## 85            Conwy    2020-07-16           0              707
    ## 86            Conwy    2020-07-15           0              707
    ## 87            Conwy    2020-07-14           2              707
    ## 88            Conwy    2020-07-13           4              705
    ## 89            Conwy    2020-07-12           1              701
    ## 90            Conwy    2020-07-11           3              700
    ## 91            Conwy    2020-07-10           1              697
    ## 92            Conwy    2020-07-09           3              696
    ## 93            Conwy    2020-07-08           0              693
    ## 94            Conwy    2020-07-07           1              693
    ## 95            Conwy    2020-07-06           0              692
    ## 96            Conwy    2020-07-05           0              692
    ## 97            Conwy    2020-07-04           1              692
    ## 98            Conwy    2020-07-03           0              691
    ## 99            Conwy    2020-07-02           2              691
    ## 100           Conwy    2020-07-01           0              689
    ## 101           Conwy    2020-06-30           0              689
    ## 102           Conwy    2020-06-29           2              689
    ## 103           Conwy    2020-06-28           3              687
    ## 104           Conwy    2020-06-27           3              684
    ## 105           Conwy    2020-06-26           0              681
    ## 106           Conwy    2020-06-25           4              681
    ## 107           Conwy    2020-06-24           2              677
    ## 108           Conwy    2020-06-23           6              675
    ## 109           Conwy    2020-06-22           4              669
    ## 110           Conwy    2020-06-21           0              665
    ## 111           Conwy    2020-06-20           1              665
    ## 112           Conwy    2020-06-19           7              664
    ## 113           Conwy    2020-06-18           5              657
    ## 114           Conwy    2020-06-17           2              652
    ## 115           Conwy    2020-06-16           5              650
    ## 116           Conwy    2020-06-15           7              645
    ## 117           Conwy    2020-06-14           3              638
    ## 118           Conwy    2020-06-13           4              635
    ## 119           Conwy    2020-06-12           8              631
    ## 120           Conwy    2020-06-11           3              623
    ## 121           Conwy    2020-06-10          10              620
    ## 122           Conwy    2020-06-09           6              610
    ## 123           Conwy    2020-06-08           6              604
    ## 124           Conwy    2020-06-07           1              598
    ## 125           Conwy    2020-06-06           5              597
    ## 126           Conwy    2020-06-05           3              592
    ## 127           Conwy    2020-06-04           5              589
    ## 128           Conwy    2020-06-03           3              584
    ## 129           Conwy    2020-06-02           5              581
    ## 130           Conwy    2020-06-01           5              576
    ## 131           Conwy    2020-05-31           3              571
    ## 132           Conwy    2020-05-30           8              568
    ## 133           Conwy    2020-05-29          20              560
    ## 134           Conwy    2020-05-28           6              540
    ## 135           Conwy    2020-05-27           8              534
    ## 136           Conwy    2020-05-26          14              526
    ## 137           Conwy    2020-05-25           6              512
    ## 138           Conwy    2020-05-24           2              506
    ## 139           Conwy    2020-05-23           8              504
    ## 140           Conwy    2020-05-22          14              496
    ## 141           Conwy    2020-05-21          15              482
    ## 142           Conwy    2020-05-20          13              467
    ## 143           Conwy    2020-05-19          13              454
    ## 144           Conwy    2020-05-18           5              441
    ## 145           Conwy    2020-05-17           5              436
    ## 146           Conwy    2020-05-16          11              431
    ## 147           Conwy    2020-05-15          12              420
    ## 148           Conwy    2020-05-14          14              408
    ## 149           Conwy    2020-05-13          21              394
    ## 150           Conwy    2020-05-12          14              373
    ## 151           Conwy    2020-05-11          39              359
    ## 152           Conwy    2020-05-10           2              320
    ## 153           Conwy    2020-05-09           9              318
    ## 154           Conwy    2020-05-08           6              309
    ## 155           Conwy    2020-05-07           7              303
    ## 156           Conwy    2020-05-06          10              296
    ## 157           Conwy    2020-05-05           4              286
    ## 158           Conwy    2020-05-04          12              282
    ## 159           Conwy    2020-05-03           5              270
    ## 160           Conwy    2020-05-02           3              265
    ## 161           Conwy    2020-05-01          14              262
    ## 162           Conwy    2020-04-30           9              248
    ## 163           Conwy    2020-04-29           7              239
    ## 164           Conwy    2020-04-28          12              232
    ## 165           Conwy    2020-04-27           4              220
    ## 166           Conwy    2020-04-26           8              216
    ## 167           Conwy    2020-04-25           5              208
    ## 168           Conwy    2020-04-24           9              203
    ## 169           Conwy    2020-04-23           7              194
    ## 170           Conwy    2020-04-22           8              187
    ## 171           Conwy    2020-04-21          12              179
    ## 172           Conwy    2020-04-20          24              167
    ## 173           Conwy    2020-04-19          10              143
    ## 174           Conwy    2020-04-18          10              133
    ## 175           Conwy    2020-04-17          13              123
    ## 176           Conwy    2020-04-16           6              110
    ## 177           Conwy    2020-04-15          15              104
    ## 178           Conwy    2020-04-14           7               89
    ## 179           Conwy    2020-04-13           5               82
    ## 180           Conwy    2020-04-12          10               77
    ## 181           Conwy    2020-04-11          10               67
    ## 182           Conwy    2020-04-10           3               57
    ## 183           Conwy    2020-04-09           8               54
    ## 184           Conwy    2020-04-08           7               46
    ## 185           Conwy    2020-04-07           5               39
    ## 186           Conwy    2020-04-06           7               34
    ## 187           Conwy    2020-04-05           2               27
    ## 188           Conwy    2020-04-04           3               25
    ## 189           Conwy    2020-04-03           2               22
    ## 190           Conwy    2020-04-02           4               20
    ## 191           Conwy    2020-04-01           1               16
    ## 192           Conwy    2020-03-31           4               15
    ## 193           Conwy    2020-03-30           1               11
    ## 194           Conwy    2020-03-29           1               10
    ## 195           Conwy    2020-03-28           2                9
    ## 196           Conwy    2020-03-27           0                7
    ## 197           Conwy    2020-03-26           1                7
    ## 198           Conwy    2020-03-25           3                6
    ## 199           Conwy    2020-03-24           1                3
    ## 200           Conwy    2020-03-23           1                2
    ## 201           Conwy    2020-03-22           0                1
    ## 202           Conwy    2020-03-21           0                1
    ## 203           Conwy    2020-03-20           0                1
    ## 204           Conwy    2020-03-19           0                1
    ## 205           Conwy    2020-03-18           0                1
    ## 206           Conwy    2020-03-17           0                1
    ## 207           Conwy    2020-03-16           0                1
    ## 208           Conwy    2020-03-15           0                1
    ## 209           Conwy    2020-03-14           0                1
    ## 210           Conwy    2020-03-13           0                1
    ## 211           Conwy    2020-03-12           1                1
    ## 212           Conwy    2020-03-11           0                0
    ## 213           Conwy    2020-03-10           0                0
    ## 214           Conwy    2020-03-09           0                0
    ## 215           Conwy    2020-03-08           0                0
    ## 216           Conwy    2020-03-07           0                0
    ## 217           Conwy    2020-03-06           0                0
    ## 218           Conwy    2020-03-05           0                0
    ## 219           Conwy    2020-03-04           0                0
    ## 220           Conwy    2020-03-03           0                0
    ## 221           Conwy    2020-03-02           0                0
    ## 222           Conwy    2020-03-01           0                0
    ## 223           Conwy    2020-02-29           0                0
    ## 224           Conwy    2020-02-28           0                0
    ## 225           Conwy    2020-02-27           0                0
    ## 226           Conwy    2020-02-26           0                0
    ## 227           Conwy    2020-02-25           0                0
    ## 228           Conwy    2020-02-24           0                0
    ## 229           Conwy    2020-02-23           0                0
    ## 230           Conwy    2020-02-22           0                0
    ## 231           Conwy    2020-02-21           0                0
    ## 232           Conwy    2020-02-20           0                0
    ## 233           Conwy    2020-02-19           0                0
    ## 234           Conwy    2020-02-18           0                0
    ## 235           Conwy    2020-02-17           0                0
    ## 236           Conwy    2020-02-16           0                0
    ## 237           Conwy    2020-02-15           0                0
    ## 238           Conwy    2020-02-14           0                0
    ## 239           Conwy    2020-02-13           0                0
    ## 240           Conwy    2020-02-12           0                0
    ## 241           Conwy    2020-02-11           0                0
    ## 242           Conwy    2020-02-10           0                0
    ## 243           Conwy    2020-02-09           0                0
    ## 244           Conwy    2020-02-08           0                0
    ## 245           Conwy    2020-02-07           0                0
    ## 246           Conwy    2020-02-06           0                0
    ## 247           Conwy    2020-02-05           0                0
    ## 248           Conwy    2020-02-04           0                0
    ## 249           Conwy    2020-02-03           0                0
    ##     Cumulative.incidence.per.100.000.population Testing.episodes..new.
    ## 1                                         860.0                      4
    ## 2                                         860.0                     42
    ## 3                                         855.8                    219
    ## 4                                         830.2                    213
    ## 5                                         820.8                    169
    ## 6                                         814.8                    187
    ## 7                                         805.4                    272
    ## 8                                         795.2                    320
    ## 9                                         785.0                    260
    ## 10                                        775.6                    273
    ## 11                                        765.3                    337
    ## 12                                        752.5                    167
    ## 13                                        746.6                    170
    ## 14                                        739.7                    299
    ## 15                                        729.5                    356
    ## 16                                        724.4                    274
    ## 17                                        720.1                    361
    ## 18                                        711.6                    290
    ## 19                                        703.9                    211
    ## 20                                        700.5                    204
    ## 21                                        693.7                    241
    ## 22                                        687.7                    282
    ## 23                                        683.4                    267
    ## 24                                        679.2                    336
    ## 25                                        677.5                    235
    ## 26                                        671.5                    153
    ## 27                                        669.8                    151
    ## 28                                        668.1                    260
    ## 29                                        663.0                    247
    ## 30                                        657.8                    252
    ## 31                                        651.9                    240
    ## 32                                        646.7                    208
    ## 33                                        640.8                    154
    ## 34                                        639.1                    111
    ## 35                                        638.2                    227
    ## 36                                        638.2                    233
    ## 37                                        634.8                    232
    ## 38                                        633.1                    236
    ## 39                                        632.2                    128
    ## 40                                        629.7                     82
    ## 41                                        628.0                    135
    ## 42                                        627.1                    211
    ## 43                                        626.3                    260
    ## 44                                        625.4                    149
    ## 45                                        625.4                    183
    ## 46                                        624.6                    138
    ## 47                                        624.6                     98
    ## 48                                        624.6                     99
    ## 49                                        624.6                    212
    ## 50                                        624.6                    229
    ## 51                                        623.7                    192
    ## 52                                        622.9                    206
    ## 53                                        622.0                    178
    ## 54                                        622.0                     68
    ## 55                                        621.1                     67
    ## 56                                        621.1                    120
    ## 57                                        621.1                    172
    ## 58                                        621.1                    144
    ## 59                                        620.3                    198
    ## 60                                        619.4                    227
    ## 61                                        618.6                     80
    ## 62                                        617.7                    115
    ## 63                                        617.7                    144
    ## 64                                        616.0                    122
    ## 65                                        615.2                    110
    ## 66                                        615.2                    126
    ## 67                                        615.2                    138
    ## 68                                        615.2                     64
    ## 69                                        615.2                     63
    ## 70                                        614.3                    109
    ## 71                                        612.6                    165
    ## 72                                        610.9                    120
    ## 73                                        610.9                    116
    ## 74                                        610.9                    145
    ## 75                                        610.9                     61
    ## 76                                        610.9                     69
    ## 77                                        610.1                    148
    ## 78                                        609.2                    192
    ## 79                                        607.5                    218
    ## 80                                        607.5                    185
    ## 81                                        606.6                    129
    ## 82                                        605.8                     67
    ## 83                                        604.9                     96
    ## 84                                        604.1                    145
    ## 85                                        603.2                    114
    ## 86                                        603.2                    101
    ## 87                                        603.2                    155
    ## 88                                        601.5                     98
    ## 89                                        598.1                     66
    ## 90                                        597.3                     58
    ## 91                                        594.7                    150
    ## 92                                        593.8                    125
    ## 93                                        591.3                     87
    ## 94                                        591.3                    123
    ## 95                                        590.4                    115
    ## 96                                        590.4                     74
    ## 97                                        590.4                     40
    ## 98                                        589.6                     70
    ## 99                                        589.6                    138
    ## 100                                       587.9                     92
    ## 101                                       587.9                    113
    ## 102                                       587.9                    137
    ## 103                                       586.2                     50
    ## 104                                       583.6                     55
    ## 105                                       581.0                     91
    ## 106                                       581.0                     74
    ## 107                                       577.6                     84
    ## 108                                       575.9                    154
    ## 109                                       570.8                    106
    ## 110                                       567.4                     69
    ## 111                                       567.4                     42
    ## 112                                       566.5                    106
    ## 113                                       560.6                     95
    ## 114                                       556.3                    135
    ## 115                                       554.6                     75
    ## 116                                       550.3                     66
    ## 117                                       544.4                     53
    ## 118                                       541.8                     70
    ## 119                                       538.4                    112
    ## 120                                       531.6                    125
    ## 121                                       529.0                    153
    ## 122                                       520.5                     78
    ## 123                                       515.3                    187
    ## 124                                       510.2                    114
    ## 125                                       509.4                    190
    ## 126                                       505.1                    276
    ## 127                                       502.5                    325
    ## 128                                       498.3                    186
    ## 129                                       495.7                    198
    ## 130                                       491.5                    230
    ## 131                                       487.2                    124
    ## 132                                       484.6                    140
    ## 133                                       477.8                    217
    ## 134                                       460.7                     76
    ## 135                                       455.6                    136
    ## 136                                       448.8                    202
    ## 137                                       436.8                     97
    ## 138                                       431.7                     76
    ## 139                                       430.0                    136
    ## 140                                       423.2                    167
    ## 141                                       411.3                    151
    ## 142                                       398.5                    161
    ## 143                                       387.4                    149
    ## 144                                       376.3                     74
    ## 145                                       372.0                     32
    ## 146                                       367.7                     76
    ## 147                                       358.4                    146
    ## 148                                       348.1                     55
    ## 149                                       336.2                    110
    ## 150                                       318.3                     71
    ## 151                                       306.3                     85
    ## 152                                       273.0                     20
    ## 153                                       271.3                     38
    ## 154                                       263.6                     76
    ## 155                                       258.5                     63
    ## 156                                       252.6                     55
    ## 157                                       244.0                     35
    ## 158                                       240.6                     70
    ## 159                                       230.4                     31
    ## 160                                       226.1                     33
    ## 161                                       223.5                     65
    ## 162                                       211.6                     48
    ## 163                                       203.9                     38
    ## 164                                       197.9                     32
    ## 165                                       187.7                     33
    ## 166                                       184.3                     15
    ## 167                                       177.5                     16
    ## 168                                       173.2                     34
    ## 169                                       165.5                     33
    ## 170                                       159.6                     26
    ## 171                                       152.7                     33
    ## 172                                       142.5                     45
    ## 173                                       122.0                     23
    ## 174                                       113.5                     25
    ## 175                                       104.9                     34
    ## 176                                        93.9                     20
    ## 177                                        88.7                     30
    ## 178                                        75.9                     27
    ## 179                                        70.0                     11
    ## 180                                        65.7                     19
    ## 181                                        57.2                     28
    ## 182                                        48.6                     11
    ## 183                                        46.1                     17
    ## 184                                        39.2                     20
    ## 185                                        33.3                     18
    ## 186                                        29.0                     18
    ## 187                                        23.0                      7
    ## 188                                        21.3                      7
    ## 189                                        18.8                     12
    ## 190                                        17.1                     12
    ## 191                                        13.7                      7
    ## 192                                        12.8                     14
    ## 193                                         9.4                     10
    ## 194                                         8.5                      8
    ## 195                                         7.7                     10
    ## 196                                         6.0                     14
    ## 197                                         6.0                     11
    ## 198                                         5.1                     13
    ## 199                                         2.6                     14
    ## 200                                         1.7                     14
    ## 201                                         0.9                      4
    ## 202                                         0.9                      5
    ## 203                                         0.9                     10
    ## 204                                         0.9                     11
    ## 205                                         0.9                      3
    ## 206                                         0.9                     16
    ## 207                                         0.9                      7
    ## 208                                         0.9                      5
    ## 209                                         0.9                      6
    ## 210                                         0.9                      5
    ## 211                                         0.9                     10
    ## 212                                         0.0                      2
    ## 213                                         0.0                      1
    ## 214                                         0.0                      0
    ## 215                                         0.0                      0
    ## 216                                         0.0                      0
    ## 217                                         0.0                      0
    ## 218                                         0.0                      1
    ## 219                                         0.0                      0
    ## 220                                         0.0                      2
    ## 221                                         0.0                      1
    ## 222                                         0.0                      0
    ## 223                                         0.0                      2
    ## 224                                         0.0                      5
    ## 225                                         0.0                      4
    ## 226                                         0.0                      1
    ## 227                                         0.0                      1
    ## 228                                         0.0                      0
    ## 229                                         0.0                      0
    ## 230                                         0.0                      0
    ## 231                                         0.0                      0
    ## 232                                         0.0                      0
    ## 233                                         0.0                      0
    ## 234                                         0.0                      0
    ## 235                                         0.0                      1
    ## 236                                         0.0                      0
    ## 237                                         0.0                      0
    ## 238                                         0.0                      0
    ## 239                                         0.0                      0
    ## 240                                         0.0                      0
    ## 241                                         0.0                      0
    ## 242                                         0.0                      0
    ## 243                                         0.0                      1
    ## 244                                         0.0                      0
    ## 245                                         0.0                      0
    ## 246                                         0.0                      0
    ## 247                                         0.0                      0
    ## 248                                         0.0                      0
    ## 249                                         0.0                      0
    ##     Cumulative.testing.episodes
    ## 1                         24349
    ## 2                         24345
    ## 3                         24303
    ## 4                         24084
    ## 5                         23871
    ## 6                         23702
    ## 7                         23515
    ## 8                         23243
    ## 9                         22923
    ## 10                        22663
    ## 11                        22390
    ## 12                        22053
    ## 13                        21886
    ## 14                        21716
    ## 15                        21417
    ## 16                        21061
    ## 17                        20787
    ## 18                        20426
    ## 19                        20136
    ## 20                        19925
    ## 21                        19721
    ## 22                        19480
    ## 23                        19198
    ## 24                        18931
    ## 25                        18595
    ## 26                        18360
    ## 27                        18207
    ## 28                        18056
    ## 29                        17796
    ## 30                        17549
    ## 31                        17297
    ## 32                        17057
    ## 33                        16849
    ## 34                        16695
    ## 35                        16584
    ## 36                        16357
    ## 37                        16124
    ## 38                        15892
    ## 39                        15656
    ## 40                        15528
    ## 41                        15446
    ## 42                        15311
    ## 43                        15100
    ## 44                        14840
    ## 45                        14691
    ## 46                        14508
    ## 47                        14370
    ## 48                        14272
    ## 49                        14173
    ## 50                        13961
    ## 51                        13732
    ## 52                        13540
    ## 53                        13334
    ## 54                        13156
    ## 55                        13088
    ## 56                        13021
    ## 57                        12901
    ## 58                        12729
    ## 59                        12585
    ## 60                        12387
    ## 61                        12160
    ## 62                        12080
    ## 63                        11965
    ## 64                        11821
    ## 65                        11699
    ## 66                        11589
    ## 67                        11463
    ## 68                        11325
    ## 69                        11261
    ## 70                        11198
    ## 71                        11089
    ## 72                        10924
    ## 73                        10804
    ## 74                        10688
    ## 75                        10543
    ## 76                        10482
    ## 77                        10413
    ## 78                        10265
    ## 79                        10073
    ## 80                         9855
    ## 81                         9670
    ## 82                         9541
    ## 83                         9474
    ## 84                         9378
    ## 85                         9233
    ## 86                         9119
    ## 87                         9018
    ## 88                         8863
    ## 89                         8765
    ## 90                         8699
    ## 91                         8641
    ## 92                         8491
    ## 93                         8366
    ## 94                         8279
    ## 95                         8156
    ## 96                         8041
    ## 97                         7967
    ## 98                         7927
    ## 99                         7857
    ## 100                        7719
    ## 101                        7627
    ## 102                        7514
    ## 103                        7377
    ## 104                        7327
    ## 105                        7272
    ## 106                        7181
    ## 107                        7107
    ## 108                        7023
    ## 109                        6869
    ## 110                        6763
    ## 111                        6694
    ## 112                        6652
    ## 113                        6546
    ## 114                        6451
    ## 115                        6316
    ## 116                        6241
    ## 117                        6175
    ## 118                        6122
    ## 119                        6052
    ## 120                        5940
    ## 121                        5815
    ## 122                        5662
    ## 123                        5584
    ## 124                        5397
    ## 125                        5283
    ## 126                        5093
    ## 127                        4817
    ## 128                        4492
    ## 129                        4306
    ## 130                        4108
    ## 131                        3878
    ## 132                        3754
    ## 133                        3614
    ## 134                        3397
    ## 135                        3321
    ## 136                        3185
    ## 137                        2983
    ## 138                        2886
    ## 139                        2810
    ## 140                        2674
    ## 141                        2507
    ## 142                        2356
    ## 143                        2195
    ## 144                        2046
    ## 145                        1972
    ## 146                        1940
    ## 147                        1864
    ## 148                        1718
    ## 149                        1663
    ## 150                        1553
    ## 151                        1482
    ## 152                        1397
    ## 153                        1377
    ## 154                        1339
    ## 155                        1263
    ## 156                        1200
    ## 157                        1145
    ## 158                        1110
    ## 159                        1040
    ## 160                        1009
    ## 161                         976
    ## 162                         911
    ## 163                         863
    ## 164                         825
    ## 165                         793
    ## 166                         760
    ## 167                         745
    ## 168                         729
    ## 169                         695
    ## 170                         662
    ## 171                         636
    ## 172                         603
    ## 173                         558
    ## 174                         535
    ## 175                         510
    ## 176                         476
    ## 177                         456
    ## 178                         426
    ## 179                         399
    ## 180                         388
    ## 181                         369
    ## 182                         341
    ## 183                         330
    ## 184                         313
    ## 185                         293
    ## 186                         275
    ## 187                         257
    ## 188                         250
    ## 189                         243
    ## 190                         231
    ## 191                         219
    ## 192                         212
    ## 193                         198
    ## 194                         188
    ## 195                         180
    ## 196                         170
    ## 197                         156
    ## 198                         145
    ## 199                         132
    ## 200                         118
    ## 201                         104
    ## 202                         100
    ## 203                          95
    ## 204                          85
    ## 205                          74
    ## 206                          71
    ## 207                          55
    ## 208                          48
    ## 209                          43
    ## 210                          37
    ## 211                          32
    ## 212                          22
    ## 213                          20
    ## 214                          19
    ## 215                          19
    ## 216                          19
    ## 217                          19
    ## 218                          19
    ## 219                          18
    ## 220                          18
    ## 221                          16
    ## 222                          15
    ## 223                          15
    ## 224                          13
    ## 225                           8
    ## 226                           4
    ## 227                           3
    ## 228                           2
    ## 229                           2
    ## 230                           2
    ## 231                           2
    ## 232                           2
    ## 233                           2
    ## 234                           2
    ## 235                           2
    ## 236                           1
    ## 237                           1
    ## 238                           1
    ## 239                           1
    ## 240                           1
    ## 241                           1
    ## 242                           1
    ## 243                           1
    ## 244                           0
    ## 245                           0
    ## 246                           0
    ## 247                           0
    ## 248                           0
    ## 249                           0

Next create a new dataframe where Conwy, Anglesey and Gwynedd each have
their own columns, along with the testing episodes for each county.I
just take the data from row 6 onwards, as turnaround times are 1-6 days
(ish), so the data from the top rows is incomplete. Row 157 was the last
row when I started doing it but the table gets longer each week (not
near 65,000 rows yet though\!) so this row number could be updated.

``` r
newdf2 <- data.frame("Specimen date" = c_cases$Specimen.date[6:157],
                    "Conwy" = c_cases$Cases..new.[6:157],
                    "Anglesey" = a_cases$Cases..new.[6:157],
                    "Gwynedd" = g_cases$Cases..new.[6:157],
                    "Conwy_episodes" = c_cases$Testing.episodes..new.[6:157],
                    "Anglesey_episodes" = a_cases$Testing.episodes..new.[6:157],
                    "Gwynedd_episodes" = g_cases$Testing.episodes..new.[6:157])
```

Add columns to the new dataframe for the AGC total and the rolling
averages

``` r
newdf2 <- newdf2 %>% 
  mutate("roll_mean_C" = rollmean(Conwy, 7, fill = NA))
newdf2 <- newdf2 %>% 
  mutate("roll_mean_G" = rollmean(Gwynedd, 7, fill = NA))
newdf2 <- newdf2 %>% 
  mutate("roll_mean_A" = rollmean(Anglesey, 7, fill = NA))
newdf2 <- newdf2 %>% 
  mutate("AGC" = (Conwy+Anglesey+Gwynedd))
newdf2 <- newdf2 %>% 
  mutate("roll_mean" = rollmean(AGC, 7, fill = NA))
newdf2 <- newdf2 %>% 
  mutate("AGC_episodes" = (Conwy_episodes +
                             Anglesey_episodes+
                             Gwynedd_episodes))
newdf2 <- newdf2 %>% 
  mutate("roll_mean_episodes" = rollmean(AGC_episodes, 7, fill = NA))
newdf2 <- newdf2 %>% 
  mutate("Per_Positivity" = ((AGC/AGC_episodes)*100))
newdf2 <- newdf2 %>% 
  mutate("roll_mean_positivity" = rollmean(Per_Positivity, 7, fill = NA))
```

Make a lovely plot for Conwy

``` r
c_plot <- ggplot(data = newdf2, aes(x=Specimen.date, y=Conwy))+
  geom_bar(stat="identity", fill="steelblue", color = "steelblue",
           width = 0.6)+
  labs(x = "Specimen date",
       y = "Conwy")+
  geom_line(data = newdf2, aes(x=Specimen.date, y=roll_mean_C), 
            color = "royalblue4",
            size = 1)+
  ggtitle("Conwy cases by specimen date")

c_plot
```

    ## Warning: Removed 6 row(s) containing missing values (geom_path).

![](Code_files/figure-gfm/unnamed-chunk-7-1.png)<!-- --> Make a lovely
plot for Gwynedd

``` r
g_plot <- ggplot(data = newdf2, aes(x=Specimen.date, y=Gwynedd))+
  geom_bar(stat="identity", fill="steelblue", color = "steelblue",
           width = 0.6)+
  labs(x = "Specimen date",
       y = "Gwynedd")+
  geom_line(data = newdf2, aes(x=Specimen.date, y=roll_mean_G), 
            color = "royalblue4",
            size = 1)+
  ggtitle("Gwynedd cases by specimen date")

g_plot
```

    ## Warning: Removed 6 row(s) containing missing values (geom_path).

![](Code_files/figure-gfm/unnamed-chunk-8-1.png)<!-- --> Make a lovely
plot for Anglesey

``` r
a_plot <- ggplot(data = newdf2, aes(x=Specimen.date, y=Anglesey))+
  geom_bar(stat="identity", fill="steelblue", color = "steelblue",
           width = 0.6)+
  labs(x = "Specimen date",
       y = "Anglesey")+
  geom_line(data = newdf2, aes(x=Specimen.date, y=roll_mean_A), 
            color = "royalblue4",
            size = 1)+
  ggtitle("Anglesey cases by specimen date")

a_plot
```

    ## Warning: Removed 6 row(s) containing missing values (geom_path).

![](Code_files/figure-gfm/unnamed-chunk-9-1.png)<!-- --> Make a lovely
plot for these 3 regions combined

``` r
agc_plot <- ggplot(data = newdf2, aes(x=Specimen.date, y=AGC))+
  geom_bar(stat="identity", fill="steelblue", color = "steelblue",
           width = 0.6)+
  labs(x = "Specimen date",
       y = "Anglesey/Gwynedd/Conwy")+
  geom_line(data = newdf2, aes(x=Specimen.date, y=roll_mean), 
            color = "royalblue4",
            size = 1)+
  ggtitle("Local cases by specimen date")

agc_plot
```

    ## Warning: Removed 6 row(s) containing missing values (geom_path).

![](Code_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> Make a lovely
plot for AGC testing episodes

``` r
agc_episodes_plot <- ggplot(data = newdf2, aes(x=Specimen.date, y=AGC_episodes))+
  geom_bar(stat="identity", fill="steelblue", color = "steelblue",
           width = 0.6)+
  labs(x = "Episode date",
       y = "Anglesey/Gwynedd/Conwy")+
  geom_line(data = newdf2, aes(x=Specimen.date, y=roll_mean_episodes), 
            color = "royalblue4",
            size = 1)+
  ggtitle("Testing episodes by date")

agc_episodes_plot
```

    ## Warning: Removed 6 row(s) containing missing values (geom_path).

![](Code_files/figure-gfm/unnamed-chunk-11-1.png)<!-- --> Make a lovely
plot for test positivity. I’m estimating positivity by doing
((AGC/AGC\_episodes)\*100) which isn’t going to be totally right as it
doesn’t account for the same case being tested more than once but it’s
the best I can do.

``` r
agc_positivity <- ggplot(data = newdf2, aes(x=Specimen.date, y= Per_Positivity))+
  geom_bar(stat="identity", fill="steelblue", color = "steelblue",
           width = 0.6)+
  labs(x = "Episode date",
       y = "Anglesey/Gwynedd/Conwy")+
  geom_line(data = newdf2, aes(x=Specimen.date, y=roll_mean_positivity), 
            color = "royalblue4",
            size = 1)+
  ggtitle("Test % Positivity by date")

agc_positivity
```

    ## Warning: Removed 6 row(s) containing missing values (geom_path).

![](Code_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

THE END
