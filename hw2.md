p8105_hw2_rh3195
================
Ruijie He
2023-10-04

``` r
library(tidyverse)
library(readxl)
library(dplyr)
library(janitor)
```

## Problem 1

**Import pols-month.csv, clean data**

``` r
 month_df = 
    tibble(
    month_num = 1:12, 
    month = month.name)

pols = 
  read_csv("./data/fivethirtyeight_datasets/pols-month.csv") |> 
  separate(mon, into = c("year", "month_num", "day"), convert = TRUE) |> 
  mutate(
    president = recode(prez_gop, "0" = "dem", "1" = "gop", "2" = "gop")) |> 
  left_join(x = _, y = month_df) |> 
  select(year, month, everything(), -day, -starts_with("prez"))
```

    ## Rows: 822 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Joining with `by = join_by(month_num)`

``` r
print(pols)
```

    ## # A tibble: 822 × 10
    ##     year month     month_num gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem
    ##    <int> <chr>         <int>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1  1947 January           1      23      51     253      23      45     198
    ##  2  1947 February          2      23      51     253      23      45     198
    ##  3  1947 March             3      23      51     253      23      45     198
    ##  4  1947 April             4      23      51     253      23      45     198
    ##  5  1947 May               5      23      51     253      23      45     198
    ##  6  1947 June              6      23      51     253      23      45     198
    ##  7  1947 July              7      23      51     253      23      45     198
    ##  8  1947 August            8      23      51     253      23      45     198
    ##  9  1947 September         9      23      51     253      23      45     198
    ## 10  1947 October          10      23      51     253      23      45     198
    ## # ℹ 812 more rows
    ## # ℹ 1 more variable: president <chr>

> In `pols` dataset, there are 822 observations and 10 variables. The
> new variable `president` is added. This dataset provides the
> information of party and president at different time.

**Clean the data in snp.csv**

``` r
snp =
  read_csv("./data/fivethirtyeight_datasets/snp.csv") |>
  clean_names() |>
  separate(date, into = c("month", "day", "year"), convert = TRUE) |>
  arrange(year, month) |>
  mutate(
    year = ifelse(year >= 50, 1900 + year, 2000+year),
    month = month.name[month]) |>
  select(year, month, close)
```

    ## Rows: 787 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
print(snp)
```

    ## # A tibble: 787 × 3
    ##     year month     close
    ##    <dbl> <chr>     <dbl>
    ##  1  2000 January   1394.
    ##  2  2000 February  1366.
    ##  3  2000 March     1499.
    ##  4  2000 April     1452.
    ##  5  2000 May       1421.
    ##  6  2000 June      1455.
    ##  7  2000 July      1431.
    ##  8  2000 August    1518.
    ##  9  2000 September 1437.
    ## 10  2000 October   1429.
    ## # ℹ 777 more rows

> In `snp` dataset, there are 787 observations and 3 variables.

**Tidy the unemployment.csv**

``` r
unemployment = 
  read_csv("./data/fivethirtyeight_datasets/unemployment.csv") |>
  rename(year = Year) |>
  pivot_longer(
    Jan:Dec, 
    names_to = "month",
    values_to = "unemployment"
  ) |> 
  left_join(x = _, y = month_df) |> 
  select(year, month, unemployment)
```

    ## Rows: 68 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Joining with `by = join_by(month)`

``` r
print (unemployment)
```

    ## # A tibble: 816 × 3
    ##     year month unemployment
    ##    <dbl> <chr>        <dbl>
    ##  1  1948 Jan            3.4
    ##  2  1948 Feb            3.8
    ##  3  1948 Mar            4  
    ##  4  1948 Apr            3.9
    ##  5  1948 May            3.5
    ##  6  1948 Jun            3.6
    ##  7  1948 Jul            3.6
    ##  8  1948 Aug            3.9
    ##  9  1948 Sep            3.8
    ## 10  1948 Oct            3.7
    ## # ℹ 806 more rows

> There are 816 observations and 3 variables in `unemployment` dataset.
> Next step is to merge this dataset with the `pols` and `snp` datasets.

**Merge three dataset**

``` r
data_538 =
  left_join(pols, snp) |>
  left_join(x =_, y = unemployment)
```

    ## Joining with `by = join_by(year, month)`
    ## Joining with `by = join_by(year, month)`

``` r
str(data_538)
```

    ## tibble [822 × 12] (S3: tbl_df/tbl/data.frame)
    ##  $ year        : num [1:822] 1947 1947 1947 1947 1947 ...
    ##  $ month       : chr [1:822] "January" "February" "March" "April" ...
    ##  $ month_num   : int [1:822] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ gov_gop     : num [1:822] 23 23 23 23 23 23 23 23 23 23 ...
    ##  $ sen_gop     : num [1:822] 51 51 51 51 51 51 51 51 51 51 ...
    ##  $ rep_gop     : num [1:822] 253 253 253 253 253 253 253 253 253 253 ...
    ##  $ gov_dem     : num [1:822] 23 23 23 23 23 23 23 23 23 23 ...
    ##  $ sen_dem     : num [1:822] 45 45 45 45 45 45 45 45 45 45 ...
    ##  $ rep_dem     : num [1:822] 198 198 198 198 198 198 198 198 198 198 ...
    ##  $ president   : chr [1:822] "dem" "dem" "dem" "dem" ...
    ##  $ close       : num [1:822] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ unemployment: num [1:822] NA NA NA NA NA NA NA NA NA NA ...

``` r
print(data_538)
```

    ## # A tibble: 822 × 12
    ##     year month     month_num gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem
    ##    <dbl> <chr>         <int>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1  1947 January           1      23      51     253      23      45     198
    ##  2  1947 February          2      23      51     253      23      45     198
    ##  3  1947 March             3      23      51     253      23      45     198
    ##  4  1947 April             4      23      51     253      23      45     198
    ##  5  1947 May               5      23      51     253      23      45     198
    ##  6  1947 June              6      23      51     253      23      45     198
    ##  7  1947 July              7      23      51     253      23      45     198
    ##  8  1947 August            8      23      51     253      23      45     198
    ##  9  1947 September         9      23      51     253      23      45     198
    ## 10  1947 October          10      23      51     253      23      45     198
    ## # ℹ 812 more rows
    ## # ℹ 3 more variables: president <chr>, close <dbl>, unemployment <dbl>

**Data description**

> The merged dataset has 822 observations and 12 variables. We are
> missing some values in `close` and `unemployment` variables as there
> are some `NA` in the list.

## Problem 2

**Read and clean Mr.Trash Wheel sheet**

``` r
mr.trashwheel =
  read_excel("./data/202309 Trash Wheel Collection Data.xlsx", sheet = "Mr. Trash Wheel", range = "A2:N586") |>
  clean_names() |>
  mutate(
    name = "mr_trash_wheel",
    year = as.numeric(year),
    homes_powered = (weight_tons * 500/30)
  )
print(mr.trashwheel)
```

    ## # A tibble: 584 × 15
    ##    dumpster month  year date                weight_tons volume_cubic_yards
    ##       <dbl> <chr> <dbl> <dttm>                    <dbl>              <dbl>
    ##  1        1 May    2014 2014-05-16 00:00:00        4.31                 18
    ##  2        2 May    2014 2014-05-16 00:00:00        2.74                 13
    ##  3        3 May    2014 2014-05-16 00:00:00        3.45                 15
    ##  4        4 May    2014 2014-05-17 00:00:00        3.1                  15
    ##  5        5 May    2014 2014-05-17 00:00:00        4.06                 18
    ##  6        6 May    2014 2014-05-20 00:00:00        2.71                 13
    ##  7        7 May    2014 2014-05-21 00:00:00        1.91                  8
    ##  8        8 May    2014 2014-05-28 00:00:00        3.7                  16
    ##  9        9 June   2014 2014-06-05 00:00:00        2.52                 14
    ## 10       10 June   2014 2014-06-11 00:00:00        3.76                 18
    ## # ℹ 574 more rows
    ## # ℹ 9 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, glass_bottles <dbl>, plastic_bags <dbl>,
    ## #   wrappers <dbl>, sports_balls <dbl>, homes_powered <dbl>, name <chr>

> The `mr.trashwheel` dataset has 584 observations and 15 columns. It
> contains variables including dumpster, month, year, date, weight_tons,
> volume_cubic_yards, plastic_bottles, polystyrene, cigarette_butts,
> glass_bottles, plastic_bags, wrappers, sports_balls, homes_powered,
> name.

**Clean professor trash wheel sheet**

``` r
professor_trash_wheel =
  read_excel("./data/202309 Trash Wheel Collection Data.xlsx", sheet = "Professor Trash Wheel", range = "A2:M108") |>
  clean_names() |>
  mutate(
    year = as.numeric(year),
    name = "professor_trash_wheel",
    homes_powered = (weight_tons * 500/30)
  )
print(professor_trash_wheel)
```

    ## # A tibble: 106 × 14
    ##    dumpster month     year date                weight_tons volume_cubic_yards
    ##       <dbl> <chr>    <dbl> <dttm>                    <dbl>              <dbl>
    ##  1        1 January   2017 2017-01-02 00:00:00        1.79                 15
    ##  2        2 January   2017 2017-01-30 00:00:00        1.58                 15
    ##  3        3 February  2017 2017-02-26 00:00:00        2.32                 18
    ##  4        4 February  2017 2017-02-26 00:00:00        3.72                 15
    ##  5        5 February  2017 2017-02-28 00:00:00        1.45                 15
    ##  6        6 March     2017 2017-03-30 00:00:00        1.71                 15
    ##  7        7 April     2017 2017-04-01 00:00:00        1.82                 15
    ##  8        8 April     2017 2017-04-20 00:00:00        2.37                 15
    ##  9        9 May       2017 2017-05-10 00:00:00        2.64                 15
    ## 10       10 May       2017 2017-05-26 00:00:00        2.78                 15
    ## # ℹ 96 more rows
    ## # ℹ 8 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, glass_bottles <dbl>, plastic_bags <dbl>,
    ## #   wrappers <dbl>, homes_powered <dbl>, name <chr>

> The `professor_trash_wheel` has 106 observations and 14 columns.

**Clean Gwynnda trash wheel sheet**

``` r
gwynnda_trash_wheel =
  read_excel("./data/202309 Trash Wheel Collection Data.xlsx", sheet = "Gwynnda Trash Wheel", range = "A2:L157") |>
  clean_names() |>
  mutate(
    year = as.numeric(year),
    name = "gwynnda_trash_wheel",
    homes_powered = (weight_tons * 500/30)
  )
print(gwynnda_trash_wheel)
```

    ## # A tibble: 155 × 13
    ##    dumpster month   year date                weight_tons volume_cubic_yards
    ##       <dbl> <chr>  <dbl> <dttm>                    <dbl>              <dbl>
    ##  1        1 July    2021 2021-07-03 00:00:00        0.93                 15
    ##  2        2 July    2021 2021-07-07 00:00:00        2.26                 15
    ##  3        3 July    2021 2021-07-07 00:00:00        1.62                 15
    ##  4        4 July    2021 2021-07-16 00:00:00        1.76                 15
    ##  5        5 July    2021 2021-07-30 00:00:00        1.53                 15
    ##  6        6 August  2021 2021-08-11 00:00:00        2.06                 15
    ##  7        7 August  2021 2021-08-14 00:00:00        1.9                  15
    ##  8        8 August  2021 2021-08-16 00:00:00        2.16                 15
    ##  9        9 August  2021 2021-08-16 00:00:00        2.6                  15
    ## 10       10 August  2021 2021-08-17 00:00:00        3.21                 15
    ## # ℹ 145 more rows
    ## # ℹ 7 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, plastic_bags <dbl>, wrappers <dbl>,
    ## #   homes_powered <dbl>, name <chr>

> The Gwynnda trash wheel dataset has 155 observations and 13 columns.

**Combine the three data sheet**

``` r
trash_data = bind_rows(list(mr.trashwheel, professor_trash_wheel, gwynnda_trash_wheel)) |>
  select(name, everything())

print(trash_data)
```

    ## # A tibble: 845 × 15
    ##    name  dumpster month  year date                weight_tons volume_cubic_yards
    ##    <chr>    <dbl> <chr> <dbl> <dttm>                    <dbl>              <dbl>
    ##  1 mr_t…        1 May    2014 2014-05-16 00:00:00        4.31                 18
    ##  2 mr_t…        2 May    2014 2014-05-16 00:00:00        2.74                 13
    ##  3 mr_t…        3 May    2014 2014-05-16 00:00:00        3.45                 15
    ##  4 mr_t…        4 May    2014 2014-05-17 00:00:00        3.1                  15
    ##  5 mr_t…        5 May    2014 2014-05-17 00:00:00        4.06                 18
    ##  6 mr_t…        6 May    2014 2014-05-20 00:00:00        2.71                 13
    ##  7 mr_t…        7 May    2014 2014-05-21 00:00:00        1.91                  8
    ##  8 mr_t…        8 May    2014 2014-05-28 00:00:00        3.7                  16
    ##  9 mr_t…        9 June   2014 2014-06-05 00:00:00        2.52                 14
    ## 10 mr_t…       10 June   2014 2014-06-11 00:00:00        3.76                 18
    ## # ℹ 835 more rows
    ## # ℹ 8 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, glass_bottles <dbl>, plastic_bags <dbl>,
    ## #   wrappers <dbl>, sports_balls <dbl>, homes_powered <dbl>

**Data description**

> The three datasets that has been cleaned records the trash collecting
> detailed data, like the weight of collected trash and date. The
> combined dataset has 845 obersations and 15 variables. There are some
> `NA` data in `sports_balls`, `wrappers`, and `glass_bottles` datasets.
> The total weight of trash collected by `professor_trash_wheel` is
> 216.26 tons. Moreover, the total number of cigarettes butt by Gwynnda
> in July, 2021 is 1.63^{4}.

## Problem 3

**Import, clean, and tidy the dataset of baseline**

``` r
baseline_data = 
  read_csv("./data/data_mci/MCI_baseline.csv", skip =1, na = c(".")) |>
  janitor::clean_names() |>
  mutate(
    sex =
      case_match(
        sex,
        1 ~ "male",
        0 ~ "female"),
    sex = as.factor(sex),
    apoe4 =
      case_match(
        apoe4,
        1 ~ "carrier",
        0 ~ "non-carrier"),
    apoe4 = as.factor(apoe4)
      ) |>
  filter(!(current_age >= age_at_onset) | age_at_onset == ".")
```

    ## Rows: 483 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (6): ID, Current Age, Sex, Education, apoe4, Age at onset
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
print(baseline_data)
```

    ## # A tibble: 93 × 6
    ##       id current_age sex    education apoe4       age_at_onset
    ##    <dbl>       <dbl> <fct>      <dbl> <fct>              <dbl>
    ##  1     3        62.5 male          16 carrier             66.8
    ##  2     5        66   male          16 non-carrier         68.7
    ##  3     7        66.5 male          18 non-carrier         74  
    ##  4    13        63.1 male          12 carrier             69  
    ##  5    14        58.4 female        20 non-carrier         66.2
    ##  6    18        67.8 male          16 non-carrier         69.8
    ##  7    22        67.3 female        20 carrier             74.6
    ##  8    26        64.8 female        20 carrier             71.1
    ##  9    30        66.3 female        12 non-carrier         73.1
    ## 10    39        68.3 female        16 carrier             70.2
    ## # ℹ 83 more rows

``` r
mean(baseline_data$current_age)
```

    ## [1] 65.54194

``` r
#Number of women who carries APOE4 gene
prop_women_apoe4 <- baseline_data %>% 
  filter(sex == "female") %>% 
  summarize(mean(apoe4 == "carrier"))
print(prop_women_apoe4)
```

    ## # A tibble: 1 × 1
    ##   `mean(apoe4 == "carrier")`
    ##                        <dbl>
    ## 1                      0.667

**Data description**

> The dataset provides the participants of the observational study that
> is to identify if carries APOE4 gene or not. The first row of the
> dataset is removed as it’s just repeating the information as row 2.
> The study has recruited 483 participants. 93 participants has
> developed MCI and has 6 variables. The variable contains `age`, `sex`,
> and `apoe4` gene carrying status. The average baseline age is
> 65.5419355. 66.6666667%

**Clean and tidy amyloid dataset**

``` r
amyloid_data =
  read_csv("./data/data_mci/mci_amyloid.csv", skip =1, na = c(".", NA)) |>
  clean_names() |>
  rename (id = study_id) |>
  pivot_longer(
    baseline:time_8,
    names_to = "time(years)",
    values_to = "amyloid_ratio"
  )
```

    ## Rows: 487 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): Baseline, Time 2, Time 4, Time 6, Time 8
    ## dbl (1): Study ID
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
print (amyloid_data)
```

    ## # A tibble: 2,435 × 3
    ##       id `time(years)` amyloid_ratio
    ##    <dbl> <chr>         <chr>        
    ##  1     1 baseline      0.1105487    
    ##  2     1 time_2        NA           
    ##  3     1 time_4        0.109325197  
    ##  4     1 time_6        0.104756131  
    ##  5     1 time_8        0.107257697  
    ##  6     2 baseline      0.107481183  
    ##  7     2 time_2        0.109157373  
    ##  8     2 time_4        0.109457839  
    ##  9     2 time_6        0.105729713  
    ## 10     2 time_8        0.10661845   
    ## # ℹ 2,425 more rows

**Comment on the steps of import process**

**Join and combine baseline and amyloid data**
