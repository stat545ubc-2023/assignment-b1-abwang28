Assignment B-1
================
Alex Wang

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

``` r
library(palmerpenguins)
library(datateachr)
library(roxygen2)
```

## Make and Document a Function

groupby then summarise to find count of another variable by group DELETE
THIS LATER

``` r
#GOAL (DELETE LATER):
building_permits %>%
group_by(year, type_of_work) %>%
  summarise(n = n()) %>%
  mutate(proportion = n/sum(n))
```

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 24 × 4
    ## # Groups:   year [4]
    ##     year type_of_work                             n proportion
    ##    <dbl> <chr>                                <int>      <dbl>
    ##  1  2017 Addition / Alteration                 3133    0.465  
    ##  2  2017 Demolition / Deconstruction            877    0.130  
    ##  3  2017 New Building                          1527    0.227  
    ##  4  2017 Outdoor Uses (No Buildings Proposed)    11    0.00163
    ##  5  2017 Salvage and Abatement                 1113    0.165  
    ##  6  2017 Temporary Building / Structure          73    0.0108 
    ##  7  2018 Addition / Alteration                 3321    0.491  
    ##  8  2018 Demolition / Deconstruction            836    0.124  
    ##  9  2018 New Building                          1674    0.248  
    ## 10  2018 Outdoor Uses (No Buildings Proposed)     9    0.00133
    ## # ℹ 14 more rows

``` r
#' @title 
#' Grouped Count and Proportion
#' 
#' @description
#' 'group_count_prop' groups a dataframe by a variable of your choosing and find the counts and proportions of another variable of your choosing within those groups.
#' 
#' @param df Data frame of interest. Named df since this is short for data frame, which is the class required.
#' @param group_vars Categorical variable present in the data frame. It is used to group the data. Named 'group_vars' since it is the variable by which the data will be grouped.
#' @param var Categorical variable present in the data frame. It is the variable that will be counted and whose proportion will be calculated. Named 'var' since this is the variable that will be counted and whose proportion will be calculated.
#' 
#' @return
#' 'group_count_prop' returns the count and proportion of a categorical variable grouped by another categorical variable in a data frame. The output is a tibble, which has columns corresponding to the count (n) and proportion.

group_count_prop <- function (df, group_vars, var) {
  stopifnot(is.data.frame(df))
  df %>%
    group_by({{group_vars}}, {{var}}) %>%
    summarise(n = n()) %>%
    mutate(proportion = n/sum(n))
}
```

## Examples

### Example using building_permits data set

Using the group_count_prop function, I can group by year and find the
counts and proportions by type_of_work in the building_permits data
frame. The output tibble contains the year, and type_of_work variables
with their corresponding counts (n) and proportions.

``` r
permits_count_prop <- group_count_prop(building_permits, year, type_of_work)
```

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

``` r
print(permits_count_prop)
```

    ## # A tibble: 24 × 4
    ## # Groups:   year [4]
    ##     year type_of_work                             n proportion
    ##    <dbl> <chr>                                <int>      <dbl>
    ##  1  2017 Addition / Alteration                 3133    0.465  
    ##  2  2017 Demolition / Deconstruction            877    0.130  
    ##  3  2017 New Building                          1527    0.227  
    ##  4  2017 Outdoor Uses (No Buildings Proposed)    11    0.00163
    ##  5  2017 Salvage and Abatement                 1113    0.165  
    ##  6  2017 Temporary Building / Structure          73    0.0108 
    ##  7  2018 Addition / Alteration                 3321    0.491  
    ##  8  2018 Demolition / Deconstruction            836    0.124  
    ##  9  2018 New Building                          1674    0.248  
    ## 10  2018 Outdoor Uses (No Buildings Proposed)     9    0.00133
    ## # ℹ 14 more rows

### Example using penguins data set

On a different data frame, called penguins, I can group by species and
find the counts and proportions of sex in the data frame. The output
tibble contains the species, and sex variables with their corresponding
counts (n) and proportions. Since some entries are missing sex, it also
displays the count and proportion of missing entries where the sex
observation is not given (NA).

``` r
penguins_count_prop <- group_count_prop(penguins, species, sex)
```

    ## `summarise()` has grouped output by 'species'. You can override using the
    ## `.groups` argument.

``` r
print(penguins_count_prop)
```

    ## # A tibble: 8 × 4
    ## # Groups:   species [3]
    ##   species   sex        n proportion
    ##   <fct>     <fct>  <int>      <dbl>
    ## 1 Adelie    female    73     0.480 
    ## 2 Adelie    male      73     0.480 
    ## 3 Adelie    <NA>       6     0.0395
    ## 4 Chinstrap female    34     0.5   
    ## 5 Chinstrap male      34     0.5   
    ## 6 Gentoo    female    58     0.468 
    ## 7 Gentoo    male      61     0.492 
    ## 8 Gentoo    <NA>       5     0.0403

## Test

``` r
test_that("group_count_prop only accepts data frame", {
  expect_error(group_count_prop(test_vector, year, species)) #when the argument that is not a data frame is used in the function, an error is expected from the function
  expect_error(group_count_prop("building_permits", year, type_of_work)) #when the argument is coerced into a character class, an error is expected from the function
})
```

    ## Test passed 🌈

``` r
test_that("group_count_prop returns the expected number of columns with the expected names", {
  expect_length(group_count_prop(building_permits, year, type_of_work), 4) #when the function is used on a data frame, the expected number of variables in the output tibble is 4 (the variable the data is grouped by, the variable that is counted, the count, and the proportion)
  expect_named(group_count_prop(building_permits, year, type_of_work), c("year", "type_of_work", "n", "proportion")) #the expected names of the columns in the output tibble are 'year', 'type_of_work', 'n', and 'proportion'
})
```

    ## Test passed 😀

``` r
test_that("group_count_prop proportions equal 1", {
  expect_length(group_count_prop(penguins, species, sex) %>% summarise_at(vars(proportion), 
               list(species = sum)), 1) #the proportions that the function returns should add up to 1 within each group
})
```

    ## Test passed 🥳
