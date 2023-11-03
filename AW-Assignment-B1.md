Assignment B-1
================

# Load Relevant Packages

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

I want to make a function that bundles a ‘group_by() %\>% summarise()’
workflow. Specifically, my function, named group_count_prop, will take a
data frame and group by a categorical variable of interest. Then, it
will calculate the number of counts and proportion in each category of
another categorical variable.

``` r
#' @title 
#' Grouped Count and Proportion
#' 
#' @description
#' 'group_count_prop' groups a data frame by a categorical variable of your choosing and will find the counts and proportions of another categorical variable of your choosing across those groups.
#' 
#' @param df Data frame of interest. Named df since this is short for data frame, which is the class required as an argument for the function.
#' @param group_vars Categorical variable present in the data frame. It is used to group the data. Named 'group_vars' since it is the variable by which the data will be grouped.
#' @param var Categorical variable present in the data frame (different from the group_vars variable). It is the variable that will be counted and whose proportion will be calculated. Named 'var' since this is the variable that will be counted and whose proportion will be calculated.
#' 
#' @return
#' 'group_count_prop' returns the count and proportion of a categorical variable grouped by another categorical variable in a data frame. The output is a tibble, which has columns corresponding to the count (n) and proportion.

group_count_prop <- function (df, group_vars, var) {
  stopifnot(is.data.frame(df)) #stops the function from running if the df argument is not a data frame
  df %>%
    group_by({{group_vars}}, {{var}}) %>% #groups by group_vars variable
    summarise(n = n()) %>% #calculate counts of var variable
    mutate(proportion = n/sum(n)) #calculate proportions
}
```

## Examples

### Example using building_permits data set

Using the group_count_prop function, I can group by year and find the
counts and proportions of all different type_of_work categories in the
building_permits data frame. The output tibble contains the year, and
type_of_work variables with their corresponding counts (n) and
proportions.

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
find the counts and proportions of different sex categories in the data
frame. The output tibble contains the species, and sex variables with
their corresponding counts (n) and proportions. Since some entries are
missing sex, it also displays the count and proportion of missing
entries where the sex observation is not given (NA).

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

## Test the function

Here, I will write formal tests for my function using expect\_()
functions from the testthat package. I will test to ensure that my
function only accepts data frames, that my function returns the expected
and numbers of columns in the output tibble, and that my function
computes proportions that total to 1.

``` r
test_that("group_count_prop only accepts data frame", {
  expect_error(group_count_prop(test_vector, year, species)) #when the argument entered in the function is not a data frame, an error is expected from the function
  expect_error(group_count_prop("building_permits", year, type_of_work)) #when the argument is coerced into a character class, an error is expected from the function
})
```

    ## Test passed 🥳

``` r
test_that("group_count_prop returns the expected number of columns with the expected names", {
  expect_length(group_count_prop(building_permits, year, type_of_work), 4) #when the function is used, the expected number of variables in the output tibble is 4 (the variable the data is grouped by, the variable that is counted, the count, and the proportion)
  expect_named(group_count_prop(building_permits, year, type_of_work), c("year", "type_of_work", "n", "proportion")) #the expected names of the columns in the output tibble are 'year', 'type_of_work', 'n', and 'proportion'
})
```

    ## Test passed 🎉

``` r
test_that("group_count_prop proportions equal 1", {
  expect_length(group_count_prop(penguins, species, sex) %>% summarise_at(vars(proportion), 
               list(species = sum)), 1) #the proportions that the function returns should add up to 1 within each group
})
```

    ## Test passed 🌈
