
<!-- README.md is generated from README.Rmd. Please edit that file -->

# farrago

<!-- badges: start -->
<!-- badges: end -->

{farrago} is an R package serving as a collection of tools for data
workflows and analysis, with focus on health surveillance data. Although
{farrago} primarily serves as a personal collection of odds-and-ends
picked-up or created over the past several years, it may assist wider
audiences as well. The package is organized by general
purpose/functionality, which may eventually be separated into discrete
packages.

## Installation

{farrago} is only available from [GitHub](https://github.com/) and the
latest version can be installed with:

``` r
# install.packages("devtools")
devtools::install_github("al-obrien/farrago")
```

## Use-cases

{farrago} has a variety of functions available. They are roughly
organized into the following categories:

1.  **Calculations:** provides algorithms for some routine processes
    such as…

    -   Determining pregnancy trimesters

    -   Assigning episode periods (e.g. for repeat infections)

    -   Collapsing time-steps

    -   Determining overlaps in time

    -   Basic metrics such as rates, max/min, etc.

2.  **Conversions:** helper functions to convert between common formats
    in epidemiology

    -   Replace all blank values to NA (e.g. when importing data from
        SAS)

    -   Switch between flu and calendar weeks

    -   Determine flu season from date

    -   Quickly convert a table to image (png)

    -   Basic conversions such as from numbers to percent, number to
        factor, etc.

3.  **Creation:** generate new content

    -   Make multi-level factors similar to SAS ‘multi-label’
        functionality

    -   Create hypercubes (i.e. n-dimensional table including group
        summaries and totals)

    -   Determine break points from set of values

4.  **Transferal:** methods to move objects and data

    -   Easily `stow()` and `retrieve()` data-sets to make efficient use
        of RAM

    -   File transfer using WinSCP wrapper

    -   Locate files

    -   Pass code and retrieve data from SAS (primarily for use with
        Classic 9.4)

5.  **Plotting:** helper functions for shared legends and less common
    plots such as bulls-eye charts and X-splines

6.  **Miscellaneous**

## Example

This is a basic example using a sub-set of functions from {farrago}…

``` r
# Load libraries
library(farrago)
library(magrittr)
library(dplyr)
library(lubridate)
```

``` r
# Download from configured SFTP location
transfer_winscp(file ='my_rmt_file.csv'),
               direction = 'download',
               connection = 'sftp://myusername:mypwd@hostlocation.ca/'
               rmt_path = './location/',
               drop_location = 'C:/PATH/TO/DESIRED/FOLDER/')
```

``` r
# Non-sense data for example
my_rmt_file <- tibble::tribble(~grp_id, ~date, ~date_of_birth, ~condition, ~date_of_birth_child, 
                               1, '2020-01-01', '1970-06-04', 'alive', '1991-01-01',
                               1, '2020-01-01', '1980-04-05', '', '1990-02-04',
                               1, '2020-01-03', '1930-04-05', 'alive', '',
                               1, '2020-01-04', '1967-04-05', 'alive', '1998-01-21',
                               2, '2020-01-01', '1978-04-05', 'alive', '1998-06-21',
                               2, '2020-09-10', '1970-04-05', 'alive', '1992-09-13',
                               2, '2020-09-21', '1949-04-05', 'dead', '1987-01-03',
                               3, '2020-01-01', '1977-04-05', '', '1992-01-21',
                               3, '2020-01-02', '1944-04-05', 'alive', '',
                               3, '2020-01-21', '1943-06-05', 'alive', '1967-09-12',
                               3, '2020-01-22', '1969-07-05', 'alive', '2006-12-21',
                               3, '2020-04-22', '', NA, NA,
                               3, '2021-06-09', '1978-09-21', 'dead', '1992-01-21') %>%
  dplyr::mutate_at(vars(contains('date')), ymd)

# Remove blanks
my_rmt_file <- convert_blank2NA(my_rmt_file)

# Determine episode period based on first date by group
my_rmt_file$episode <- assign_episode(data = my_rmt_file,
                                      grp_id = grp_id,
                                      date = date,
                                      threshold = 10)

# Determine age and age group from date
my_rmt_file$age <- calculate_age(my_rmt_file$date_of_birth)
my_rmt_file$age_grp <- create_breaks(my_rmt_file$age, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90), format = TRUE)
 
# Calculate trimester based on dob (of child)
my_rmt_file <- calculate_trimesters(my_rmt_file, date_of_birth_child)
#> Warning in calculate_trimesters(my_rmt_file, date_of_birth_child): No variable
#> for gestation length was provided, all pregnancies will assume the average
#> pregnancy length of: 40

# View final dataset
knitr::kable(my_rmt_file)
```

| grp\_id | date       | date\_of\_birth | condition | date\_of\_birth\_child | episode | age | age\_grp | tri1\_s    | tri1\_e    | tri2\_s    | tri2\_e    | tri3\_s    | preterm |
|--------:|:-----------|:----------------|:----------|:-----------------------|--------:|----:|:---------|:-----------|:-----------|:-----------|:-----------|:-----------|--------:|
|       1 | 2020-01-01 | 1970-06-04      | alive     | 1991-01-01             |       1 |  51 | 50-59    | 1990-03-27 | 1990-06-26 | 1990-06-25 | 1990-09-24 | 1990-09-23 |       0 |
|       1 | 2020-01-01 | 1980-04-05      | NA        | 1990-02-04             |       1 |  41 | 40-49    | 1989-04-30 | 1989-07-30 | 1989-07-29 | 1989-10-28 | 1989-10-27 |       0 |
|       1 | 2020-01-03 | 1930-04-05      | alive     | NA                     |       1 |  91 | &gt;=90  | NA         | NA         | NA         | NA         | NA         |      NA |
|       1 | 2020-01-04 | 1967-04-05      | alive     | 1998-01-21             |       1 |  54 | 50-59    | 1997-04-16 | 1997-07-16 | 1997-07-15 | 1997-10-14 | 1997-10-13 |       0 |
|       2 | 2020-01-01 | 1978-04-05      | alive     | 1998-06-21             |       1 |  43 | 40-49    | 1997-09-14 | 1997-12-14 | 1997-12-13 | 1998-03-14 | 1998-03-13 |       0 |
|       2 | 2020-09-10 | 1970-04-05      | alive     | 1992-09-13             |       2 |  51 | 50-59    | 1991-12-08 | 1992-03-08 | 1992-03-07 | 1992-06-06 | 1992-06-05 |       0 |
|       2 | 2020-09-21 | 1949-04-05      | dead      | 1987-01-03             |       3 |  72 | 70-79    | 1986-03-29 | 1986-06-28 | 1986-06-27 | 1986-09-26 | 1986-09-25 |       0 |
|       3 | 2020-01-01 | 1977-04-05      | NA        | 1992-01-21             |       1 |  44 | 40-49    | 1991-04-16 | 1991-07-16 | 1991-07-15 | 1991-10-14 | 1991-10-13 |       0 |
|       3 | 2020-01-02 | 1944-04-05      | alive     | NA                     |       1 |  77 | 70-79    | NA         | NA         | NA         | NA         | NA         |      NA |
|       3 | 2020-01-21 | 1943-06-05      | alive     | 1967-09-12             |       2 |  78 | 70-79    | 1966-12-06 | 1967-03-07 | 1967-03-06 | 1967-06-05 | 1967-06-04 |       0 |
|       3 | 2020-01-22 | 1969-07-05      | alive     | 2006-12-21             |       2 |  52 | 50-59    | 2006-03-16 | 2006-06-15 | 2006-06-14 | 2006-09-13 | 2006-09-12 |       0 |
|       3 | 2020-04-22 | NA              | NA        | NA                     |       3 |  NA | NA       | NA         | NA         | NA         | NA         | NA         |      NA |
|       3 | 2021-06-09 | 1978-09-21      | dead      | 1992-01-21             |       4 |  43 | 40-49    | 1991-04-16 | 1991-07-16 | 1991-07-15 | 1991-10-14 | 1991-10-13 |       0 |

``` r
# Save file for easy retrieval later
my_rmt_file_stowed <- stow(my_rmt_file, cleanup = TRUE)
```
