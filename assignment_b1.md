### Function: Percentage above mean

    #' Percentage above mean
    #' This function calculates the percentage of a vector's values that
    #' are above that vector's own mean.
    #' 
    #'@details
    #' The mean itself can be modified by passing arguments (like `trim`)
    #' via the `...` argument. The function first calculates the `total_mean`
    #' (which could be a trimmed mean) and then finds the percentage of
    #' all original values above that mean.
    #' 
    #'@param x A numeric vector.
    #'@param na.rm Remove NAs if applicable. 
    #'@param ... Additional arguments passed on to the `base::mean()` function.
    #'   An example is that it could be used to pass a `trim` value.
    #'
    #'@return A single numeric value (the percentage).
    #'@export
    #'
    #'@examples
    #' my_vector <- c(1, 2, 3, 10, 100)
    #'
    #' # 1/5 values are above -> 20%
    #' perc_above_mean(my_vector)
    #' 
    #'

    perc_above_mean <- function(x, na.rm = TRUE,...) { 
      # Added the ellipse to allow the function to pass unnamed functions.
      
      # Check that the input vector is numeric. 
      stopifnot("Input vector must be numeric!" = is.numeric(x))
      
      # Remove NAs if requested
      if (na.rm) {
        x <- x[!is.na(x)]
      }
      
      # Make sure the vector count is above zero.
      total_count <- length(x)
      if (total_count == 0) {
        return(0)
      }
      
      # Calculate the vector's mean. The ellipse is added here to allow for the mean function to pass any unnamed functions. 
      total_mean <- mean(x,...) 
      
      # Count how many values are above the mean
      count_above <- sum(x > total_mean)
      
      # Calculate and return the percentage
      percent_above <- (count_above / total_count) * 100
      
      return(percent_above)
    }

### Examples

## Example 1

    # Example 1: We are working with vector x. We ensure that the vector is assigned to x and then apply the function. 
    x <- c(1:20,35:120,99)
    perc_above_mean(x)

    ## [1] 52.33645

## Example 2

    # Example 2: We are working with the dataset penguins from palmerpenguins. 
    # We would like to look at the distribution of larger penguins across the islands.

    library(tidyverse)

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.2
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

    library(palmerpenguins)

    ## 
    ## Attaching package: 'palmerpenguins'
    ## 
    ## The following objects are masked from 'package:datasets':
    ## 
    ##     penguins, penguins_raw

    data(penguins)
    # Call the libraries for tidyverse, palmerpenguins, penguins if it it not already loaded

    island_stats <- penguins %>% # Use piping to apply the dataset and group by island
      group_by(island) %>%
      summarize(
        perc_above_m_flipper = round(perc_above_mean(flipper_length_mm),1),
        perc_above_m_mass = round(perc_above_mean(body_mass_g),1)
      ) # Round the percentage to the first decimal and assign to the summarised variable.
    # In this case, I choose to only look at two variables, flipper length and body mass. 

    head(island_stats)

    ## # A tibble: 3 × 3
    ##   island    perc_above_m_flipper perc_above_m_mass
    ##   <fct>                    <dbl>             <dbl>
    ## 1 Biscoe                    65.3              55.1
    ## 2 Dream                     45.2              45.2
    ## 3 Torgersen                 43.1              43.1

## Example 3

    # Example 3: We showcase an example that does not work. 

    species_stats <- penguins %>% # Use piping to apply the dataset and group by island
      group_by(species) %>%
      summarize(
        perc_above_m_islands = round(perc_above_mean(island),1),
        perc_above_m_mass = round(perc_above_mean(body_mass_g),1)
      ) # Round the percentage to the first decimal and assign to the summarised variable.

    ## Error in `summarize()`:
    ## ℹ In argument: `perc_above_m_islands = round(perc_above_mean(island),
    ##   1)`.
    ## ℹ In group 1: `species = Adelie`.
    ## Caused by error in `perc_above_mean()`:
    ## ! Input vector must be numeric!

    # In this case, I choose to only look at two variables, island and body mass. 
    # However, island is a categorical variable and you cannot calculate the mean. 
    # Thus the error ouput is given.
    head(species_stats)

    ## Error: object 'species_stats' not found

### Testing the function using testthat

    library(testthat)

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null

    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

    test_that("Perc_above_mean works!", {
      expect_equal(perc_above_mean(c(1,2,3,0)), 50)
      expect_equal(round(perc_above_mean(c(1:97)),1), 49.5)
      expect_equal(perc_above_mean(c(33,4,22,100)), 25)
      expect_equal(perc_above_mean(c(-1,3,50,89,-44)),40)
      expect_equal(perc_above_mean(c(NA,47,30,88,22)),50)
      expect_equal(perc_above_mean(c(0,0)),0)
    })

    ## Test passed 🥇
