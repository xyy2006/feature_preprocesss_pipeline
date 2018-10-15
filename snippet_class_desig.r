library(data.table)
library(purrr)
library(dplyr)


#: a function
class_designation <- function(dt, varNames) {
  # @desc: ths logic is we have all character type columns in the beginning (regardless of column values), 
  # if can coerce to numeric value, do it; otherwise(try and error/warning), coerce to factor. If error again, will stop,
  # then manually check is needed.
  #
  # @param dt: data.table object
  # @param varNames: string vector of colnames in the dt.
  # @return a modified dt[, varNames, with = FALSE] with coerced col class for each of the `varNames` specified.
  dt[ , map(.SD, ~try_catch(as.numeric(.), 
                            .e = function(e) return(as.factor(.)), 
                            .w = function(w) return(as.factor(.)))),
.SDcols = varNames]

}
# then manually assign the dt's varNames columns with returned new values (after type conversion)                            


#: a function                              
continuous_binnedCategorical <- function(dt, varNames, n_levels = 10, 
                                         cut_func = c(ggplot2::cut_number, ggplot2::cut_interval)) {
  # @desc: ths logic is at first check if those cols of varNames are numeric and integer columns,
  # then decide if distinct value > n_levels, if so bin them to n_levels ordered factors, otherwise keep the levels asIs and coerce to ordered factors. 
  #
  # @param dt: data.table object
  # @param varNames: string vector of colnames in the dt.
  # @param n_levels: an integer specifying how many levels you want.
  # @param cut_func: which cut function to use. `cut_interval` makes 'n' groups with equal range, `cut_number`
  #  makes 'n' groups with (approximately) equal numbers of observations.
  # @return a modified dt with numeric and integer columns coerced to ordered factors (might be binned)
  stop_if_not(dt[, map(.SD, ~(is.numeric(.)|is.integer(.))), .SDcols = varNames] %>% unlist %>% all, 
              msg = "Provided `varNames` cols in `dt` are not all numeric or integer type!")

  #: DONT use `ifelse`, doesnt work for vector here.
  #dt[ , map_dfc(.SD, ~ifelse(n_distinct(.) <= n_levels, yes = as.factor(.), no = ggplot2::cut_number(., n = n_levels )))
  #  , .SDcols = varNames]



  #: use standard if-else syntax.
  dt[ , .SD, .SDcols = varNames] %>% map_dfc(., ~if(n_distinct(.) <= n_levels) { as.ordered(.)} 
                                             else { 
                                               #: if error or warning, use base::cut function to be safe.
                                               try_catch(cut_func(., n = n_levels, ordered_result = TRUE ), 
                                                         .e = function(e) return( base::cut(., breaks = n_levels, ordered_result = TRUE) ), 
                                                         .w = function(w) return(base::cut(., breaks = n_levels, ordered_result = TRUE)))
                                             })
  # or equivalently, 
  #dt[ , map_dfc(.SD, ~if(n_distinct(.) <= n_levels) { as.factor(.)} else { ggplot2::cut_number(., n = n_levels )}), .SDcols = varNames]


}                              



