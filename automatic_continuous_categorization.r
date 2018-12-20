#: 1, generate continuous gap variable
data_dt[ , gap_between_inquiry_n_accept := difftime(ts_accepted_at_first,ts_interaction_first, units = "hours") %>% as.numeric]

data_dt[ , gap_between_reply_n_accept := difftime(ts_accepted_at_first,ts_reply_at_first, units = "mins")%>% as.numeric]

data_dt[ , gap_between_accept_n_booking := difftime(ts_booking_at,ts_accepted_at_first, units = "secs")%>% as.numeric]

data_dt[ , gap_between_inquiry_n_checkin := difftime(ds_checkin_first,ts_interaction_first, units = "days") %>% as.numeric]

data_dt[ , length_of_stay := difftime(ds_checkout_first,ds_checkin_first, units = "days")%>% as.numeric]

#: 2, investigate range and spreadness of gap
foreach (var = c("gap_between_inquiry_n_reply", "gap_between_inquiry_n_accept", "gap_between_inquiry_n_checkin","gap_between_reply_n_accept","gap_between_accept_n_booking", "length_of_stay")) %do% {

  var_value = data_dt[, var, with = FALSE] %>% unlist %>% as.numeric

  message("look at var:", var)
  sum(is.na(var_value)) %>% message("na values: ",.)

  var_value %>% length  %>% message("length: ",.)

  message("Range is: ")
  var_value %>% range(., na.rm = TRUE) %>% cat

  sum(var_value <= 1, na.rm = TRUE) %>% message("# of <= 1: ",.)

  sum(var_value <= 5, na.rm = TRUE) %>% message("# of <= 5: ",.)

  sum(var_value <= 10, na.rm = TRUE) %>% message("# of <= 10: ",.)

  sum(var_value <= 120, na.rm = TRUE) %>% message("# of <= 120: ",.)


  catt("================")

}

#: 3, generate equal sized bins, preferrable from more # of bins to less # of bins, automatically
foreach (var = c("gap_between_inquiry_n_reply", "gap_between_inquiry_n_accept", "gap_between_inquiry_n_checkin","gap_between_reply_n_accept","gap_between_accept_n_booking", "length_of_stay")) %do% {

  var_value = data_dt[, var, with = FALSE] %>% unlist %>% as.numeric

  message("look at var:", var)

  var_value[is.na(var_value)] = max(var_value, na.rm = TRUE) # assign missing value to be the maximal outlier, then categorize

  n_bin = 6
  var_value_binned <- try(ggplot2::cut_number(var_value, n_bin))

  while (inherits(var_value_binned, "try-error") & n_bin > 2) {
    # dynamic assign equal sized bins, try from 6 to 2.
    n_bin = n_bin - 1
    var_value_binned <- try(ggplot2::cut_number(var_value, n_bin))

  }

  message(levels(var_value_binned))

  # if at least n_bin >= 2, then generate the new variables xxxx_binned.
  if (!inherits(var_value_binned, "try-error")) {
    data_dt[, sprintf("%s_binned", var) := var_value_binned]
  }



  message("================")

}
