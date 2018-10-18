#: --- assign value for "catg_part_dt" (categorical cols)--------
# assign "missing_value".
#: memory efficient version, replace in place, serial.
for (var in catg_var_withNAs_names) {

  message("+ ", appendLF = FALSE)
  #temp <- temp %>% unlist
  temp <- joined_dt[[var]]
  #head(temp) %>% print
  levels(temp) <- c(levels(temp), "missing_value")
  temp[is.na(temp)] <- "missing_value"
  set(joined_dt, i = NULL, j = var, value = temp )

}

# or

# #: parallel, faster with spread on 29 cores, but need more memory.
# catg_part_dt <- foreach (temp = joined_dt[, catg_var_withNAs_names, with = FALSE], .inorder = TRUE, .combine = cbind) %dopar% {

#     #message("+ ", appendLF = FALSE)
#     #temp <- temp %>% unlist
#     #temp <- joined_dt[ , var, with = FALSE] %>% unlist 
#     #head(temp) %>% print
#     levels(temp) <- c(levels(temp), "missing_value")
#     temp[is.na(temp)] <- "missing_value"
#     #set(joined_dt, i = NULL, j = var, value = temp )
#     #head(temp) %>% print
#     return(temp %>% as.data.table)

# }

# #: then assign collected data.table back to `joined_dt`s corresponding colnames.
# joined_dt[, (catg_var_withNAs_names) := catg_part_dt]




#: --- assign value for "contin_part_dt" (continuous, i.e., numeric and integer, cols)--------
# assign median (or mean), here use median.
for (var in contin_var_withNAs_names) {

  set(joined_dt, i = which(is.na(joined_dt[[var]])), j = var, value =  joined_dt[[var]] %>% median(., na.rm = TRUE))
  message("+ ", appendLF = FALSE)
}
