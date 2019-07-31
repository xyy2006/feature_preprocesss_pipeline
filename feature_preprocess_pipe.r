 #: --- assign value for "contin_part_dt" (continuous, i.e., numeric and integer, cols)--------
  # assign median (or mean), here use median.
  for (var in contin_var_withNAs_names) {
    
    set(dt, i = which(is.na(dt[[var]])), j = var, value =  0)
    message("+ ", appendLF = FALSE)
  }
  
   
  #: --- assign value for "catg_part_dt" (categorical cols)--------
  # assign "No test available".
  #: memory efficient version, replace in place, serial.
  for (var in catg_var_withNAs_names) {

    message("+ ", appendLF = FALSE)
    #temp <- temp %>% unlist
    temp <- dt[[var]]
    #head(temp) %>% print
    levels(temp) <- c(levels(temp), "No test available")
    temp[is.na(temp)] <- "No test available"
    set(dt, i = NULL, j = var, value = temp )

  }

  
#: ---- convert logical to numeric for later modeling purpose----
# (algorithm can be approved, overhead in [.data.table still exists in `set(..., value = xxxxxxxx)` part)
logical_class_index <- dt %>% sapply(., function(x) is(x, "logical")) %>% which
message("Logical class features have: ", logical_class_index %>% length)
noReturn <- foreach( index = logical_class_index) %do% {
	message("convert logical to integer on index: ", index)
	set(dt, j = index, value = dt[, index, with = FALSE] %>% unlist %>% as.numeric )
	return(NULL)
}  



#: ----convert some character to factor/integer, such as gender----
character_colnames_chr <- dt %>% map_lgl(., ~inherits(., "character")) %>% which %>% colnames(dt)[.]
message("Character class features have: ", character_colnames_chr %>% length)

#: confirm it is character, not able to be dbl.
# dt[, character_colnames_chr, with = FALSE]
for (vN in character_colnames_chr) {
      set(dt, i = NULL, j = vN, value = as.factor(dt[[vN]]))
}
dt %>% sapply(., class) %>% table # as validation.

#---now filter on var == 0 on all non-character columns, split for numeric/integer and factor respectively----------
numeric_index <- dt %>% map_lgl(., ~inherits(., c("integer", "numeric"))) %>% which
factor_index <- dt %>% map_lgl(., ~inherits(., "factor")) %>% which
#
message("Numeric_index features have: ", numeric_index %>% length)
message("Factor_index features have: ", factor_index %>% length)

var_vec <- dt[, sapply(.SD, stats::var, na.rm = TRUE), .SDcols = numeric_index]
#: update these cols to NULL (i.e., remove these cols from dt) 
dt[ , names(var_vec)[which(var_vec == 0)] := NULL]

var_vec <- dt[, sapply(.SD, function(x) all(duplicated(x)[-1L])), .SDcols = factor_index] # check all the same factor for the column
#: update these cols to NULL (i.e., remove these cols from dt) 
dt[ , names(var_vec)[var_vec] := NULL]
