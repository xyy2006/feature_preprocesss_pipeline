#: bring in those significant variables

result_isna_association_pvalue <- foreach( vn = ses_variable_names, .combine = c) %do% {

  temp_value <- joined_dt[[vn]]

  temp_value_binary <- ifelse(is.na(temp_value), yes = 0, no = 1)

  message(mean(temp_value_binary))

  if (n_distinct(temp_value_binary) == 2) {

    set(joined_dt, i = NULL, j = sprintf("%s_isNA_factor", vn), value = temp_value_binary %>% as.factor)

  }


  message(vn, "finished!")


  return (NULL)



}
