#: Content calculate the entropy (according to the information contained in the variable itself). A high entropy is preferred as more information is carried in the variable.


library(entropy)


#: for debug, use errorhanliding = "pass", verbose = TRUE, and .combine = list.
entropy_dbl <- foreach(col = joined_dt %>% select(., 
                                                  matches("cig|HOME_ADDR_STE_CD|MEMBR_AGE|GENDER_CD_M|x_lob|x_product"))
, .combine = c, .errorhandling='stop', .verbose = FALSE) %dopar% {

  message(class(col))

  if (inherits(col, c("factor", "character"))) {

    y <- table(col, useNA = "ifany")
    en <- entropy(y)

  } else {

    y <- discretize(col, numBins = 10, r=range(col, na.rm = TRUE)) %>% table(., useNA = "ifany")
    en <- entropy(y)

  }

  return(en)
}

#: render the names
names(entropy_dbl) <- colnames(joined_dt %>% select(., matches("cig|HOME_ADDR_STE_CD|MEMBR_AGE|GENDER_CD_M|x_lob|x_product")))

#: build the dt object, later convenient for subset.
entropy_dt <- entropy_dbl %>% as.data.table(., keep.rownames = TRUE )

setkey(entropy_dt, rn )

setnames(entropy_dt, c("rn", "entropy"))

entropy_dt
