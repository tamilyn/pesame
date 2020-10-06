#' uniq_vals_description
#'
#' @import purrr
#' @import stringr 
#' @import dplyr 
#' @import futile.logger
uniq_vals_description <- function(ff1) {

  ## remove NA's
  ff <- ff1[!is.na(ff1)]
  uu <- unique(ff)
  if(class(uu)=="numeric") {
    return(str_c("#unique: ", length(uu)))
  }
  ss <- str_c(uu, collapse = ",")
  if(!length(ss)) {
    flog.error("nothing in this string")
    flog.error(uu)
    return(NULL)
  }
  if (length(ss) && nchar(ss) > 20) {
    ss <- str_c( str_sub(ss, 1, 16), "...")
  }
  ss
}

#' identifyFactors
#'
#' @import purrr
#' @import stringr 
#' @import dplyr 
#' @import tidyr 
identifyFactors <- function(df) {

  dfcounts <- df %>% summarise(across(everything(), n_distinct))
  dfcols <- dfcounts %>% pivot_longer(everything()) %>%
     rename(num_values = value)

  dftypes <- df %>% summarise(across(everything(), class))
  dfcoltypes <- dftypes %>% pivot_longer(everything()) %>%
     rename(type = value)

  dfu <- df %>% summarise(across(everything(), uniq_vals_description))
  dfuvals <- dfu %>% pivot_longer(everything()) %>%
     mutate(description = value) %>%
     rename(unique_values = value)

  all <- dfcols %>% 
   left_join(dfcoltypes, by="name") %>% 
   left_join(dfuvals, by="name") %>% 
   mutate(ready = (num_values == 2), 
          labels = "FALSE,TRUE",
          true_label = "TRUE",
          false_label = "FALSE",
          level0_values = "",
          method_applied = "none")


  all 
}
