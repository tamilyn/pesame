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

#' uniqvals 
#'
#' @import purrr
#' @import stringr 
#' @import dplyr 
#' @import tidyr 
#' @import glue 
uniqvals <- function(f) {

  u_vals <- unique(f)
  num_unique <- length(u_vals)
  ready <- (num_unique == 2)
  type <- class(u_vals)

  labels <- "FALSE,TRUE"
  true_label <- "TRUE"

  uu_str <- glue_collapse(u_vals, ", ")
  if(num_unique == 2) {
     labels <- str_c(u_vals, collapse = ",")
     true_label <- u_vals[2]
  }

  description <- uniq_vals_description(f)
  new_df <- tibble(num_unique = num_unique,
                   unique_values = uu_str,
                   method_applied = "none",
                   type = type,
                   description = description,
                   labels = labels,
                   true_label = true_label)
}


#' identifyFactors
#'
#' @import purrr
#' @import stringr 
#' @import dplyr 
#' @import tidyr 
identifyFactors <- function(df) {
  dfcounts <- df %>% summarise(across(everything(), n_distinct))
  dfcols <- dfcounts %>% pivot_longer(everything())

  cols <- dfcols %>% filter(value == 2) %>% pull(name)
  factors_df <- map_df(cols, ~ uniqvals(pull(df,.))) %>%
    mutate(name = cols, ready = TRUE) 

  othercols <- dfcols %>% filter(value != 2) %>% pull(name)
  nonfactors_df <- map_df(othercols, ~ uniqvals(pull(df,.))) %>%
    mutate(name = othercols, ready = FALSE) 

  both <- bind_rows(factors_df, nonfactors_df)
}
