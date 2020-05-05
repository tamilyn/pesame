
#' fileFormats - available formats
#'
fileFormats = c('text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain','.csv','.tsv',
                '.xlsx', '.xls')


#' significance.options - available significance options 
#'
significance.options <- list(
  "All"           = "All",
  "0.05"          = 0.05,
  "0.01"          = 0.01,
  "0.001"         = 0.001,
  "0.0001"        = 0.0001,
  "0.00001"       = 0.00001,
  "0.000001"      = 0.000001,
  "0.0000001"     = 0.0000001,
  "0.00000001"    = 0.00000001,
  "0.000000001"   = 0.000000001)


#' significance.options.default - default significance option
#'
significance.options.default = significance.options[3]

#' method.options.all - all method options 
#'
method.options.all = list("holm" = "holm",
     "hochberg" = "hochberg",
     "hommel" = "hommel",
     "bonferroni" = "bonferroni",
     "BH" = "BH",
     "BY" = "BY",
     "fdr" = "fdr",
     "none" = "none")

#' method.options - available method options 
#'
method.options = list( "bonferroni" = "bonferroni",
                      "fdr" = "fdr",
                      "none" = "none")

#' method.options.default - default method options 
#'
#' @export
method.options.default = "fdr"

#' input.options - input file options 
#'
input.options = list( "Excel" = "excel",
                      "CSV" = "csv",
                      "Rdata" = "rdata")

#' input.options.default - default input file option 
#'
input.options.default = "excel"


