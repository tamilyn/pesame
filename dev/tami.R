library(golem)

golem::add_module( name = "outernav" ) # Name of the module
golem::add_module( name = "loadmodule" ) # Name of the module
golem::add_module( name = "load_factors" )


golem::add_module( name = "factors" ) # Name of the module
golem::add_module( name = "analyze" )

usethis::use_package( "plotly" )
usethis::use_package("bsplus")
usethis::use_package("shinyjs")
usethis::use_package("shinyalert")
usethis::use_package("skimr")
usethis::use_package("kableExtra")
usethis::use_package("Hmisc")
usethis::use_package("futile.logger")
usethis::use_package('stringr')

golem::add_js_handler( "handlers" )
golem::add_css_file( "styles" )
