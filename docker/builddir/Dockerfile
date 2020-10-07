FROM rocker/shiny-verse:4.0.2
RUN Rscript -e 'install.packages(c("devtools","Formula","Hmisc","bsplus","futile.logger", "shinyjs", "plotly", "thinkr", "shinyalert", "kableExtra", "config", "golem", "skimr"))'
RUN Rscript -e 'library(devtools); devtools::install_github("alekseyenko/pesame")'
CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');pesame::run_app()"
