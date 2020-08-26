FROM rocker/shiny-verse:4.0.2
RUN mkdir /build_zone
ADD files /build_zone
WORKDIR /build_zone
RUN Rscript -e 'install.packages(c("Formula","Hmisc","bsplus","futile.logger", "shinyjs", "plotly", "thinkr", "shinyalert", "kableExtra", "config", "golem", "skimr"))'
RUN Rscript -e 'install.packages("golemshinyapp_0.0.1.1.tgz", repos=NULL, type="source")'
CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');golemshinyapp::run_app()"
