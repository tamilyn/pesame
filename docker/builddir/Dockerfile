FROM rocker/shiny-verse
RUN mkdir /build_zone
ADD files /build_zone
WORKDIR /build_zone
RUN Rscript -e 'install.packages(c("Hmisc","bsplus","futile.logger", "shinyjs", "plotly", "thinkr", "shinyalert", "kableExtra", "config", "golem", "skimr"))'
RUN Rscript -e 'install.packages("golemshinyapp_0.0.1.0.tgz", repos=NULL, type="source")'
#EXPOSE 3838
