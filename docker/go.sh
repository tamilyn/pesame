#!/bin/sh

D=`pwd`

docker run -d --rm -p 3838:3838 \
    --name shinypesame \
    -v ${D}/srv/shinyapps/:/srv/shiny-server/ \
    -v ${D}/srv/shinylog/:/var/log/shiny-server/ \
    shinypesame 
