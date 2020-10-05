#!/bin/sh
docker run -p 3838:3838 alekseyenko/pesame &
sleep 5
open http://0.0.0.0:3838
