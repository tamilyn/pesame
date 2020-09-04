#!/bin/sh
docker run -p 3838:3838 alekseyenko/pesame &
open http://0.0.0.0:3838
