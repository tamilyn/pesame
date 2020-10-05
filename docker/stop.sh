#!/bin/sh

docker kill `docker ps | grep alekseyenko/pesame | cut -d' ' -f1`
