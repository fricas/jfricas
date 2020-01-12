#!/bin/sh
sudo docker rmi nilqed/jfricas:latest
sudo docker build -t nilqed/jfricas:latest . > log
sudo docker images
