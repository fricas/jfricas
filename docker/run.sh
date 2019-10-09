#!/bin/sh
echo Testing "nilqed/jfricas:latest"
echo Container name is: jfc
echo Use docker commit if you want to save your changes!
docker run -ti --name jfc --network=host --env DISPLAY=:0 nilqed/jfricas:latest  jupyter notebook --no-browser --allow-root
docker ps -a
echo done.


