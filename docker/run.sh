#!/bin/sh
echo Running "nilqed/jfricas:latest"
echo Use docker commit if you want to save your changes!
docker run -ti --network=host --env DISPLAY=$DISPLAY nilqed/jfricas:latest jupyter notebook --no-browser --allow-root
docker ps -a
echo done.


