#!/bin/sh
echo Running "nilqed/jfricas:latest"
echo Use docker commit if you want to save your changes!
echo Warning: using xhost local:root
xhost local:root
docker run -ti --network=host --env DISPLAY=$DISPLAY nilqed/jfricas:latest jupyter notebook --no-browser --allow-root
docker ps -a
xhost -local:root
echo done.
