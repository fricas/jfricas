#!/bin/sh
echo Testing "nilqed/jfricas:latest"
docker run -ti --rm --network=host --env DISPLAY=:0 nilqed/jfricas:latest jupyter notebook --no-browser --allow-root
docker ps -a
echo done.


