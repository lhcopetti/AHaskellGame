#!/bin/bash

# If you attempt to run a GUI application using the docker container, it will most likely fail. 
# The reason behind that is because the container doesn't have access to any of the desktop servers 
# running on the host, like X.
# To allow the X server on your host machine to listen for incoming connections from within the container
# you can execute the below command:
# xhost +local:root

# ATTENTION: Please, don't forget to disable after you are done! 
# Leaving the connection open might put you in danger!

# To disable after you are done:
# xhost -local:root

sudo docker run -it --env="DISPLAY"  --volume="/tmp/.X11-unix:/tmp/.X11-unix:rw" copetti/haskell-game:1.0
