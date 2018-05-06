#!/bin/bash

docker run                                          \
    --runtime=nvidia                                \
    -it                                             \
    --rm                                            \
    --env="STACK_ROOT=/app/"                        \
    --env="QT_X11_NO_MITSHM=1"                      \
    --env="DISPLAY=unix$DISPLAY"                    \
    --volume="/tmp/.X11-unix:/tmp/.X11-unix:rw"     \
    -v "$(pwd):/app/"                               \
    -w "/app/"                                      \
    lhcopetti/haskell-game:dev                      \
    /bin/bash