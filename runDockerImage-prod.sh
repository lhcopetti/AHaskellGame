#!/bin/bash
docker run                                      \
    --rm                                        \
    --runtime=nvidia                            \
    --env="DISPLAY=unix$DISPLAY"                \
    --volume="/tmp/.X11-unix:/tmp/.X11-unix:rw" \
    lhcopetti/haskell-game:lts