#!/bin/bash

echo "Building docker image"
sudo docker build -t copetti/haskell-game:1.0 .
