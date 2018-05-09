#!/bin/bash

currentVersion="$(cat src/version.ver)"
imageName="lhcopetti/haskell-game:lts-${currentVersion}"
docker build -t "$imageName" -f DockerfileProduction .