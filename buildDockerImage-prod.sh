#!/bin/bash

currentVersion="$(cat src/version.ver)"

imageName="lhcopetti/haskell-game:lts-${currentVersion}"

# Building the versioned image version (eg: lts-1.0)
docker build -t "$imageName" -f DockerfileProduction .

# Building the non-versioned image (eg: lts)
# The intent is so that the lts always points to the most recent master build
docker build -t "${imageName%-*}" -f DockerfileProduction .

## More information on the string substitution used above at: "http://wiki.bash-hackers.org/syntax/pe"
