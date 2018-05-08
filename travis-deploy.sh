#!/bin/bash -e

echo "Building the Docker Image | Release Version"

./buildDockerImage-prod.sh

echo "Logging to DockerHub"

echo "DOCKER_HUB_PASSWORD" | docker login --username "DOCKER_HUB_LOGIN" --password-stdin

currentVersion="$(cat src/version.ver)"
imageName="lhcopetti/haskell-game:lts-${currentVersion}"

echo "Pushing image with name: $imageName"
docker push $imageName