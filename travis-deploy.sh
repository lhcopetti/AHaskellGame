#!/bin/bash -e

echo "Building the Docker Image | Release Version"

./buildDockerImage-prod.sh

echo "Logging to DockerHub"

echo "$DOCKER_HUB_PASSWORD" | docker login --username "$DOCKER_HUB_LOGIN" --password-stdin

baseImageName="lhcopetti/haskell-game:lts"

currentVersion="$(cat src/version.ver)"
versionedImageName="${baseImageName}-${currentVersion}"

echo "Pushing image with name: $versionedImageName"
docker push $versionedImageName

echo "Pushing image with name: $baseImageName"
docker tag "$versionedImageName" "$baseImageName"
docker push "$baseImageName"