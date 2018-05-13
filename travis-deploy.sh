#!/bin/bash -e

echo "Building the Docker Image | Release Version"

./buildDockerImage-prod.sh

echo "Logging to DockerHub"

echo "$DOCKER_HUB_PASSWORD" | docker login --username "$DOCKER_HUB_LOGIN" --password-stdin

currentVersion="$(cat src/version.ver)"

versionedImage="lhcopetti/haskell-game:lts-${currentVersion}"
echo "Pushing versioned image: $versionedImage"
docker push $versionedImage

unversionedImage="${imageName%-*}"
echo "Pushing unversioned image: $unversionedImage"
docker push "$unversionedImage"