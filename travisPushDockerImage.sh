#!/bin/bash
echo "Pushing built images to DockerHub"

echo "Logging to DockerHub"
echo "$DOCKER_HUB_PASSWORD" | docker login --username "$DOCKER_HUB_LOGIN" --password-stdin

while true; do

    read imageName
    if [ $? -ne 0 ]; then
        break;
    fi

    echo "Pushing >>>$imageName<<< to DockerHub"
    docker push $imageName

done;

echo "Done pushing images"
exit 0;