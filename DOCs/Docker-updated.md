Docker Stack Integration Update (01-06-2018)


After finishing the docker integration I was after since I began wondering if docker was capable of containerizing a desktop application I realized that there was a stack + docker integration to be explored.

According to its documentation [https://docs.haskellstack.org/en/stable/docker_integration/]([1])[https://docs.haskellstack.org/en/stable/GUIDE/#docker]([2]), it has two built-ins docker integrations:

-  "stack has support for automatically performing builds inside a Docker container, using volume mounts and user ID switching to make it mostly seamless". This basically is the development image I had built, however, all the things I had to take care: .stack-work directory mapping, "--allow-different-user" flags and have to manually run a script reach the stack inside the image were all coming for free!

- "stack can also generate Docker images for you containing your built executables.". This one refers to the deployment image I built with the docker builder pattern, by simply building the application in one container and copying the executables to another image (which contains only the runtime dependencies).
