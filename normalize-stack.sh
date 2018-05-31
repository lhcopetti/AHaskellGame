#!/bin/bash

# The main purpose for this script is to normalize the 'stack.yaml' file so that
# it works on the travis.ci integration platform.

STACK_FILE="stack.yaml"

# This removes the dependency on the nvidia runtime, something we can't guarantee that a integration
# platform such as travis will have available. Furthermore, this option would only come into play when 
# the application is executed meaning that it doesn't make a difference after all.
# Only compilation and test execution is exercised in this integration routine.
sed -i $STACK_FILE -e '/run-args:*/d'

# Update the generated production version image that will compiled using 'stack image container'
VERSION="$(cat src/version.ver)"
sed -i -E "s/(name: \"lhcopetti\/haskell-game:base-lts).*/\1-$VERSION\"/" stack.yaml