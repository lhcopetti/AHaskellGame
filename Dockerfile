FROM ubuntu:16.04

RUN apt-get update && \
		apt-get -y upgrade && \
		apt-get install -y \
        build-essential \
        g++ \
        cmake \
        git \
        wget \
        libfreetype6-dev \
        libjpeg-dev \
        xorg-dev \
        libxrandr-dev \
        xcb \
        libx11-xcb-dev \
        libxcb-randr0-dev \
        libxcb-image0-dev \
        libgl1-mesa-dev \
        libflac-dev \
        libogg-dev \
        libvorbis-dev \
        libopenal-dev \
        libpthread-stubs0-dev \
        curl \
        libudev-dev \
        libsfml-dev \
        libcsfml-dev

# Installing stack.
RUN curl -sSL https://get.haskellstack.org/ | /bin/sh

WORKDIR /home/
RUN git clone https://github.com/lhcopetti/AHaskellGame
WORKDIR /home/AHaskellGame

RUN stack clean
RUN stack build