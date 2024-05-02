FROM ericfont/armv7hf-debian-qemu:jessie

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

RUN [ "cross-build-start" ]

RUN apt-get update

# need to be able to use https for wget
RUN apt-get --no-install-recommends -y install \
  ca-certificates \
  wget

# get prebuilt AppImageKit
RUN wget "https://bintray.com/artifact/download/ericfont/prebuilt-AppImageKit/AppImageKit-5_built-in-armv7hf-jessie.tar.gz" \
  && echo "1710396680a0b4e0c149885e9ee89bd170455c15e86e2ac3ebd426739bd33ec0  AppImageKit-5_built-in-armv7hf-jessie.tar.gz" | sha256sum -c \
  && tar -xvzf AppImageKit-5_built-in-armv7hf-jessie.tar.gz  \
  && rm AppImageKit-5_built-in-armv7hf-jessie.tar.gz

# add AppImageKit dependencies
RUN apt-get --no-install-recommends -y install \
  libfuse-dev \
  libglib2.0-dev \
  cmake \
  git \
  libc6-dev \
  binutils \
  fuse \
  python \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

RUN [ "cross-build-end" ]
