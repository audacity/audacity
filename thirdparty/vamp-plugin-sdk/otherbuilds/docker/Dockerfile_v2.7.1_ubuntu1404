FROM ubuntu:14.04
MAINTAINER Chris Cannam <cannam@all-day-breakfast.com>
RUN apt-get update && \
    apt-get install -y \
    build-essential \
    libsndfile-dev \
    git \
    mercurial
RUN gcc --version
RUN apt-get clean && rm -rf /var/lib/apt/lists/*
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8
RUN git clone --branch vamp-plugin-sdk-v2.7.1 https://github.com/c4dm/vamp-plugin-sdk
RUN hg clone https://code.soundsoftware.ac.uk/hg/vamp-test-plugin
WORKDIR vamp-plugin-sdk
RUN ./configure && make
WORKDIR ../vamp-test-plugin
RUN make -f Makefile.linux
WORKDIR ../vamp-plugin-sdk
RUN test/run-test-plugin-regression.sh
RUN mkdir vamp-plugin-sdk-2.7.1-binaries-amd64-gcc4-linux
RUN cp libvamp-sdk.a libvamp-hostsdk.a host/vamp-simple-host rdf/generator/vamp-rdf-template-generator vamp-plugin-sdk-2.7.1-binaries-amd64-gcc4-linux
RUN tar cvzf vamp-plugin-sdk-2.7.1-binaries-amd64-gcc4-linux.tar.gz vamp-plugin-sdk-2.7.1-binaries-amd64-gcc4-linux
RUN tar cvf output.tar *.tar.gz && cp output.tar ..
