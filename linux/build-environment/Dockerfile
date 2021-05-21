FROM ubuntu:focal

LABEL maintainer="d.vedenko@audacityteam.org"
LABEL description="A build environment to check the builds for the package maintainers"
LABEL version="3.0"

ENV TZ=Europe/London
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN apt-get update

RUN apt-get install -y \
    build-essential \
    python3-minimal \
    python3-pip \
    g++-9 \
    libstdc++-9-dev \
    cmake \
    git

RUN pip3 install conan

RUN apt-get install -y \ 
    zlib1g-dev \
    libgtk2.0-dev \
    libasound2-dev \
    libavformat-dev \
    libjack-jackd2-dev \
    libexpat1-dev \
    libmp3lame-dev \
    libsndfile-dev \
    libsoxr-dev \
    portaudio19-dev \
    libsqlite3-dev \
    libavcodec-dev \
    libavformat-dev \
    libavutil-dev \
    libid3tag0-dev \
    libmad0-dev \
    libvamp-hostsdk3v5 \
    libogg-dev \
    libvorbis-dev \
    libflac-dev \
    libflac++-dev \
    lv2-dev \
    liblilv-dev \
    libserd-dev \
    libsord-dev \
    libsratom-dev \
    libsuil-dev \
    libportmidi-dev \
    libportsmf-dev \
    libsbsms-dev \
    libsoundtouch-dev \
    libtwolame-dev \
    libssl-dev \
    libcurl4-openssl-dev

RUN apt-get install -y libpng-dev

WORKDIR /audacity

COPY ["build_audacity.sh", "/audacity/"]

# pkg-config is so broken
COPY ["pkgconfig/*", "/usr/local/lib/pkgconfig/"]

CMD ["./build_audacity.sh"]