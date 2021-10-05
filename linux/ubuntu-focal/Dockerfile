FROM ubuntu:focal

LABEL maintainer="d.vedenko@audacityteam.org"
LABEL description="A build environment to check the builds for the package maintainers"
LABEL version="3.0"

RUN apt-get update && apt-get install -y dos2unix

WORKDIR /audacity

COPY ["dependencies.sh", "/audacity/"]
# pkg-config is so broken
COPY ["pkgconfig/*", "/usr/local/lib/pkgconfig/"]

RUN dos2unix dependencies.sh && \
    find /usr/local/lib/pkgconfig/ -type f -print0 | xargs -0 dos2unix

RUN ["bash", "-ex", "dependencies.sh"]

COPY ["entrypoint.sh", "/audacity/"]
RUN dos2unix entrypoint.sh

CMD ["bash", "-ex", "./entrypoint.sh"]
