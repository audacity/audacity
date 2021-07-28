FROM fedora:latest

LABEL maintainer="d.vedenko@audacityteam.org"
LABEL description="A build environment to check the builds for Fedora package maintainers"
LABEL version="3.0"

RUN dnf install -y dos2unix wget

RUN dnf install -y fedora-packager @development-tools

COPY ["audacity.spec", "/root/rpmbuild/SPEC/audacity.spec"]

RUN dos2unix /root/rpmbuild/SPEC/audacity.spec

COPY ["audacity.cfg", "/etc/mock/audacity.cfg"]

RUN dos2unix /etc/mock/audacity.cfg

RUN wget -P /root/rpmbuild/SOURCES/ https://github.com/audacity/audacity/releases/download/Audacity-3.0.2/audacity-manual-3.0.2.zip

COPY ["entrypoint.sh", "/entrypoint.sh"]
RUN dos2unix /entrypoint.sh

CMD ["bash", "-ex", "/entrypoint.sh"]
