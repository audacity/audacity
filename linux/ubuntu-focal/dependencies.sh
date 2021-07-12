export TZ=Europe/London
ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

apt_packages_basic=(
    build-essential
    python3-minimal
    python3-pip
    g++-9
    libstdc++-9-dev
    cmake
    git
)

apt_packages_minimal_deps=(
    libgtk2.0-dev
    libasound2-dev
    libavformat-dev
    libjack-jackd2-dev
)

apt_packages_full_deps=(
    zlib1g-dev
    libexpat1-dev
    libmp3lame-dev
    libsndfile-dev
    libsoxr-dev
    portaudio19-dev
    libsqlite3-dev
    libavcodec-dev
    libavformat-dev
    libavutil-dev
    libid3tag0-dev
    libmad0-dev
    libvamp-hostsdk3v5
    libogg-dev
    libvorbis-dev
    libflac-dev
    libflac++-dev
    lv2-dev
    liblilv-dev
    libserd-dev
    libsord-dev
    libsratom-dev
    libsuil-dev
    libportmidi-dev
    libportsmf-dev
    libsbsms-dev
    libsoundtouch-dev
    libtwolame-dev
    libssl-dev
    libcurl4-openssl-dev
    libpng-dev
    libjpeg-turbo8-dev
)

apt-get install -y --no-install-recommends \
  "${apt_packages_basic[@]}" \
  "${apt_packages_minimal_deps[@]}" \
  "${apt_packages_full_deps[@]}"


pip3 install conan
