#!/usr/bin/env bash

echo "Setup Linux build environment"
trap 'echo Setup failed; exit 1' ERR

df -h .

BUILD_TOOLS=$HOME/build_tools
ENV_FILE=$BUILD_TOOLS/environment.sh
COMPILER="gcc" # gcc, clang

while [[ "$#" -gt 0 ]]; do
    case $1 in
        --compiler) COMPILER="$2"; shift ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

mkdir -p $BUILD_TOOLS

# Let's remove the file with environment variables to recreate it
rm -f $ENV_FILE

echo "echo 'Setup build environment'" >> $ENV_FILE

##########################################################################
# GET DEPENDENCIES
##########################################################################

# DISTRIBUTION PACKAGES

apt_packages_basic=(
  # Alphabetical order please!
  desktop-file-utils
  file
  git
  lcov # for code coverage
  pkg-config
  software-properties-common # installs `add-apt-repository`
  unzip
  p7zip-full
  )

apt_packages_standard=(
  # Alphabetical order please!
  curl
  libasound2-dev
  libfontconfig1-dev
  libfreetype6-dev
  libfreetype6
  libgl1-mesa-dev
  #libjack-dev
  libnss3-dev
  #libportmidi-dev
  libpulse-dev
  libsndfile1-dev
  make
  wget
  )

apt_packages_runtime=(
  # Alphabetical order please!
  libcups2
  libdbus-1-3
  libegl1-mesa-dev
  #libodbc1
  libpq-dev
  libssl-dev
  libxcomposite-dev
  libxcursor-dev
  libxi-dev
  libxkbcommon-x11-0
  libxrandr2
  libxtst-dev
  libdrm-dev
  libxcb-icccm4
  libxcb-image0
  libxcb-keysyms1
  libxcb-randr0
  libxcb-render-util0
  libxcb-xinerama0
  libxcb-xkb-dev
  libxcb-shape0
  libxkbcommon-dev
  libvulkan-dev
  )

apt_packages_ffmpeg=(
  ffmpeg
  libavcodec-dev
  libavformat-dev
  libswscale-dev
  )

apt_packages_au3=(
    # For Audacity
    libasound2-dev
    libgtk2.0-dev
    libjack-jackd2-dev
    gettext
    python3-pip
    libgl1-mesa-dev
    uuid-dev
    # xkeyboard-config
    xkb-data
    # It appears that CCI M4 package does not work correctly
    m4
)

sudo apt-get update
sudo apt-get install -y --no-install-recommends \
  "${apt_packages_basic[@]}" \
  "${apt_packages_standard[@]}" \
  "${apt_packages_runtime[@]}" \
  "${apt_packages_au3[@]}"

##########################################################################
# GET TOOLS
##########################################################################

# COMPILER
if [ "$COMPILER" == "gcc" ]; then

  gcc_version="11"
  sudo apt install -y --no-install-recommends "g++-${gcc_version}"
  sudo update-alternatives \
    --install /usr/bin/gcc gcc "/usr/bin/gcc-${gcc_version}" 40 \
    --slave /usr/bin/g++ g++ "/usr/bin/g++-${gcc_version}"

  echo export CC="/usr/bin/gcc-${gcc_version}" >> ${ENV_FILE}
  echo export CXX="/usr/bin/g++-${gcc_version}" >> ${ENV_FILE}

  gcc-${gcc_version} --version
  g++-${gcc_version} --version


elif [ "$COMPILER" == "clang" ]; then

  sudo apt install clang
  echo export CC="/usr/bin/clang" >> ${ENV_FILE}
  echo export CXX="/usr/bin/clang++" >> ${ENV_FILE}

  clang --version
  clang++ --version

else
  echo "Unknown compiler: $COMPILER"
fi

cmake --version
sudo apt-get install -y ninja-build
echo "ninja version"
ninja --version

##########################################################################
# POST INSTALL
##########################################################################

chmod +x "$ENV_FILE"

# # tidy up (reduce size of Docker image)
# apt-get clean autoclean
# apt-get autoremove --purge -y
# rm -rf /tmp/* /var/{cache,log,backups}/* /var/lib/apt/*

df -h .
echo "Setup script done"
