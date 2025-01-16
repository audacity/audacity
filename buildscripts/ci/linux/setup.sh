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
# GET QT
##########################################################################

# Get newer Qt (only used cached version if it is the same)
qt_version="624"
qt_dir="$BUILD_TOOLS/Qt/${qt_version}"
if [[ ! -d "${qt_dir}" ]]; then
  mkdir -p "${qt_dir}"
  qt_url="https://s3.amazonaws.com/utils.musescore.org/Qt${qt_version}_gcc64.7z"
  wget -q --show-progress -O qt.7z "${qt_url}"
  7z x -y qt.7z -o"${qt_dir}"
fi

echo export PATH="${qt_dir}/bin:\${PATH}" >> ${ENV_FILE}
echo export LD_LIBRARY_PATH="${qt_dir}/lib:\${LD_LIBRARY_PATH}" >> ${ENV_FILE}
echo export QT_PATH="${qt_dir}" >> ${ENV_FILE}
echo export QT_PLUGIN_PATH="${qt_dir}/plugins" >> ${ENV_FILE}
echo export QML2_IMPORT_PATH="${qt_dir}/qml" >> ${ENV_FILE}


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

# CMAKE
# Get newer CMake (only used cached version if it is the same)
cmake_dir="$BUILD_TOOLS/cmake/3.24.0"
if [[ ! -d "$cmake_dir" ]]; then
  mkdir -p "$cmake_dir"
  cmake_url="https://cmake.org/files/v3.24/cmake-3.24.0-linux-x86_64.tar.gz"
  wget -q --show-progress --no-check-certificate -O - "${cmake_url}" | tar --strip-components=1 -xz -C "${cmake_dir}"
fi
echo export PATH="$cmake_dir/bin:\${PATH}" >> ${ENV_FILE}
$cmake_dir/bin/cmake --version

# Ninja
echo "Get Ninja"
ninja_dir=$BUILD_TOOLS/Ninja
if [[ ! -d "$ninja_dir" ]]; then
  mkdir -p $ninja_dir
  wget -q --show-progress -O $ninja_dir/ninja "https://s3.amazonaws.com/utils.musescore.org/build_tools/linux/Ninja/ninja"
  chmod +x $ninja_dir/ninja
fi
echo export PATH="${ninja_dir}:\${PATH}" >> ${ENV_FILE}
echo "ninja version"
$ninja_dir/ninja --version

# Dump syms
echo "Get Breakpad"
breakpad_dir=$BUILD_TOOLS/breakpad
if [[ ! -d "$breakpad_dir" ]]; then
  wget -q --show-progress -O $BUILD_TOOLS/dump_syms.7z "https://s3.amazonaws.com/utils.musescore.org/breakpad/linux/x86-64/dump_syms.7z"
  7z x -y $BUILD_TOOLS/dump_syms.7z -o"$breakpad_dir"
fi
echo export DUMPSYMS_BIN="$breakpad_dir/dump_syms" >> $ENV_FILE

##########################################################################
# OTHER
##########################################################################
# TODO: https://github.com/musescore/MuseScore/issues/11689
#echo "Get VST"
#vst_dir=$BUILD_TOOLS/vst
#if [[ ! -d "$vst_dir" ]]; then
#  wget -q --show-progress -O $BUILD_TOOLS/vst_sdk.7z "https://s3.amazonaws.com/utils.musescore.org/VST3_SDK_379.7z"
#  7z x -y $BUILD_TOOLS/vst_sdk.7z -o"$vst_dir"
#fi
#echo export VST3_SDK_PATH="$vst_dir/VST3_SDK" >> $ENV_FILE

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
