#!/bin/sh
#
# This script builds the 2 plugins that are included in the Audacity
# distribution.
#
# Since we do not include the plugin source as part of Audacity, we
# must retrieve it first, followed by configure.  However, we do not
# use the constructed Makefile since we only need 2 of the plugins and
# it is not universal binary friendly.
#
# In addition, each plugin has an initialization routine that must be
# executed before the plugin can be used.  The plugin distribution does
# not provide support for this on OSX so, by default, the initialization
# routine never gets executed.
#
# Therefore, we assign the constructor attribute to the initialization
# routine which causes the routine to be executed as soon as the plugin
# is loaded.
#

# Set to the URL to the plugins source distribution
swhdist="http://plugin.org.uk/releases/0.4.15/swh-plugins-0.4.15.tar.gz"

# Set to base directory within the above distribution
swhpath="./swh-plugins-0.4.15"

#
# Builds an individual plugin
#
function build
{
   # Get the plugin name
   target="$1"
   shift

   # Create the destination directory if it's not already there
   if [ ! -d "${TARGET_BUILD_DIR}" ]
   then 
      mkdir -p "${TARGET_BUILD_DIR}"
      chmod -RH "${INSTALL_MODE_FLAG}" "${TARGET_BUILD_DIR}"
      chown -RH "${INSTALL_OWNER}:${INSTALL_GROUP}" "${TARGET_BUILD_DIR}"
   fi

   # Build the plugin if it doesn't exist
   if [ ! -e "${TARGET_BUILD_DIR}/${target}.so" ]
   then 
      GCC=gcc
      if [ -e "${GCC}-${GCC_VERSION}" ]
      then
         GCC="${GCC}-${GCC_VERSION}"
      fi
      
      echo "Building ${target} using ${GCC}"

      "${GCC}" -bundle -arch ${ARCHS/ / -arch } \
          -mmacosx-version-min="${MACOSX_DEPLOYMENT_TARGET}" -isysroot "${SDKROOT}" \
          -O3 -fomit-frame-pointer -fstrength-reduce -funroll-loops -ffast-math \
          "-D_init=__attribute__ ((constructor)) _${target}_init" \
          -o "${TARGET_BUILD_DIR}/${target}.so" ${*}
   fi
}

function cleanAction
{
   [ -d "${TARGET_TEMP_DIR}" ] && rm -rf "${TARGET_TEMP_DIR}"
   rm -f "${TARGET_BUILD_DIR}/hard_limiter_1413.so"
   rm -f "${TARGET_BUILD_DIR}/sc4_1882.so"
}

function buildAction
{
   # Create the temp directory
   mkdir -p "${TARGET_TEMP_DIR}"
   cd "${TARGET_TEMP_DIR}"

   # Get the distribution
   if [ ! -e "${swhpath}" ]
   then
      echo "Retrieving plugins"
      ftp -o '|tar xf -' "${swhdist}"
   fi

   # Get to where we need to be
   cd "${swhpath}"
    
   # Get rid of the existing config.h options
   echo >config.h
    
   # Build the 2 standard plugins
   build hard_limiter_1413 hard_limiter_1413.c
   build sc4_1882 sc4_1882.c util/db.c util/rms.c
} 

function installAction
{
   # Just do the build to put things where the belong
   buildAction
}

case "${ACTION}" in
   "" | build)
      TARGET_BUILD_DIR="${TARGET_BUILD_DIR}/plug-ins"
      buildAction
   ;;

   install)
      TARGET_BUILD_DIR="${DSTROOT}/Audacity/plug-ins"
      installAction
   ;;

   clean)
      TARGET_BUILD_DIR="${TARGET_BUILD_DIR}/plug-ins"
      cleanAction
   ;;
esac

exit
