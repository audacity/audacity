#!/bin/sh
#
# This script retrieves and build the Audacity manual
#

function cleanAction
{
   [ -d "${TARGET_TEMP_DIR}" ] && rm -rf "${TARGET_TEMP_DIR}"
   [ -d "${TOPLEVEL}/help/manual" ] && rm -r "${TOPLEVEL}/help/manual"
   rm -rf "${TARGET_BUILD_DIR}"
}

function buildAction
{
   # Retrieve the manual
   if [ ! -d "${TOPLEVEL}/help/manual" ]
   then 
       cd "${TOPLEVEL}/scripts/mw2html_audacity"
       ./wiki2htm.sh
       cd "${SRCROOT}"
   fi

   # Create the destination directory if it's not already there
   if [ ! -d "${TARGET_BUILD_DIR}" ]
   then 
      mkdir -p "${TARGET_BUILD_DIR}"
      chmod -RH "${INSTALL_MODE_FLAG}" "${TARGET_BUILD_DIR}"
      chown -RH "${INSTALL_OWNER}:${INSTALL_GROUP}" "${TARGET_BUILD_DIR}"
   fi

   # Copy the manual to the destination
   cp -pPR "${TOPLEVEL}/help/manual/" "${TARGET_BUILD_DIR}"

   # Remove svn files
   find "${TARGET_BUILD_DIR}" -name .svn -print0 | xargs -0 rm -rf
} 

function installAction
{
   # Just do the build to put things where the belong
   buildAction
}

case "${ACTION}" in
   "" | build)
      TARGET_BUILD_DIR="${TARGET_BUILD_DIR}/help/manual"
      buildAction
   ;;

   install)
      TARGET_BUILD_DIR="${DSTROOT}/Audacity/help/manual"
      installAction
   ;;

   clean)
      TARGET_BUILD_DIR="${TARGET_BUILD_DIR}/help/manual"
      cleanAction
   ;;
esac

exit
