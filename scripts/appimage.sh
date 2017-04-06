#!/bin/bash

########################################################################
# Package the binaries built on Travis-CI as an AppImage
# By Simon Peter 2016
# For more information, see http://appimage.org/
########################################################################

APP=Audacity
LOWERAPP=${APP,,}

mkdir -p $HOME/$APP/$APP.AppDir/

cd $HOME/$APP/

wget -q https://github.com/probonopd/AppImages/raw/master/functions.sh -O ./functions.sh
. ./functions.sh

cd $APP.AppDir

sudo chown -R $USER /app/

cp -r /app ./usr
BINARY=./usr/bin/audacity

########################################################################
# Copy desktop and icon file to AppDir for AppRun to pick them up
########################################################################

get_apprun
get_desktop
get_icon

########################################################################
# Copy in the dependencies that cannot be assumed to be available
# on all target systems
########################################################################

copy_deps

########################################################################
# Delete stuff that should not go into the AppImage
########################################################################

# Delete dangerous libraries; see
# https://github.com/probonopd/AppImages/blob/master/excludelist
delete_blacklisted

rm -rf app/ || true

########################################################################
# Patch away absolute paths; it would be nice if they were relative
########################################################################

find usr/ -type f -exec sed -i -e "s|/usr|././|g" {} \;
find usr/ -type f -exec sed -i -e "s|/app|././|g" {} \;

########################################################################
# Other application-specific finishing touches
########################################################################

cp usr/share/icons/hicolor/scalable/apps/audacity.svg .
sed -i -e 's|^Exec=.*|Exec=audacity|g' audacity.desktop
rm -rf usr/lib/x86_64-linux-gnu/libpango* # Otherwise, "expect ugly output"

########################################################################
# desktopintegration asks the user on first run to install a menu item
########################################################################

get_desktopintegration $LOWERAPP

########################################################################
# Determine the version of the app; also include needed glibc version
########################################################################

GLIBC_NEEDED=$(glibc_needed)
VERSION=travis${TRAVIS_BUILD_NUMBER}-glibc$GLIBC_NEEDED
echo $VERSION > ../VERSION

########################################################################
# AppDir complete
# Now packaging it as an AppImage
########################################################################

cd .. # Go out of AppImage

mkdir -p ../out/
generate_type2_appimage

########################################################################
# Upload the AppDir
########################################################################

transfer ../out/*
echo "AppImage has been uploaded to the URL above; use something like GitHub Releases for permanent storage"
echo "For this, see https://github.com/probonopd/uploadtool"
