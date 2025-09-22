#!/bin/bash

INSTALL_DIR="build.install"
APPNAME=audacity
LONG_NAME="Audacity"
LONGER_NAME="Audacity 4"
VERSION=0
HERE="$(cd "$(dirname "$0")" && pwd)"

while [[ "$#" -gt 0 ]]; do
    case $1 in
        --long_name) LONG_NAME="$2"; shift ;;
        --longer_name) LONGER_NAME="$2"; shift ;;
        --version) VERSION=$2; shift ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done  

echo "LONG_NAME: $LONG_NAME"
echo "LONGER_NAME: $LONGER_NAME"
echo "VERSION: $VERSION"

WORKING_DIRECTORY=${INSTALL_DIR}
BACKGROUND=$HERE/audacity-dmg-background.tiff
APP_PATH=${INSTALL_DIR}/${APPNAME}.app

VOLNAME=${LONG_NAME}-${VERSION}
DMGNAME=${VOLNAME}Uncompressed.dmg
COMPRESSEDDMGNAME=${VOLNAME}.dmg


function set_bundle_display_options() {
	osascript <<-EOF
    tell application "Finder"
        set f to POSIX file ("${1}" as string) as alias
        tell folder f
            open
            tell container window
                set toolbar visible to false
                set statusbar visible to false
                set current view to icon view
                delay 1 -- sync
                set the bounds to {0, 0, 589, 435}
            end tell
            delay 1 -- sync
            set icon size of the icon view options of container window to 120
            set arrangement of the icon view options of container window to not arranged
            set position of item "${LONGER_NAME}.app" to {150, 200}
            close
            set position of item "Applications" to {439, 200}
            open
            set background picture of the icon view options of container window to file "background.tiff" of folder "Pictures"
            set the bounds of the container window to {0, 0, 589, 435}
            update without registering applications
            delay 5 -- sync
            close
        end tell
        delay 5 -- sync
    end tell
EOF

}

function change_rpath() {
   for P in `otool -L $1 | awk '{print $1}'`
   do
      if [[ "$P" == *@rpath* ]]
      then
         if [[ "$P" == *Qt* ]]
         then
            PSLASH=$(echo $P | sed 's,@rpath,@loader_path/../Frameworks,g')
            FNAME=$(echo $P | sed "s,@rpath,${VOLUME}/${APPNAME}.app/Contents/Frameworks,g")
            install_name_tool -change $P $PSLASH $1
            for P1 in `otool -L $FNAME | awk '{print $1}'`
            do
               if [[ "$P1" == *@rpath* ]]
               then
                   PSLASH1=$(echo $P1 | sed "s,@rpath,@loader_path/../../..,g")
                   install_name_tool -change $P1 $PSLASH1 $FNAME
               fi
            done
         else
            PSLASH=$(echo $P | sed 's,@rpath,@executable_path/../Frameworks,g')
            FNAME=$(echo $P | sed "s,@rpath,${VOLUME}/${APPNAME}.app/Contents/Frameworks,g")
            install_name_tool -change $P $PSLASH $1
         fi
      fi
   done
}


rm ${WORKING_DIRECTORY}/${COMPRESSEDDMGNAME}

#tip: increase the size if error on copy or macdeployqt
hdiutil create -size 750m -fs HFS+ -volname ${VOLNAME} ${WORKING_DIRECTORY}/${DMGNAME}

# Mount the disk image
hdiutil attach ${WORKING_DIRECTORY}/${DMGNAME}

# Obtain device information
DEVS=$(hdiutil attach ${WORKING_DIRECTORY}/${DMGNAME} | cut -f 1)
DEV=$(echo $DEVS | cut -f 1 -d ' ')
VOLUME=$(mount |grep ${DEV} | cut -f 3 -d ' ')

# copy in the application bundle
cp -Rp ${APP_PATH} ${VOLUME}/${APPNAME}.app

# delete unused stuff
rm -rf ${VOLUME}/${APPNAME}.app/Contents/PlugIns/mediaservice
rm -rf ${VOLUME}/${APPNAME}.app/Contents/PlugIns/audio
rm -rf ${VOLUME}/${APPNAME}.app/Contents/PlugIns/sqldrivers

rm -rf ${VOLUME}/${APPNAME}.app/Contents/Frameworks/Enginio.*
rm -rf ${VOLUME}/${APPNAME}.app/Contents/Frameworks/QtBluetooth.*
rm -rf ${VOLUME}/${APPNAME}.app/Contents/Frameworks/QtNfc.*
rm -rf ${VOLUME}/${APPNAME}.app/Contents/Frameworks/QtLocation.*
rm -rf ${VOLUME}/${APPNAME}.app/Contents/Frameworks/QtPurchasing.*
rm -rf ${VOLUME}/${APPNAME}.app/Contents/Frameworks/QtQuickParticles.*

rm -rf ${VOLUME}/${APPNAME}.app/Contents/Resources/Frameworks/qml/Enginio
rm -rf ${VOLUME}/${APPNAME}.app/Contents/Resources/Frameworks/qml/QtBluetooth
rm -rf ${VOLUME}/${APPNAME}.app/Contents/Resources/Frameworks/qml/QtNfc
rm -rf ${VOLUME}/${APPNAME}.app/Contents/Resources/Frameworks/qml/QtLocation
rm -rf ${VOLUME}/${APPNAME}.app/Contents/Resources/Frameworks/qml/QtPositioning
rm -rf ${VOLUME}/${APPNAME}.app/Contents/Resources/Frameworks/qml/QtPurchasing
rm -rf ${VOLUME}/${APPNAME}.app/Contents/Resources/Frameworks/qml/QtQuick/Particles*
rm -rf ${VOLUME}/${APPNAME}.app/Contents/Resources/Frameworks/qml/Qt/WebSockets

# TODO: These files should not be here in the first place, fix install step
find "${VOLUME}/${APPNAME}.app/Contents/Resources" -type d -name "objects-*" -prune -exec rm -rf {} +
find "${VOLUME}/${APPNAME}.app/Contents/Resources" -type d -name ".qt" -prune -exec rm -rf {} +
find "${VOLUME}/${APPNAME}.app/Contents/Resources" -type f -name "*.o" -delete

#homebrew libs may not be writable force it?
chmod -R +w /usr/local/Cellar/
echo "macdeployqt"
macdeployqt ${VOLUME}/${APPNAME}.app \
    -verbose=2 \
    -qmldir=. \
    -sign-for-notarization="Developer ID Application: MuseScore"

# replace qt.conf generated by macdeployqt
cp -f $HERE/qt.conf ${VOLUME}/${APPNAME}.app/Contents/Resources

# fix the libs, qt5.6 has @rpath...
BIN_FILE=${VOLUME}/${APPNAME}.app/Contents/MacOS/${APPNAME}
change_rpath $BIN_FILE

# Workaround:
# fix Homebrew libraries with hard coded absolute path, see QTBUG-56814
FOLDER_NAME=${VOLUME}/${APPNAME}.app/Contents/Frameworks
for P in `ls -d $FOLDER_NAME/*.* | awk '{print $1}'`
do
    if [[ "$P" == *.dylib ]]
    then
        for P1 in `otool -L $P | awk '{print $1}'`
        do
            if [[ "$P1" == /usr/local/Cellar*.dylib ]]
            then
                PATHNAME=$(dirname $P1)
                PSLASH1=$(echo $P1 | sed "s,$PATHNAME,@executable_path/../Frameworks,g")
                install_name_tool -change $P1 $PSLASH1 $P
            fi
        done
    fi
done

for f in $(find "${VOLUME}/${APPNAME}.app/Contents" -iname "*");
do
  lipo -remove ppc7400 "$f" -output "$f" > /dev/null 2>&1
  lipo -remove x86 "$f" -output "$f" > /dev/null 2>&1
  lipo -remove ppc "$f" -output "$f" > /dev/null 2>&1
done

otool -L ${VOLUME}/${APPNAME}.app/Contents/MacOS/${APPNAME}

# Remove dSYM files
echo "Remove dSYM files"
find ${VOLUME}/${APPNAME}.app/Contents -type d -name "*.dSYM" -exec rm -r {} +

#codesign
echo "Codesign"
# `codesign --deep` doesn't seem to search for code in Contents/Resources directory so sign libraries in it manually
find "${VOLUME}/${APPNAME}.app/Contents/Resources" -name '*.dylib' -exec codesign --force --options runtime --deep -s "Developer ID Application: MuseScore" '{}' ';'
# Sign code in other (more conventional) locations
codesign --force --options runtime --entitlements "${HERE}/macosx_entitlements.plist" --deep -s "Developer ID Application: MuseScore" "${VOLUME}/${APPNAME}.app"
echo "Codesign verify"
codesign --verify --deep --strict --verbose=2 "${VOLUME}/${APPNAME}.app"
echo "spctl"
spctl --assess --type execute "${VOLUME}/${APPNAME}.app"

echo "Rename ${APPNAME}.app to ${VOLUME}/${LONGER_NAME}.app"
mv ${VOLUME}/${APPNAME}.app "${VOLUME}/${LONGER_NAME}.app"

echo "Copy in background image"
# copy in background image
mkdir -p ${VOLUME}/Pictures
# fixme: path
cp  ${BACKGROUND} ${VOLUME}/Pictures/background.tiff

echo "symlink application"
# nssymlink application
ln -s /Applications/ ${VOLUME}/Applications
set_bundle_display_options ${VOLUME}
mv ${VOLUME}/Pictures ${VOLUME}/.Pictures


echo "Unmount"
for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20; do
    # Unmount the disk image
    hdiutil detach $DEV
    if [ $? -eq 0 ]; then
        break
    fi
    if [ $i -eq 20 ]; then
        echo "Failed to unmount the disk image; exiting after 20 retries."
        exit 1
    fi
    echo "Failed to unmount the disk image; retrying in 30s"
    sleep 30
done

# Convert the disk image to read-only
hdiutil convert ${WORKING_DIRECTORY}/${DMGNAME} -format UDBZ -o ${WORKING_DIRECTORY}/${COMPRESSEDDMGNAME}

shasum -a 256 ${WORKING_DIRECTORY}/${COMPRESSEDDMGNAME}

rm ${WORKING_DIRECTORY}/${DMGNAME}

ls ${WORKING_DIRECTORY}
