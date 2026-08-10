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
         elif [[ "$P" == *libwx_* ]]
         then
            PSLASH=$(echo $P | sed 's,@rpath,@loader_path/../Frameworks,g')
            FNAME=$(echo $P | sed "s,@rpath,${VOLUME}/${APPNAME}.app/Contents/Frameworks,g")
            install_name_tool -change $P $PSLASH $1
            if [[ -f "$FNAME" ]]
            then
               install_name_tool -add_rpath @loader_path/../Frameworks $FNAME 2>/dev/null || true
               install_name_tool -add_rpath @executable_path/../Frameworks $FNAME 2>/dev/null || true

               for WX_DEP in `otool -L $FNAME | awk '{print $1}' | grep libwx_`
               do
                  if [[ "$WX_DEP" == *@rpath* ]]
                  then
                     WX_PSLASH=$(echo $WX_DEP | sed 's,@rpath,@loader_path/../Frameworks,g')
                     install_name_tool -change $WX_DEP $WX_PSLASH $FNAME 2>/dev/null || true
                  fi
               done
            fi
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

TARGET_ARCH=$(lipo -archs "$BIN_FILE" 2>/dev/null)
if [[ "$TARGET_ARCH" == "x86_64" || "$TARGET_ARCH" == "arm64" ]]; then
  echo "Thin universal dependencies to ${TARGET_ARCH}"
  while IFS= read -r -d '' FILE_PATH; do
    ARCHS=$(lipo -archs "$FILE_PATH" 2>/dev/null) || continue
    if [[ " $ARCHS " == *" $TARGET_ARCH "* && "$ARCHS" == *" "* ]]; then
      FILE_MODE=$(stat -f '%Lp' "$FILE_PATH")
      lipo "$FILE_PATH" -thin "$TARGET_ARCH" -output "${FILE_PATH}.thin"
      chmod "$FILE_MODE" "${FILE_PATH}.thin"
      mv -f "${FILE_PATH}.thin" "$FILE_PATH"
    fi
  done < <(find "${VOLUME}/${APPNAME}.app/Contents" -type f -print0)
fi

otool -L ${VOLUME}/${APPNAME}.app/Contents/MacOS/${APPNAME}

# Remove dSYM files
echo "Remove dSYM files"
find ${VOLUME}/${APPNAME}.app/Contents -type d -name "*.dSYM" -exec rm -r {} +

#codesign
echo "Codesign"
# `codesign --deep` doesn't seem to search for code in Contents/Resources directory so sign libraries in it manually
SIGN_IDENTITY="Developer ID Application: MuseScore"

if security find-identity -v -p codesigning | grep -q "$SIGN_IDENTITY"; then
  echo "Using signing identity: $SIGN_IDENTITY"
else
  echo "Falling back to ad-hoc signing"
  SIGN_IDENTITY="-"
fi

find "${VOLUME}/${APPNAME}.app/Contents/Resources" -name '*.dylib' -print0 | \
  xargs -0 -P 4 -I {} codesign --force --options runtime -s "$SIGN_IDENTITY" '{}'

codesign --force --options runtime \
  --entitlements "${HERE}/macosx_entitlements.plist" \
  --deep -s "$SIGN_IDENTITY" "${VOLUME}/${APPNAME}.app"

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
hdiutil detach $DEV -force || true

# Convert the disk image to read-only
hdiutil convert ${WORKING_DIRECTORY}/${DMGNAME} -format UDBZ -o ${WORKING_DIRECTORY}/${COMPRESSEDDMGNAME}

shasum -a 256 ${WORKING_DIRECTORY}/${COMPRESSEDDMGNAME}

rm ${WORKING_DIRECTORY}/${DMGNAME}

# Fix rpath after packaging, needed for generating dump symbols
SOURCE_BIN_FILE=${APP_PATH}/Contents/MacOS/${APPNAME}
if [[ -f "$SOURCE_BIN_FILE" ]]
then
    change_rpath "$SOURCE_BIN_FILE"
fi

SOURCE_FRAMEWORKS=${APP_PATH}/Contents/Frameworks
if [[ -d "$SOURCE_FRAMEWORKS" ]]
then
    for LIB_FILE in $SOURCE_FRAMEWORKS/*.dylib
    do
        if [[ -f "$LIB_FILE" ]]
        then
            change_rpath "$LIB_FILE"
        fi
    done
fi

ls ${WORKING_DIRECTORY}
