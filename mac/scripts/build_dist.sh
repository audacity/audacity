#!/bin/bash


# Function to retrieve a value from a plist
function plist
{
   /usr/libexec/PlistBuddy -c "Print ${2}" "${1}"
}

function realpath
{
   python -c "import os; import sys; print(os.path.realpath(sys.argv[1]))" "${1}"
}

# Function to notarize a file (APP or DMG)
function notarize
{
   # Bail if not signing
   if [ -z "${SIGNING}" ]
   then
      return
   fi

   # Create temporary output file
   OUTPUT=$(mktemp /tmp/notarization-XXXX)
   trap "cat '${OUTPUT}' ; rm '${OUTPUT}'" EXIT

   # Send the app off for notarization
   xcrun altool --notarize-app \
                --primary-bundle-id "${IDENT}" \
                --file "${1}" \
                --username "${NOTARIZE_USERNAME}" \
                --password "${NOTARIZE_PASSWORD}" \
                --output-format xml \
                >"${OUTPUT}"

   # Bail if notarization failed
   if [ ${?} -ne 0 ]
   then
      exit 1
   fi

   # Extract the request UUID from the output plist
   REQ=$(plist "${OUTPUT}" "notarization-upload:RequestUUID")

   # Poll until the request is complete
   for ((;;))
   do
      # Sleep a bit
      sleep 15s

      # Ask for request status
      xcrun altool --notarization-info "${REQ}" \
                   --username "${NOTARIZE_USERNAME}" \
                   --password "${NOTARIZE_PASSWORD}" \
                   --output-format xml \
                   >"${OUTPUT}"
      if [ ${?} -ne 0 ]
      then
         exit 1
      fi

      # Extract the current status and stop polling if it's no longer in progress
      STATUS=$(plist "${OUTPUT}" "notarization-info:Status")
      echo "Notarization status: ${STATUS}"
      if [ "${STATUS}" != "in progress" ]
      then
         break
      fi
   done

   # Bail if the notarization wasn't successful
   if [ "${STATUS}" != "success" ]
   then
      exit 1
   fi

   # Cleanup
   trap EXIT
   rm "${OUTPUT}"
}

if [ -z "${SRCROOT}" -o -z "${DSTROOT}" ]
then
   if [ "${#}" -ne 2 ]
   then
      echo "You must specify the source and destination roots"
      echo
      echo "Usage: ${0} srcroot dstroot"
      echo
      echo "  srcroot   path to the 'mac' subdirectory of your source tree"
      echo "  dstroot   path to where Audacity was built:"
      echo "            legacy build = /tmp/Audacity.dst"
      echo "            cmake build = <build directory/bin/Release"
      exit 1
   fi
   SRCROOT="${1}"
   DSTROOT="${2}"
fi

# Get full paths
SRCROOT=$(realpath "${SRCROOT}")
DSTROOT=$(realpath "${DSTROOT}")

# Setup
VERSION=`awk '/^#define+ AUDACITY_VERSION / {print $3}' ${SRCROOT}/../src/Audacity.h`
RELEASE=`awk '/^#define+ AUDACITY_RELEASE / {print $3}' ${SRCROOT}/../src/Audacity.h`
REVISION=`awk '/^#define+ AUDACITY_REVISION / {print $3}' ${SRCROOT}/../src/Audacity.h`
VERSION=$VERSION.$RELEASE.$REVISION
IDENT=$(plist "${DSTROOT}/Audacity.app/Contents/Info.plist" "CFBundleIdentifier")

#
# This depends on a file in the builders HOME directory called ".audacity_signing" that
# contains the following four lines with the appropriate values specified.  If the file
# doesn't exist or one of the values is missing the distribution will be built unsigned
# and unnotarized.
#
# CODESIGN_APP_IDENTITY="Developer ID Application:"
# NOTARIZE_USERNAME="specify your Apple developer email address"
# NOTARIZE_PASSWORD="@keychain:APP_PASSWORD"
#
# For information on how to create that APP_PASSWORD in your keychain, refer to:
#
#   https://support.apple.com/guide/keychain-access/add-a-password-to-a-keychain-kyca1120/mac
#
# You generate the app-specific password in your Apple developer account and you must specify
# "org.audacityteam.audacity" as the application identifier.
#
SIGNING=
if [ -r ~/.audacity_signing ]
then
   source ~/.audacity_signing
   if [ -n "${CODESIGN_APP_IDENTITY}" -a -n "${NOTARIZE_USERNAME}" -a -n "${NOTARIZE_PASSWORD}" ]
   then
      SIGNING="y"
   fi
fi

VOL="Audacity $VERSION"
DMG="audacity-macos-$VERSION"

echo "Audacity has been installed to: ${DSTROOT}"
cd "${DSTROOT}/.."

# Make sure we have consistent ownership and permissions
chmod -RH 755 "${DSTROOT}"
chown -RH $(id -u):$(id -g) "${DSTROOT}"

# Preclean
rm -rf "$DMG" "$DMG.dmg" TMP.dmg

# Sign and notarize the app
if [ -n "${SIGNING}" ]
then
   xcrun codesign --verbose \
                  --timestamp \
                  --identifier "${IDENT}" \
                  --options runtime \
                  --entitlements "${SRCROOT}/Audacity.entitlements" \
                  --sign "${CODESIGN_APP_IDENTITY}" \
                  ${DSTROOT}/Audacity.app/Contents/modules/*

   xcrun codesign --verbose \
                  --timestamp \
                  --identifier "${IDENT}" \
                  --options runtime \
                  --entitlements "${SRCROOT}/Audacity.entitlements" \
                  --sign "${CODESIGN_APP_IDENTITY}" \
                  ${DSTROOT}/Audacity.app/Contents/plug-ins/*

   xcrun codesign --verbose \
                  --deep \
                  --timestamp \
                  --identifier "${IDENT}" \
                  --options runtime \
                  --entitlements "${SRCROOT}/Audacity.entitlements" \
                  --sign "${CODESIGN_APP_IDENTITY}" \
                  ${DSTROOT}/Audacity.app

   # Create the ZIP archive for notarization
   xcrun ditto -c -k --keepParent "${DSTROOT}/Audacity.app" "${DSTROOT}.zip" 

   # Send it off for notarization
   notarize "${DSTROOT}.zip"

   # Remove the zip file
   rm "${DSTROOT}.zip"

   # Staple the app
   stapler staple "${DSTROOT}/Audacity.app"
fi

# Create structure
mkdir "$DMG"
cp -pR "${DSTROOT}/" "${DMG}"

# Copy over the background image
mkdir "${DMG}/.background"
cp "${SRCROOT}/Resources/Audacity-DMG-background.png" "${DMG}/.background"

#Add a custom icon for the DMG
#cp -p mac/Resources/Audacity.icns "${DMG}"/.VolumeIcon.icns

# Make sure it's not already attached
ATTACHED=$(hdiutil info | awk "/\/Volumes\/${VOL}/{print \$1}")
if [ -n "${ATTACHED}" ]
then
   hdiutil detach "${ATTACHED}"
fi

# Create and mount the image
hdiutil create -ov -format UDRW -srcdir "$DMG" -fs HFS+ -volname "$VOL" TMP.dmg
if [ $? -ne 0 ]
then
   echo "Create failed"
   exit 1
fi

#Mount the DMG and store the name it was mounted with
TITLE=$(hdiutil attach TMP.dmg | grep \/Volumes | sed "s/^.*\/Volumes\///")
if [ $? -ne 0 ]
then
   echo "Attach failed"
   exit 1
fi

# And wait for it to show up in Finder
osascript <<EOF
   tell application "Finder"
      repeat until exists disk "${TITLE}"
         log "Waiting for ${TITLE} to appear"
         delay 0.2
      end repeat
   end tell
EOF

# Make our DMG look pretty and install the custom background image
echo '
   tell application "Finder"
     with timeout of 300 seconds
        tell disk "'$TITLE'"
              open
              set current view of container window to icon view
              set toolbar visible of container window to false
              set statusbar visible of container window to false
              set the bounds of container window to {400, 100, 1000, 550}
              set theViewOptions to the icon view options of container window
              set arrangement of theViewOptions to not arranged
              set icon size of theViewOptions to 72
              set background picture of theViewOptions to file ".background:Audacity-DMG-background.png" 
              make new alias file at container window to POSIX file "/Applications" with properties {name:"Applications"}
              set position of item "Audacity" of container window to {170, 350}
              set position of item "Applications" of container window to {430, 350}
              close
              open
              update without registering applications
              delay 5
              eject
        end tell
     end timeout
   end tell
' | osascript

# Compress and prepare for Internet delivery
hdiutil convert TMP.dmg -format UDZO -imagekey zlib-level=9 -o "$DMG.dmg"

# Sign, notarize and staple the DMG
if [ -n "${SIGNING}" ]
then
   xcrun codesign --verbose \
                  --timestamp \
                  --identifier "${IDENT}" \
                  --sign "${CODESIGN_APP_IDENTITY}" \
                  "${DMG}.dmg"
   notarize "${DMG}.dmg"
   stapler staple "${DMG}.dmg"
fi

# Create zip version
rm -rf "${DMG}/.background"
rm -rf "${DMG}/Audacity.app/Contents/help"
zip -r9 "${DMG}.zip" "${DMG}"

# Cleanup
rm -rf ${DMG} TMP.dmg

exit 0

