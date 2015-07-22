set -x

# Setup
VERSION=`awk '/^#define+ AUDACITY_VERSION / {print $3}' build/Info.plist.h`
RELEASE=`awk '/^#define+ AUDACITY_RELEASE / {print $3}' build/Info.plist.h`
REVISION=`awk '/^#define+ AUDACITY_REVISION / {print $3}' build/Info.plist.h`
VERSION=$VERSION.$RELEASE.$REVISION

cd "${DSTROOT}"
chmod -RH "${INSTALL_MODE_FLAG}" "${TARGET_BUILD_DIR}"
chown -RH "${INSTALL_OWNER}:${INSTALL_GROUP}" "${TARGET_BUILD_DIR}"

echo "Audacity has been installed to: ${DSTROOT}"

cd ..

VOL="Audacity $VERSION"
DMG="audacity-macosx-$VERSION"

# Preclean
rm -rf "$DMG" "$DMG.dmg" TMP.dmg

# Create structure
mkdir "$DMG"
cp -pR "${DSTROOT}/" "${DMG}"

#Add a custom icon for the DMG
#cp -p mac/Resources/Audacity.icns "${DMG}"/.VolumeIcon.icns

# Create and mount the image
hdiutil create -ov -format UDRW -srcdir "$DMG" -fs HFS+ -volname "$VOL" TMP.dmg

#Mount the DMG and store the name it was mounted with
TITLE=`hdiutil attach TMP.dmg | grep \/Volumes | sed "s/^.*\/Volumes\///"`

#Set the custom icon flag
#SetFile -a C /Volumes/"$TITLE"

#Make our DMG look pretty and install the custom background image
echo '
   tell application "Finder"
     tell disk "'${TITLE}'"
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
   end tell
' | osascript

# Compress and prepare for Internet delivery
hdiutil convert TMP.dmg -format UDZO -imagekey zlib-level=9 -o "$DMG.dmg"

# Create zip version
rm -rf "${DMG}/.background"
rm -rf "${DMG}/Audacity/help/"
zip -r9 "${DMG}.zip" "${DMG}"

# Cleanup
rm -rf ${DMG} TMP.dmg
