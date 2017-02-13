
#Hand edit this to get the 'right' results.
#This batch file is just a reminder of the commands...

hdiutil resize -size 200m /private/tmp/TMP.dmg
hdiutil mount /private/tmp/TMP.dmg
#Now move the folders out the way so they don't clutter appearance.
#Also optionally add Portable Settings and Auto Save with chmod 777.
codesign --deep -s IDENTITY -v "/Volumes/Audacity VERSION/Audacity.app" 
spctl -a -v "/Volumes/Audacity VERSION/Audacity.app"
hdiutil eject "/Volumes/Audacity VERSION/"

# Compress and prepare for Internet delivery
hdiutil convert /private/tmp/TMP.dmg -format UDZO -imagekey zlib-level=9 -o "/private/tmp/RESULT.dmg"

