
#Hand edit this to get the 'right' results.
#This batch file is just a reminder of the commands...

hdiutil resize -size 200m /private/tmp/TMP.dmg
hdiutil mount /private/tmp/TMP.dmg
codesign --deep -s IDENTITY -v "/Volumes/Audacity VERSION/Audacity.app" 
spctl -s -v "Volumes/Audacity VERSION/Audacity.app"
hdiutil eject "/Volumes/Audacity VERSION/"

# Compress and prepare for Internet delivery
hdiutil convert /private/tmp/TMP.dmg -format UDZO -imagekey zlib-level=9 -o "/private/tmp/RESULT.dmg"

