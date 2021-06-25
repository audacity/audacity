on run argv
    set diskImage to item 1 of argv

    tell application "Finder"
        tell disk diskImage
              open
              set current view of container window to icon view
              set toolbar visible of container window to false
              set statusbar visible of container window to false
              set the bounds of container window to {400, 100, 1000, 550}
              set theViewOptions to the icon view options of container window
              set arrangement of theViewOptions to not arranged
              set icon size of theViewOptions to 72
              set background picture of theViewOptions to file ".background:background.png" 
              set position of item "Audacity" of container window to {170, 350}
              set position of item "Applications" of container window to {430, 350}
              close
              open
              update without registering applications
        end tell
   end tell
end run