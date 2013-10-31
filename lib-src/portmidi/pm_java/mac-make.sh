# script to build a jar file to run PmDefaults from the command line on OS X
# (This is for debugging. Normally, you would use XCode to build PmDefaults.app.)

# Compile the java Portidi interface classes.
javac jportmidi/*.java

# Compile the pmdefaults application.
javac -classpath . pmdefaults/*.java

# Temporarily copy the portmusic_logo.png file here to add to the jar file.
cp pmdefaults/portmusic_logo.png . 

# Create a directory to hold the distribution.
mkdir mac-osx

# Copy the interface DLL to the distribution directory.
cp ../Release/libpmjni.dylib mac-osx

# Create a java archive (jar) file of the distribution.
jar cmf pmdefaults/manifest.txt mac-osx/pmdefaults.jar pmdefaults/*.class portmusic_logo.png jportmidi/*.class

# Clean up the temporary image file now that it is in the jar file.
rm portmusic_logo.png

echo "You now have a jar file in mac-osx"

