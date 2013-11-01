@echo off

rem Compile the java PortMidi interface classes.
javac jportmidi/*.java

rem Compile the pmdefaults application.
javac -classpath . pmdefaults/*.java

rem Temporarily copy the portmusic_logo.png file here to add to the jar file.
copy pmdefaults\portmusic_logo.png . > nul

rem Create a directory to hold the distribution.
mkdir win32

rem Attempt to copy the interface DLL to the distribution directory.

if exist "..\release\pmjni.dll"  goto have-dll

echo "ERROR: pmjni.dll not found!"
exit /b 1

:have-dll
copy "..\release\pmjni.dll" win32\pmjni.dll > nul

rem Create a java archive (jar) file of the distribution.
jar cmf pmdefaults\manifest.txt win32\pmdefaults.jar pmdefaults\*.class portmusic_logo.png jportmidi\*.class

rem Clean up the temporary image file now that it is in the jar file.
del portmusic_logo.png

rem Copy the java execution code obtained from
rem http://devwizard.free.fr/html/en/JavaExe.html to the distribution
rem directory.  The copy also renames the file to our desired executable
rem name.
copy JavaExe.exe win32\pmdefaults.exe > nul

rem Integrate the icon into the executable using UpdateRsrcJavaExe from
rem http://devwizard.free.fr
UpdateRsrcJavaExe -run -exe=win32\pmdefaults.exe -ico=pmdefaults\pmdefaults.ico

rem Copy the 32-bit windows read me file to the distribution directory.
copy pmdefaults\readme-win32.txt win32\README.txt > nul

rem Copy the license file to the distribution directory.
copy pmdefaults\pmdefaults-license.txt win32\license.txt > nul

echo "You can run pmdefaults.exe in win32"
