# pm_java

if(UNIX)
  if(APPLE)
    # java not dealt with in CMake -- see pm_mac/pm_mac.xcodeproj
  else(APPLE)
    # linux
    set(JPORTMIDICLASS JPortMidi.class JPortMidiException.class
    		       JPortMidiApi.class)
    set(PMDEFAULTSCLASS PmDefaultsFrame.class PmDefaults.class)
    prepend_path(JPORTMIDICLASS2 jportmidi/ ${JPORTMIDICLASS})
    prepend_path(PMDEFAULTSCLASS2 pmdefaults/ ${PMDEFAULTSCLASS})
    set(PMDEFAULTS_ALL_CLASSES ${JPORTMIDICLASS2} ${PMDEFAULTSCLASS2})
    # message(STATUS "PMDEFAULTS_ALL_CLASSES is " ${PMDEFAULTS_ALL_CLASSES})
    add_custom_command(OUTPUT pmdefaults/PmDefaultsFrame.class
        COMMAND javac -classpath . pmdefaults/PmDefaultsFrame.java
	MAIN_DEPENDENCY pmdefaults/PmDefaultsFrame.java
	DEPENDS pmdefaults/PmDefaults.java
	WORKING_DIRECTORY pm_java)
    add_custom_command(OUTPUT pmdefaults/PmDefaults.class
        COMMAND javac -classpath . pmdefaults/PmDefaults.java
	MAIN_DEPENDENCY pmdefaults/PmDefaults.java
	DEPENDS pmdefaults/PmDefaultsFrame.java
	WORKING_DIRECTORY pm_java)
    add_custom_command(OUTPUT ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/pmdefaults.jar
        COMMAND	cp pmdefaults/portmusic_logo.png .
        COMMAND	jar cmf pmdefaults/manifest.txt pmdefaults.jar
		pmdefaults/*.class portmusic_logo.png jportmidi/*.class
  	COMMAND chmod +x pmdefaults/pmdefaults
	COMMAND cp pmdefaults/pmdefaults ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
	COMMAND mv pmdefaults.jar ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
	COMMAND rm portmusic_logo.png
	MAIN_DEPENDENCY pmdefaults/PmDefaults.class
	DEPENDS ${PMDEFAULTS_ALL_CLASSES}
	WORKING_DIRECTORY pm_java)
    add_custom_target(pmdefaults_target ALL 
        DEPENDS ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/pmdefaults.jar)
    # message(STATUS "add_custom_target: pmdefaults.jar")

    # install the libraries (Linux only)
    INSTALL(FILES ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/pmdefaults.jar
      DESTINATION /usr/share/java)
    INSTALL(PROGRAMS ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/pmdefaults
      DESTINATION /usr/local/bin)
  endif(APPLE)
endif(UNIX)
# In windows, use pm_java/make.bat
