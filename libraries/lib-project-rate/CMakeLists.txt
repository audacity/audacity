list( APPEND SOURCES
   ProjectRate.cpp
   ProjectRate.h
   QualitySettings.cpp
   QualitySettings.h
)
audacity_library( lib-project-rate "${SOURCES}"
   "lib-project-interface;lib-audio-devices-interface;lib-xml-interface;lib-math-interface;PRIVATE;wxBase"
   "" ""
)
