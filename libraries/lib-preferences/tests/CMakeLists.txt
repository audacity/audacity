set(SOURCES
   SettingsTestsCommon.cpp
   SettingsTestsCommon.h   
)

set(LIBRARIES
   lib-preferences
)

if(NOT ${_OPT}enable_experimental_qt_support)
   list(APPEND SOURCES SettingsWXTests.cpp)
   list(APPEND LIBRARIES lib-wx-init)
endif()

add_unit_test(
   NAME
      lib-preferences
   SOURCES ${SOURCES}
   LIBRARIES ${LIBRARIES}
)
