#[[
Unit tests for au3-preferences
]]

# SettingsWXTests exercises SettingsWX from au3-wx-init, which AU4 does not build yet
# (commented out in au3/libraries/CMakeLists.txt). Enable the test together with that library.
if("au3-wx-init" IN_LIST AU3_ALL_LIBRARIES)
   add_unit_test(
      NAME
         au3-preferences
      SOURCES
         SettingsTestsCommon.cpp
         SettingsTestsCommon.h
         SettingsWXTests.cpp
      LIBRARIES
         au3-preferences
         au3-wx-init
   )
endif()
