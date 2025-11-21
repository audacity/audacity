#[[
Unit tests for lib-preferences
]]

add_unit_test(
   NAME
      lib-preferences
   SOURCES
      SettingsTestsCommon.cpp
      SettingsTestsCommon.h
      SettingsWXTests.cpp
   LIBRARIES
      lib-preferences
      lib-wx-init
)
