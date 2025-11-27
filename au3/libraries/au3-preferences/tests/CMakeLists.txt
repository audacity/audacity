#[[
Unit tests for au3-preferences
]]

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
