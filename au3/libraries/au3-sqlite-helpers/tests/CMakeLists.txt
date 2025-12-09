#[[
Unit tests for au3-sqlite
]]

add_unit_test(
   NAME
      au3-sqlite-helpers
   SOURCES
      SQliteHelpersTests.cpp
   LIBRARIES
      au3-sqlite-helpers-interface
)
