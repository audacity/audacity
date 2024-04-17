#[[
Unit tests for lib-sqlite
]]

add_unit_test(
   NAME
      lib-sqlite-helpers
   SOURCES
      SQliteHelpersTests.cpp
   LIBRARIES
      lib-sqlite-helpers-interface
)
