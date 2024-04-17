#[[
Unit tests for lib-string-utils
]]

add_unit_test(
   NAME
      lib-string-utils
   SOURCES
      FromCharsTests.cpp
      StringUtilsTests.cpp
      UriParserTests.cpp
   LIBRARIES
      lib-string-utils
      wxBase
)
