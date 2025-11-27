#[[
Unit tests for au3-string-utils
]]

add_unit_test(
   NAME
      au3-string-utils
   SOURCES
      FromCharsTests.cpp
      StringUtilsTests.cpp
      UriParserTests.cpp
   LIBRARIES
      au3-string-utils
      wxBase
)
