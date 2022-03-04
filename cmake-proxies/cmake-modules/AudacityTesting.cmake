if( ${_OPT}has_tests )

   set( TESTS_DIR "${CMAKE_BINARY_DIR}/tests" )

   if( CMAKE_CONFIGURATION_TYPES )
      set( TESTS_DEST_DIR "${CMAKE_BINARY_DIR}/tests/${CMAKE_CFG_INTDIR}" )
   else()
      set( TESTS_DEST_DIR "${CMAKE_BINARY_DIR}/tests/${CMAKE_BUILD_TYPE}" )
   endif()

   # Install Catch2 framework

   add_conan_lib(
      Catch2
      catch2/2.13.8
      REQUIRED
      ALWAYS_ALLOW_CONAN_FALLBACK
   )

   # Setup default CTest arguments when running from IDE

   set(CMAKE_CTEST_ARGUMENTS "--output-on-failure;--verbose;${CMAKE_CTEST_ARGUMENTS}")

   enable_testing()

   #[[
      add_unit_test(NAME name SOURCES file1 ... LIBRARIES lib1 ...)

      Creates an executable called ${name}-test from the source files ${file1}, ... and linked
      to libraries ${lib1}, ... Catch2 is linked implicitly.

      Create a CTest test called ${name}, that expects 0 code on success
   ]]
   function( add_unit_test )
      cmake_parse_arguments(
         ADD_UNIT_TEST # Prefix
         "" # Options
         "NAME" # One value keywords
         "SOURCES;LIBRARIES"
         ${ARGN}
      )

      if( NOT ADD_UNIT_TEST_NAME )
         message( FATAL_ERROR "Missing required NAME parameter for the add_unit_test")
      endif()

      set( test_executable_name "${ADD_UNIT_TEST_NAME}-test" )

      # Create test executable

      add_executable( ${test_executable_name} ${ADD_UNIT_TEST_SOURCES} "${CMAKE_SOURCE_DIR}/tests/Catch2Main.cpp")
      target_link_libraries( ${test_executable_name} ${ADD_UNIT_TEST_LIBRARIES} Catch2::Catch2 )

      set( OPTIONS )
      audacity_append_common_compiler_options( OPTIONS NO )
      target_compile_options( ${test_executable_name} ${OPTIONS} )

      set_target_properties(
         ${test_executable_name}
         PROPERTIES
            FOLDER "tests" # for IDE organization
            RUNTIME_OUTPUT_DIRECTORY ${TESTS_DIR}
            BUILD_RPATH ${_SHARED_PROXY_PATH}
            # Allow running tests from Visual Studio by setting up the proper PATH
            VS_DEBUGGER_ENVIRONMENT "PATH=${_SHARED_PROXY_PATH};%PATH%"
      )

      # Register unit test with CTest

      add_test(
         NAME
            ${ADD_UNIT_TEST_NAME}
         COMMAND
            ${test_executable_name}
      )

      set_tests_properties(
         ${ADD_UNIT_TEST_NAME}
         PROPERTIES
            LABELS "unit_tests"
      )

      if( WIN32 )
         # On Windows, set the PATH so it points to the DLL location
         # This is required to avoid invoking CopyLibs.

         # CTest expects that ENVIRONMENT is a CMake list.
         # Escape ';' so PATH is handled correctly
         string(REPLACE ";" "\\;" escaped_path "$ENV{PATH}")

         set_tests_properties(
            ${ADD_UNIT_TEST_NAME}
            PROPERTIES
               ENVIRONMENT "PATH=$<SHELL_PATH:${_SHARED_PROXY_BASE_PATH}/$<CONFIG>>\\;${escaped_path}"
         )
      elseif( APPLE )
         # We target an old version of macOS, disable std::uncaught_exceptions
         target_compile_definitions( ${test_executable_name} PRIVATE CATCH_CONFIG_NO_CPP17_UNCAUGHT_EXCEPTIONS )
         # Similar to Windows, but uses DYLD_FALLBACK_LIBRARY_PATH instead of PATH
         set_tests_properties(
            ${ADD_UNIT_TEST_NAME}
            PROPERTIES
               ENVIRONMENT "DYLD_FALLBACK_LIBRARY_PATH=$<SHELL_PATH:${_SHARED_PROXY_BASE_PATH}/$<CONFIG>>"
         )
      endif()
   endfunction()

   set( JOURNAL_TEST_TIMEOUT_SECONDS 180 )

   #[[
      add_journal_test(journal_file)

      Adds a test, that runs Audacity with the journal ${journal_file}.

      Test name is based on the name component of the ${journal_file}
   ]]
   function( add_journal_test journal_file )
      get_filename_component(test_name ${journal_file} NAME_WE)

      if( APPLE )
         # On macOS CMake will generate a placeholder that CTest fails to handle correctly,
         # so we have to setup the path manually
         set( audacity_target "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/Audacity.app/Contents/MacOS/Audacity" )
      elseif (WIN32)
         set( audacity_target
            powershell
               -ExecutionPolicy Bypass
               -File "${CMAKE_SOURCE_DIR}/tests/journals/test_runner.ps1"
               "$<TARGET_FILE:Audacity>"
               --timeout ${JOURNAL_TEST_TIMEOUT_SECONDS}
         )
      else()
         set( audacity_target "$<TARGET_FILE:Audacity>" )
      endif()

      # Adds a test that invokes Audacity with the given journal file

      add_test(
         NAME
            ${test_name}
         COMMAND
            ${audacity_target} --journal ${journal_file}
      )

      set_tests_properties(
         ${test_name}
         PROPERTIES
            LABELS "journal_tests"
            TIMEOUT ${JOURNAL_TEST_TIMEOUT_SECONDS}
      )
   endfunction()
else()
   # Just a placeholder for the cases unit testing is disabled
   function(add_unit_test)
   endfunction()

   function( add_journal_test journal_file )
   endfunction()
endif()
