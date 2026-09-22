cmake_minimum_required(VERSION 3.10)

project(vamp-plugin-sdk)

if(MSVC)
    add_compile_options("/W4")
else()
    add_compile_options("-Wall" "-Wextra")
endif()

# sdk
add_library(
    vamp-sdk
    src/vamp-sdk/FFT.cpp
    src/vamp-sdk/PluginAdapter.cpp
    src/vamp-sdk/RealTime.cpp
    src/vamp-sdk/acsymbols.c
)
set_target_properties(vamp-sdk PROPERTIES POSITION_INDEPENDENT_CODE ON)  # to be linked to shared plugin libraries
target_include_directories(vamp-sdk PUBLIC .)
target_compile_features(vamp-sdk PUBLIC cxx_std_11)
add_library(vamp-plugin-sdk::vamp-sdk ALIAS vamp-sdk)

# host sdk
add_library(
    vamp-hostsdk
    src/vamp-hostsdk/Files.cpp
    src/vamp-hostsdk/PluginBufferingAdapter.cpp
    src/vamp-hostsdk/PluginChannelAdapter.cpp
    src/vamp-hostsdk/PluginHostAdapter.cpp
    src/vamp-hostsdk/PluginInputDomainAdapter.cpp
    src/vamp-hostsdk/PluginLoader.cpp
    src/vamp-hostsdk/PluginSummarisingAdapter.cpp
    src/vamp-hostsdk/PluginWrapper.cpp
    src/vamp-hostsdk/RealTime.cpp
    src/vamp-hostsdk/host-c.cpp
    src/vamp-hostsdk/acsymbols.c
)
target_link_libraries(vamp-hostsdk PUBLIC ${CMAKE_DL_LIBS})
target_include_directories(vamp-hostsdk PUBLIC .)
target_compile_definitions(vamp-hostsdk PUBLIC _USE_MATH_DEFINES)  # for e.g. M_PI constant
target_compile_features(vamp-hostsdk PUBLIC cxx_std_11)
add_library(vamp-plugin-sdk::vamp-hostsdk ALIAS vamp-hostsdk)

# rdf generator
option(VAMPSDK_BUILD_RDFGEN "Build RDF template generator" OFF)
if(VAMPSDK_BUILD_RDFGEN)
    add_executable(vamp-rdf-template-generator rdf/generator/vamp-rdf-template-generator.cpp)
    target_link_libraries(vamp-rdf-template-generator PRIVATE vamp-hostsdk)
endif()

# example plugins
option(VAMPSDK_BUILD_EXAMPLE_PLUGINS "Build example plugins" OFF)
if(VAMPSDK_BUILD_EXAMPLE_PLUGINS)
    file(GLOB sources_plugins "examples/*.cpp")
    add_library(vamp-example-plugins SHARED ${sources_plugins})
    set_target_properties(vamp-example-plugins PROPERTIES PREFIX "")  # remove lib* prefix
    target_link_libraries(vamp-example-plugins PRIVATE vamp-sdk)
    if(MSVC)
        set_target_properties(vamp-example-plugins PROPERTIES LINK_FLAGS /EXPORT:vampGetPluginDescriptor)
    else()
        if(APPLE)
            set_target_properties(vamp-example-plugins PROPERTIES LINK_FLAGS "-exported_symbols_list ${CMAKE_SOURCE_DIR}/vamp-plugin.list")
        else()
            set_target_properties(vamp-example-plugins PROPERTIES LINK_FLAGS "-Wl,--version-script=${CMAKE_SOURCE_DIR}/vamp-plugin.map")
        endif(APPLE)
    endif(MSVC)
endif()

# simple host
option(VAMPSDK_BUILD_SIMPLE_HOST "Build simple host" OFF)
if(VAMPSDK_BUILD_SIMPLE_HOST)
    find_path(LIBSNDFILE_INCLUDE_DIR NAMES sndfile.h)
    find_library(LIBSNDFILE_LIBRARY NAMES sndfile)

    include(FindPackageHandleStandardArgs)
    find_package_handle_standard_args(
        SndFile
        REQUIRED_VARS
            LIBSNDFILE_LIBRARY
            LIBSNDFILE_INCLUDE_DIR
    )

    add_executable(vamp-simple-host host/vamp-simple-host.cpp)
    target_link_libraries(vamp-simple-host PRIVATE vamp-hostsdk ${LIBSNDFILE_LIBRARY})
    target_include_directories(vamp-simple-host PRIVATE ${LIBSNDFILE_INCLUDE_DIR})
endif()

# install

option(VAMPSDK_ENABLE_INSTALL "Enable to add install directives" ON)

if(VAMPSDK_ENABLE_INSTALL)
    include(GNUInstallDirs)

    install(TARGETS vamp-sdk vamp-hostsdk EXPORT VampSdkTargets
      RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
      ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
      LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
      PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

    file (GLOB VAMP_HEADERS "vamp/*.h")
    install (FILES ${VAMP_HEADERS} DESTINATION include/vamp)

    file (GLOB VAMP_SDK_HEADERS "vamp-sdk/*.h")
    install (FILES ${VAMP_SDK_HEADERS} DESTINATION include/vamp-sdk)

    file (GLOB VAMP_HOSTSDK_HEADERS "vamp-hostsdk/*.h")
    install (FILES ${VAMP_HOSTSDK_HEADERS} DESTINATION include/vamp-hostsdk)

    # The following code for installing the pc files could be simplified
    # in case support for native configure/make was dropped.

    macro(configure_pc_file file)
      file(READ "pkgconfig/${file}.in" FILE_CONTENT)
      string(REPLACE "%PREFIX%" "@prefix@" FILE_CONTENT "${FILE_CONTENT}" )
      file(WRITE "${CMAKE_BINARY_DIR}/${file}.in" "${FILE_CONTENT}")
      configure_file("${CMAKE_BINARY_DIR}/${file}.in" ${file} @ONLY)
    endmacro()

    set(prefix ${CMAKE_INSTALL_PREFIX})
    configure_pc_file(vamp.pc)
    configure_pc_file(vamp-sdk.pc)
    configure_pc_file(vamp-hostsdk.pc)
    install(FILES
        ${CMAKE_CURRENT_BINARY_DIR}/vamp.pc
        ${CMAKE_CURRENT_BINARY_DIR}/vamp-sdk.pc
        ${CMAKE_CURRENT_BINARY_DIR}/vamp-hostsdk.pc
      DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)
endif()
