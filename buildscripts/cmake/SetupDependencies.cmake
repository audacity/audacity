
message(STATUS "Setup dependencies")

include(GetPlatformInfo)


set(EXTDEPS_DIR "${CMAKE_SOURCE_DIR}/muse_deps")
if (NOT EXISTS "${EXTDEPS_DIR}/buildtools/manifest.cmake")
    message(FATAL_ERROR "muse_deps submodule missing. Run: git submodule update --init muse_deps")
endif()

set(LOCAL_ROOT_PATH ${FETCHCONTENT_BASE_DIR})
include(${EXTDEPS_DIR}/buildtools/manifest.cmake)

include(${CMAKE_CURRENT_LIST_DIR}/DependencyManifest.cmake)
include(${MUSE_FRAMEWORK_PATH}/buildscripts/cmake/ExtDepsManifest.cmake)

# bundle each consumed dep's runtime libs + licenses into the app.
extdeps_install_consumed(MACOS_BUNDLE audacity.app)

# Pre-fetch dep sources into the cache so source/REBUILD builds can run offline.
add_custom_target(prepare_deps_sources
    COMMAND ${CMAKE_COMMAND} -P "${CMAKE_CURRENT_LIST_DIR}/PrepareDepsSources.cmake"
    COMMENT "Pre-fetching dependency sources into the cache"
    VERBATIM
)
