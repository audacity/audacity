#
# Audacity: A Digital Audio Editor
#

declare_module(importer)

set(MODULE_SRC
    # public
    ${CMAKE_CURRENT_LIST_DIR}/importermodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/importermodule.h
    ${CMAKE_CURRENT_LIST_DIR}/iimporter.h
    ${CMAKE_CURRENT_LIST_DIR}/iimporterconfiguration.h

    # internals
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3importer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3importer.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/importerconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/importerconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/legacyaupimporter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/legacyaupimporter.h

    # tempo detection
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/tempodetection.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/tempodetection.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/lteaudioreaderbridge.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/lteaudioreaderbridge.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/filenamebpmparser.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/filenamebpmparser.h

    ${CMAKE_CURRENT_LIST_DIR}/types/importtypes.h
)

loop_tempo_estimator_add_to_build()

set(MODULE_LINK
        au3wrap
        au3-audio-graph
        au3-basic-ui
        au3-channel
        au3-exceptions
        au3-files
        au3-import-export
        au3-label-track
        au3-math
        au3-mixer
        au3-numeric-formats
        au3-playable-track
        au3-preferences
        au3-project
        au3-project-file-io
        au3-project-history
        au3-registries
        au3-sample-track
        au3-stretching-sequence
        au3-strings
        au3-tags
        au3-time-frequency-selection
        au3-track
        au3-utility
        au3-wave-track
        au3-xml
        loop-tempo-estimator
        SndFile::sndfile
)

set(MODULE_USE_UNITY OFF)

setup_module()

if (AU_BUILD_IMPORT_TESTS)
    add_subdirectory(tests)
endif()
