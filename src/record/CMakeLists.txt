#
# Audacity: A Digital Audio Editor
#

muse_create_module(record NO_UNITY)

target_sources(record PRIVATE
    #public
    recordmodule.cpp
    recordmodule.h
    recorderrors.h
    irecordconfiguration.h
    irecordcontroller.h
    irecordmetercontroller.h
    irecord.h
    iaudioinput.h

    #internal
    internal/recordconfiguration.cpp
    internal/recordconfiguration.h
    internal/recordcontroller.cpp
    internal/recordcontroller.h
    internal/recordmetercontroller.cpp
    internal/recordmetercontroller.h
    internal/recorduiactions.cpp
    internal/recorduiactions.h

    #internal/au3
    internal/au3/au3audioinput.cpp
    internal/au3/au3audioinput.h
    internal/au3/au3record.cpp
    internal/au3/au3record.h

    view/toolbars/playbacktoolbarrecordlevelitem.cpp
    view/toolbars/playbacktoolbarrecordlevelitem.h
    )

target_link_libraries(record PRIVATE
        au3wrap
        au3-audio-devices
        au3-audio-io
        au3-project-rate
        au3-stretching-sequence
        au3-strings
        au3-time-frequency-selection
        au3-track
        au3-wave-track
)

add_subdirectory(qml/Audacity/Record)

if (AU_BUILD_RECORD_TESTS)
    add_subdirectory(tests)
endif()
