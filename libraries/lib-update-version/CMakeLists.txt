set( TARGET lib-update-version )
set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

def_vars()

set( SOURCES
    VersionId.h
    VersionId.cpp

    VersionPatch.h

    ServerCommunication.h
    ServerCommunication.cpp

    UpdateDataParser.h
    UpdateDataParser.cpp

    UpdateManager.h
    UpdateManager.cpp

    UpdatePopupDialog.h
    UpdatePopupDialog.cpp
)

set ( LIBRARIES PRIVATE
    lib-network-manager
    wxwidgets::base
)

set( DEFINES INTERFACE "HAS_UPDATES_CHECK" )

audacity_library ( ${TARGET} "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
