#
# Audacity: A Digital Audio Editor
#

declare_module(trackedit)

set(MODULE_QRC trackedit.qrc)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/trackeditmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/trackeditmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/trackedittypes.h
    ${CMAKE_CURRENT_LIST_DIR}/itrackeditproject.h
    ${CMAKE_CURRENT_LIST_DIR}/itrackeditinteraction.h
    ${CMAKE_CURRENT_LIST_DIR}/itrackeditactionscontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/iselectioncontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/itracksinteraction.h
    ${CMAKE_CURRENT_LIST_DIR}/iclipsinteraction.h
    ${CMAKE_CURRENT_LIST_DIR}/ilabelsinteraction.h
    ${CMAKE_CURRENT_LIST_DIR}/iprojecthistory.h
    ${CMAKE_CURRENT_LIST_DIR}/itrackeditconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/itrackeditclipboard.h
    ${CMAKE_CURRENT_LIST_DIR}/timespan.cpp
    ${CMAKE_CURRENT_LIST_DIR}/timespan.h
    ${CMAKE_CURRENT_LIST_DIR}/trackediterrors.h
    ${CMAKE_CURRENT_LIST_DIR}/trackeditutils.cpp
    ${CMAKE_CURRENT_LIST_DIR}/trackeditutils.h

    ${CMAKE_CURRENT_LIST_DIR}/dom/track.h
    ${CMAKE_CURRENT_LIST_DIR}/dom/clip.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dom/clip.h
    ${CMAKE_CURRENT_LIST_DIR}/dom/label.h
    ${CMAKE_CURRENT_LIST_DIR}/dom/wave.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dom/wave.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/snapshotspectrogramconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackspectrogramsettingsupdater.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackspectrogramsettingsupdater.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/trackeditinteraction.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackeditinteraction.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackeditoperationcontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackeditoperationcontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackeditconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackeditconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackedituiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackedituiactions.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackeditactionscontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackeditactionscontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/tracknavigationcontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/tracknavigationcontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/itracknavigationcontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/changedetection.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/changedetection.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/deletebehavioronboardingscenario.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/deletebehavioronboardingscenario.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3trackeditproject.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3trackeditproject.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3selectioncontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3selectioncontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3selectionsaver.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3selectionsaver.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3interactiontypes.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3interactionutils.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3interactionutils.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3projecthistory.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3projecthistory.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3trackeditclipboard.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3trackeditclipboard.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3trackdata.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3trackdata.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/selectionrestorer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/selectionrestorer.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3tracksinteraction.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3tracksinteraction.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3clipsinteraction.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3clipsinteraction.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3labelsinteraction.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3labelsinteraction.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/iundomanager.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/undomanager.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/undomanager.h

    ${CMAKE_CURRENT_LIST_DIR}/view/deletebehaviorpanelmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/deletebehaviorpanelmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/pastebehaviorpanelmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/pastebehaviorpanelmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/tracknavigationmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/tracknavigationmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspectrogramsettingsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspectrogramsettingsmodel.h
    )

# AU3
# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../au3wrap/au3wrapDefs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK au3wrap)

set(MODULE_LINK
    ${MODULE_LINK}
    muse::draw
)

set(MODULE_USE_UNITY OFF)

setup_module()

if (AU_BUILD_TRACKEDIT_TESTS)
    add_subdirectory(tests)
endif()
