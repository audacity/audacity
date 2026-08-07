if(NOT PAYLOAD_ROOT)
    message(FATAL_ERROR "PAYLOAD_ROOT is required")
endif()

get_filename_component(PAYLOAD_ROOT "${PAYLOAD_ROOT}" ABSOLUTE)
if(NOT IS_DIRECTORY "${PAYLOAD_ROOT}")
    message(FATAL_ERROR "Release payload does not exist: ${PAYLOAD_ROOT}")
endif()

file(GLOB_RECURSE _payload_entries LIST_DIRECTORIES FALSE RELATIVE "${PAYLOAD_ROOT}"
    "${PAYLOAD_ROOT}/*"
)
list(SORT _payload_entries)

if(MANIFEST_FILE)
    get_filename_component(_manifest_dir "${MANIFEST_FILE}" DIRECTORY)
    file(MAKE_DIRECTORY "${_manifest_dir}")
    string(REPLACE ";" "\n" _manifest "${_payload_entries}")
    file(WRITE "${MANIFEST_FILE}" "${_manifest}\n")
endif()

set(_forbidden_patterns
    "[.]ilk$"
    "(^|/)vc_redist[^/]*[.]exe$"
    "(^|/)translations/"
    "(^|/)qmltooling/"
    "(^|/)sqldrivers/"
    "Resources/Frameworks/qml/"
    "(^|/)qml/.*[.](a|prl|qmltypes)$"
    "(^|/)qml/Qt/labs/(animation|folderlistmodel|settings|sharedimage|synchronizer|wavefrontmesh)/"
    "(^|/)qml/(QtCore|QtGraphs|QtQuick3D|QtWebSockets)/"
    "(^|/)qml/QtQml/StateMachine/"
    "(^|/)qml/QtQml/XmlListModel/"
    "(^|/)qml/QtQuick/(Dialogs|LocalStorage|Particles|VectorImage|tooling)/"
    "(^|/)qml/QtQuick/Shapes/DesignHelpers/"
)

set(_allowed_macos_quick_plugins
    libeffectsplugin.dylib
    liblabsmodelsplugin.dylib
    liblabsplatformplugin.dylib
    libmodelsplugin.dylib
    libqmlplugin.dylib
    libqmlshapesplugin.dylib
    libqquicklayoutsplugin.dylib
    libqtgraphicaleffectsplugin.dylib
    libqtgraphicaleffectsprivateplugin.dylib
    libqtquick2plugin.dylib
    libqtquickcontrols2implplugin.dylib
    libqtquickcontrols2plugin.dylib
    libqtquicktemplates2plugin.dylib
    libquickwindowplugin.dylib
    libworkerscriptplugin.dylib
)

set(_missing_entries)
if(IS_DIRECTORY "${PAYLOAD_ROOT}/audacity.app" OR IS_DIRECTORY "${PAYLOAD_ROOT}/bin")
    set(_qml_root "")
    foreach(_qml_candidate
            "${PAYLOAD_ROOT}/qml"
            "${PAYLOAD_ROOT}/audacity.app/Contents/Resources/qml")
        if(IS_DIRECTORY "${_qml_candidate}")
            set(_qml_root "${_qml_candidate}")
            break()
        endif()
    endforeach()

    if(NOT _qml_root)
        list(APPEND _missing_entries "QML runtime root")
    else()
        set(_required_qml_modules
            Qt/labs/platform
            Qt/labs/qmlmodels
            Qt5Compat/GraphicalEffects
            QtQml
            QtQml/Models
            QtQml/WorkerScript
            QtQuick
            QtQuick/Controls
            QtQuick/Controls/impl
            QtQuick/Effects
            QtQuick/Layouts
            QtQuick/Shapes
            QtQuick/Templates
            QtQuick/Window
        )
        foreach(_module IN LISTS _required_qml_modules)
            if(NOT EXISTS "${_qml_root}/${_module}/qmldir")
                list(APPEND _missing_entries "qml/${_module}/qmldir")
            endif()
        endforeach()
    endif()

    if(IS_DIRECTORY "${PAYLOAD_ROOT}/audacity.app")
        foreach(_plugin IN LISTS _allowed_macos_quick_plugins)
            if(NOT EXISTS "${PAYLOAD_ROOT}/audacity.app/Contents/PlugIns/quick/${_plugin}")
                list(APPEND _missing_entries "PlugIns/quick/${_plugin}")
            endif()
        endforeach()
    endif()
endif()

if(_missing_entries)
    string(REPLACE ";" "\n  " _formatted "${_missing_entries}")
    message(FATAL_ERROR "Required release payload entries are missing:\n  ${_formatted}")
endif()

set(_forbidden_entries)
foreach(_entry IN LISTS _payload_entries)
    if(_entry MATCHES "(^|/)PlugIns/quick/([^/]+)$")
        set(_plugin_name "${CMAKE_MATCH_2}")
        if(NOT _plugin_name IN_LIST _allowed_macos_quick_plugins
           AND NOT _plugin_name MATCHES "^libqtquickcontrols2.*plugin[.]dylib$")
            list(APPEND _forbidden_entries "${_entry}")
            continue()
        endif()
    endif()

    foreach(_pattern IN LISTS _forbidden_patterns)
        if(_entry MATCHES "${_pattern}")
            list(APPEND _forbidden_entries "${_entry}")
            break()
        endif()
    endforeach()
endforeach()

if(_forbidden_entries)
    string(REPLACE ";" "\n  " _formatted "${_forbidden_entries}")
    message(FATAL_ERROR "Forbidden release payload entries:\n  ${_formatted}")
endif()

list(LENGTH _payload_entries _payload_entry_count)
message(STATUS "Audited ${_payload_entry_count} files in ${PAYLOAD_ROOT}")
