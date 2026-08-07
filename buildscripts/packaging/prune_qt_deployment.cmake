# Remove Qt deployment content that is outside Audacity's supported runtime
# surface. This script is shared by the Windows, macOS, and Linux packagers.

if(NOT QT_DEPLOY_ROOT)
    message(FATAL_ERROR "QT_DEPLOY_ROOT is required")
endif()

get_filename_component(QT_DEPLOY_ROOT "${QT_DEPLOY_ROOT}" ABSOLUTE)

set(_qml_roots
    "${QT_DEPLOY_ROOT}/qml"
    "${QT_DEPLOY_ROOT}/audacity.app/Contents/Resources/qml"
)

# This was the location used by the old blanket macOS QML copy. The generated
# Qt deploy script uses Contents/Resources/qml instead, so the legacy tree must
# never survive an incremental install.
file(REMOVE_RECURSE
    "${QT_DEPLOY_ROOT}/audacity.app/Contents/Resources/Frameworks/qml"
)

set(_unsupported_qml_paths
    QtCore
    QtGraphs
    QtQml/StateMachine
    QtQml/XmlListModel
    QtQuick/Dialogs
    QtQuick/LocalStorage
    QtQuick/Particles
    QtQuick/Shapes/DesignHelpers
    QtQuick/VectorImage
    QtQuick/tooling
    QtQuick3D
    QtWebSockets
)

foreach(_qml_root IN LISTS _qml_roots)
    if(NOT EXISTS "${_qml_root}")
        continue()
    endif()

    foreach(_relative_path IN LISTS _unsupported_qml_paths)
        file(REMOVE_RECURSE "${_qml_root}/${_relative_path}")
    endforeach()

    file(GLOB _qt_labs_modules LIST_DIRECTORIES TRUE "${_qml_root}/Qt/labs/*")
    foreach(_qt_labs_module IN LISTS _qt_labs_modules)
        if(NOT _qt_labs_module STREQUAL "${_qml_root}/Qt/labs/platform"
           AND NOT _qt_labs_module STREQUAL "${_qml_root}/Qt/labs/qmlmodels")
            file(REMOVE_RECURSE "${_qt_labs_module}")
        endif()
    endforeach()

    file(GLOB_RECURSE _qml_development_files LIST_DIRECTORIES FALSE
        "${_qml_root}/*.a"
        "${_qml_root}/*.prl"
        "${_qml_root}/*.qmltypes"
    )
    if(_qml_development_files)
        file(REMOVE ${_qml_development_files})
    endif()
endforeach()

# QML debugging/tooling and SQL drivers are not part of the release runtime.
file(REMOVE_RECURSE
    "${QT_DEPLOY_ROOT}/bin/qmltooling"
    "${QT_DEPLOY_ROOT}/bin/sqldrivers"
    "${QT_DEPLOY_ROOT}/plugins/qmltooling"
    "${QT_DEPLOY_ROOT}/plugins/sqldrivers"
    "${QT_DEPLOY_ROOT}/audacity.app/Contents/PlugIns/qmltooling"
    "${QT_DEPLOY_ROOT}/audacity.app/Contents/PlugIns/sqldrivers"
    "${QT_DEPLOY_ROOT}/translations"
)

# macdeployqt flattens QML plugins into Contents/PlugIns/quick, independently
# of the QML module directories above. Keep only the plugins backing Audacity's
# production and documented extension-import surface.
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
file(GLOB _macos_quick_plugins LIST_DIRECTORIES FALSE
    "${QT_DEPLOY_ROOT}/audacity.app/Contents/PlugIns/quick/*"
)
foreach(_plugin IN LISTS _macos_quick_plugins)
    get_filename_component(_plugin_name "${_plugin}" NAME)
    if(NOT _plugin_name IN_LIST _allowed_macos_quick_plugins
       AND NOT _plugin_name MATCHES "^libqtquickcontrols2.*plugin[.]dylib$")
        file(REMOVE "${_plugin}")
    endif()
endforeach()

# The compiler runtime DLLs are deployed app-locally on Windows. The separate
# redistributable executable is not run by the installer and is redundant.
file(GLOB _compiler_redistributables
    "${QT_DEPLOY_ROOT}/vc_redist*.exe"
    "${QT_DEPLOY_ROOT}/bin/vc_redist*.exe"
)
if(_compiler_redistributables)
    file(REMOVE ${_compiler_redistributables})
endif()

set(_unused_qt_library_stems
    Qt6Graphs
    Qt6GraphsWidgets
    Qt6LabsAnimation
    Qt6LabsFolderListModel
    Qt6LabsSettings
    Qt6LabsSharedImage
    Qt6LabsSynchronizer
    Qt6LabsWavefrontMesh
    Qt6QmlLocalStorage
    Qt6StateMachine
    Qt6StateMachineQml
    Qt6QmlXmlListModel
    Qt6Quick3DUtils
    Qt6Quick3D
    Qt6Quick3DAssetImport
    Qt6Quick3DAssetUtils
    Qt6Quick3DEffects
    Qt6Quick3DHelpers
    Qt6Quick3DHelpersImpl
    Qt6Quick3DParticleEffects
    Qt6Quick3DParticles
    Qt6Quick3DRuntimeRender
    Qt6Quick3DXr
    Qt6QuickDialogs2
    Qt6QuickDialogs2QuickImpl
    Qt6QuickDialogs2Utils
    Qt6QuickParticles
    Qt6QuickShapesDesignHelpers
    Qt6QuickTest
    Qt6QuickVectorImage
    Qt6QuickVectorImageGenerator
    Qt6QuickVectorImageHelpers
    Qt6QuickWidgets
    Qt6Sql
    Qt6WebSockets
)

foreach(_stem IN LISTS _unused_qt_library_stems)
    file(GLOB _unused_qt_libraries
        "${QT_DEPLOY_ROOT}/bin/${_stem}.dll"
        "${QT_DEPLOY_ROOT}/lib/lib${_stem}.so*"
        "${QT_DEPLOY_ROOT}/lib/lib${_stem}.dylib"
    )
    if(_unused_qt_libraries)
        file(REMOVE ${_unused_qt_libraries})
    endif()

    string(REGEX REPLACE "^Qt6" "Qt" _framework_stem "${_stem}")
    file(REMOVE_RECURSE
        "${QT_DEPLOY_ROOT}/audacity.app/Contents/Frameworks/${_framework_stem}.framework"
    )
endforeach()

message(STATUS "Pruned unsupported Qt deployment content from ${QT_DEPLOY_ROOT}")
