# Remove unused Qt runtime content

if(NOT QT_DEPLOY_ROOT AND CMAKE_INSTALL_PREFIX)
    set(QT_DEPLOY_ROOT "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}")
endif()

if(NOT QT_DEPLOY_ROOT)
    message(FATAL_ERROR "QT_DEPLOY_ROOT is required")
endif()

get_filename_component(QT_DEPLOY_ROOT "${QT_DEPLOY_ROOT}" ABSOLUTE)

set(_qml_roots
    "${QT_DEPLOY_ROOT}/qml"
    "${QT_DEPLOY_ROOT}/audacity.app/Contents/Resources/qml"
)
set(_macos_quick_plugins_root
    "${QT_DEPLOY_ROOT}/audacity.app/Contents/PlugIns/quick"
)

# Remove unused optional child modules copied with their parent directories
set(_unused_qml_paths
    QtQml/StateMachine
    QtQml/XmlListModel
    QtQuick/NativeStyle
    QtQuick/Dialogs
    QtQuick/Controls/designer
    QtQuick/LocalStorage
    QtQuick/Particles
    QtQuick/Shapes/DesignHelpers
    QtQuick/VectorImage
    QtQuick/tooling

    # Keep only Fusion and its Basic fallback
    QtQuick/Controls/FluentWinUI3
    QtQuick/Controls/Imagine
    QtQuick/Controls/Material
    QtQuick/Controls/Universal
    QtQuick/Controls/Windows
    QtQuick/Controls/macOS
    QtQuick/Controls/iOS
)

foreach(_qml_root IN LISTS _qml_roots)
    if(NOT EXISTS "${_qml_root}")
        continue()
    endif()

    foreach(_relative_path IN LISTS _unused_qml_paths)
        set(_qml_path "${_qml_root}/${_relative_path}")

        # Remove flattened macOS plugins for pruned QML modules
        if(EXISTS "${_macos_quick_plugins_root}")
            file(GLOB_RECURSE _qmldir_files LIST_DIRECTORIES FALSE
                "${_qml_path}/qmldir"
            )
            foreach(_qmldir_file IN LISTS _qmldir_files)
                file(STRINGS "${_qmldir_file}" _plugin_lines
                    REGEX "^(optional[ \t]+)?plugin[ \t]+"
                )
                foreach(_plugin_line IN LISTS _plugin_lines)
                    string(REGEX REPLACE
                        "^(optional[ \t]+)?plugin[ \t]+([^ \t]+).*"
                        "\\2"
                        _plugin_name
                        "${_plugin_line}"
                    )
                    file(REMOVE
                        "${_macos_quick_plugins_root}/lib${_plugin_name}.dylib"
                    )
                endforeach()
            endforeach()
        endif()

        file(REMOVE_RECURSE "${_qml_path}")
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

# Remove unused QML tooling, SQL drivers, external widget styles, and translations
file(REMOVE_RECURSE
    "${QT_DEPLOY_ROOT}/bin/qmltooling"
    "${QT_DEPLOY_ROOT}/bin/sqldrivers"
    "${QT_DEPLOY_ROOT}/bin/styles"
    "${QT_DEPLOY_ROOT}/plugins/qmltooling"
    "${QT_DEPLOY_ROOT}/plugins/sqldrivers"
    "${QT_DEPLOY_ROOT}/plugins/styles"
    "${QT_DEPLOY_ROOT}/audacity.app/Contents/PlugIns/qmltooling"
    "${QT_DEPLOY_ROOT}/audacity.app/Contents/PlugIns/sqldrivers"
    "${QT_DEPLOY_ROOT}/audacity.app/Contents/PlugIns/styles"
    "${QT_DEPLOY_ROOT}/translations"
)

# Remove dangling macOS links left by pruned QML plugins
file(GLOB _macos_quick_plugins LIST_DIRECTORIES FALSE
    "${QT_DEPLOY_ROOT}/audacity.app/Contents/PlugIns/quick/*"
)
foreach(_plugin IN LISTS _macos_quick_plugins)
    if(IS_SYMLINK "${_plugin}" AND NOT EXISTS "${_plugin}")
        file(REMOVE "${_plugin}")
    endif()
endforeach()

# Remove the redundant compiler redistributable
file(GLOB _compiler_redistributables
    "${QT_DEPLOY_ROOT}/vc_redist*.exe"
    "${QT_DEPLOY_ROOT}/bin/vc_redist*.exe"
)
if(_compiler_redistributables)
    file(REMOVE ${_compiler_redistributables})
endif()

# Remove libraries backing the unused QML modules
set(_unused_qt_library_stems
    Qt6QmlLocalStorage
    Qt6Sql
    Qt6StateMachine
    Qt6StateMachineQml
    Qt6QmlXmlListModel
    Qt6QuickDialogs2
    Qt6QuickDialogs2QuickImpl
    Qt6QuickDialogs2Utils
    Qt6QuickParticles
    Qt6QuickShapesDesignHelpers
    Qt6QuickVectorImage
    Qt6QuickVectorImageGenerator
    Qt6QuickVectorImageHelpers
    Qt6QuickControls2FluentWinUI3StyleImpl
    Qt6QuickControls2Imagine
    Qt6QuickControls2ImagineStyleImpl
    Qt6QuickControls2Material
    Qt6QuickControls2MaterialStyleImpl
    Qt6QuickControls2Universal
    Qt6QuickControls2UniversalStyleImpl
    Qt6QuickControls2WindowsStyleImpl
    Qt6QuickControls2MacOSStyleImpl
    Qt6QuickControls2IOSStyleImpl
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

message(STATUS "Pruned unused Qt runtime content from ${QT_DEPLOY_ROOT}")
