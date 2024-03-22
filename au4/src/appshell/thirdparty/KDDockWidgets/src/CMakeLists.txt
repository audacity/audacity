#
# This file is part of KDDockWidgets.
#
# SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
# Author: Sergio Martins <sergio.martins@kdab.com>
#
# SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only
#
# Contact KDAB at <info@kdab.com> for commercial licensing options.
#

if(POLICY CMP0043)
  cmake_policy(SET CMP0043 NEW)
endif()

add_definitions(-DQT_NO_SIGNALS_SLOTS_KEYWORDS
                -DQT_USE_QSTRINGBUILDER
                -DQT_NO_NARROWING_CONVERSIONS_IN_CONNECT
                -DQT_STRICT_ITERATORS
                -DQT_NO_KEYWORDS
                -DQT_NO_FOREACH
)

set(DOCKSLIBS_SRCS
    Config.cpp
    Config.h
    Qt5Qt6Compat_p.h
    FocusScope.cpp
    FocusScope.h
    FrameworkWidgetFactory.cpp
    FrameworkWidgetFactory.h
    DockWidgetBase.cpp
    DockWidgetBase.h
    MainWindowBase.cpp
    MainWindowBase.h
    MainWindowMDI.cpp
    MainWindowMDI.h
    LayoutSaver.cpp
    LayoutSaver.h
    private/LayoutSaver_p.h
    private/LayoutWidget.cpp
    private/LayoutWidget_p.h
    private/MDILayoutWidget.cpp
    private/MDILayoutWidget_p.h
    private/MultiSplitter.cpp
    private/MultiSplitter_p.h
    private/Position.cpp
    private/Position_p.h
    private/DropIndicatorOverlayInterface.cpp
    private/DropIndicatorOverlayInterface_p.h
    private/DropArea.cpp
    private/DropArea_p.h
    private/FloatingWindow.cpp
    private/FloatingWindow_p.h
    private/Logging.cpp
    private/Logging_p.h
    private/TabWidget.cpp
    private/TabWidget_p.h
    private/TitleBar.cpp
    private/TitleBar_p.h
    private/SideBar.cpp
    private/SideBar_p.h
    private/DockRegistry.cpp
    private/DockRegistry_p.h
    private/Draggable.cpp
    private/Draggable_p.h
    private/WindowBeingDragged.cpp
    private/WindowBeingDragged_p.h
    private/DragController.cpp
    private/DragController_p.h
    private/Frame.cpp
    private/Frame_p.h
    private/DropAreaWithCentralFrame.cpp
    private/DropAreaWithCentralFrame_p.h
    private/WidgetResizeHandler.cpp
    private/WidgetResizeHandler_p.h
    private/indicators/NullIndicators.cpp
    private/indicators/NullIndicators_p.h
    private/indicators/ClassicIndicators.cpp
    private/indicators/ClassicIndicators_p.h
    private/indicators/ClassicIndicatorsWindow.cpp
    private/indicators/ClassicIndicatorsWindow_p.h

    private/multisplitter/Item.cpp
    private/multisplitter/Item_p.h
    private/multisplitter/ItemFreeContainer.cpp
    private/multisplitter/ItemFreeContainer_p.h
    private/multisplitter/Logging.cpp
    private/multisplitter/Logging_p.h
    private/multisplitter/MultiSplitterConfig.cpp
    private/multisplitter/MultiSplitterConfig.h
    private/multisplitter/Separator.cpp
    private/multisplitter/Separator_p.h
    private/multisplitter/Widget.cpp
    private/multisplitter/Widget.h
)

set(DOCKS_INSTALLABLE_INCLUDES
    docks_export.h
    Config.h
    FrameworkWidgetFactory.h
    DockWidgetBase.h
    KDDockWidgets.h
    Qt5Qt6Compat_p.h
    FocusScope.h
    QWidgetAdapter.h
    LayoutSaver.h
    MainWindowMDI.h
    MainWindowBase.h
)

set(DOCKS_INSTALLABLE_PRIVATE_INCLUDES
    private/DragController_p.h
    private/Draggable_p.h
    private/DropArea_p.h
    private/DropIndicatorOverlayInterface_p.h
    private/FloatingWindow_p.h
    private/Frame_p.h
    private/LayoutSaver_p.h
    private/MultiSplitter_p.h
    private/LayoutWidget_p.h
    private/SideBar_p.h
    private/TitleBar_p.h
    private/WindowBeingDragged_p.h
    private/WidgetResizeHandler_p.h
    private/DockRegistry_p.h
    private/TabWidget_p.h
)

set(DOCKS_INSTALLABLE_PRIVATE_WIDGET_INCLUDES
    private/widgets/QWidgetAdapter_widgets_p.h
    private/widgets/TitleBarWidget_p.h
    private/widgets/SideBarWidget_p.h
    private/widgets/FloatingWindowWidget_p.h
    private/widgets/FrameWidget_p.h
    private/widgets/TabBarWidget_p.h
    private/widgets/TabWidgetWidget_p.h
)

set(DOCKS_INSTALLABLE_PRIVATE_QUICK_INCLUDES
    private/quick/QWidgetAdapter_quick_p.h
)

if(${PROJECT_NAME}_QTQUICK)
  set(DOCKSLIBS_SRCS ${DOCKSLIBS_SRCS}
      DockWidgetQuick.cpp
      DockWidgetQuick.h
      private/quick/DockWidgetInstantiator.cpp
      private/quick/DockWidgetInstantiator_p.h
      private/quick/QWidgetAdapter_quick.cpp
      private/quick/QWidgetAdapter_quick_p.h
      private/quick/FloatingWindowQuick.cpp
      private/quick/FloatingWindowQuick_p.h
      private/quick/TabWidgetQuick.cpp
      private/quick/TabWidgetQuick_p.h
      private/quick/TabBarQuick.cpp
      private/quick/TabBarQuick_p.h
      private/quick/TitleBarQuick.cpp
      private/quick/TitleBarQuick_p.h
      private/quick/QmlTypes.cpp
      private/quick/QmlTypes.h
      private/quick/Helpers.cpp
      private/quick/Helpers_p.h
      private/quick/FrameQuick.cpp
      private/quick/FrameQuick_p.h
      private/quick/LayoutSaverInstantiator.cpp
      private/quick/LayoutSaverInstantiator_p.h
      private/quick/RubberBandQuick.cpp
      private/quick/RubberBandQuick.h
      private/quick/MainWindowQuick.cpp
      private/quick/MainWindowQuick_p.h
      private/quick/MainWindowInstantiator.cpp
      private/quick/MainWindowInstantiator_p.h
      private/multisplitter/Widget_quick.cpp
      private/multisplitter/Widget_quick.h
      private/multisplitter/Separator_quick.cpp
      private/multisplitter/Separator_quick.h
      private/multisplitter/Rubberband_quick.cpp
      private/multisplitter/Rubberband_quick.h
      kddockwidgets_qtquick.qrc)

  set(DOCKS_INSTALLABLE_INCLUDES ${DOCKS_INSTALLABLE_INCLUDES} DockWidgetQuick.h)

else()
  set(DOCKSLIBS_SRCS ${DOCKSLIBS_SRCS}
      private/DebugWindow.cpp
      private/DebugWindow_p.h
      private/ObjectViewer.cpp
      private/ObjectViewer_p.h
      MainWindow.cpp
      MainWindow.h
      DockWidget.h
      private/multisplitter/Widget_qwidget.cpp
      private/multisplitter/Widget_qwidget.h
      private/multisplitter/Separator_qwidget.cpp
      private/multisplitter/Separator_qwidget.h
      private/widgets/TabBarWidget.cpp
      private/widgets/TabBarWidget_p.h
      private/widgets/FloatingWindowWidget.cpp
      private/widgets/FloatingWindowWidget_p.h
      private/widgets/FrameWidget.cpp
      private/widgets/FrameWidget_p.h
      private/widgets/SideBarWidget.cpp
      private/widgets/SideBarWidget_p.h
      private/widgets/TabWidgetWidget.cpp
      private/widgets/TabWidgetWidget_p.h
      private/widgets/TitleBarWidget.cpp
      private/widgets/TitleBarWidget_p.h
      private/widgets/DockWidget.cpp
      private/widgets/QWidgetAdapter_widgets.cpp
      private/widgets/QWidgetAdapter_widgets_p.h
      private/indicators/SegmentedIndicators.cpp
      private/indicators/SegmentedIndicators_p.h
      # private/indicators/AnimatedIndicators.cpp
  )

  set(DOCKS_INSTALLABLE_INCLUDES
      ${DOCKS_INSTALLABLE_INCLUDES}
      MainWindow.h
      DockWidget.h
  )
endif()

#Generate C/C++ CamelCase forwarding headers (only public includes)
#include(ECMGenerateHeaders)
#ecm_generate_headers(camelcase_HEADERS
#  ORIGINAL
#    CAMELCASE
#  HEADER_NAMES
#    Config
#    DockWidget
#    DockWidgetBase
#    FocusScope
#    FrameworkWidgetFactory,DefaultWidgetFactory
#    LayoutSaver
#    MainWindow
#    MainWindowBase
#    MainWindowMDI
#)

set(RESOURCES ${CMAKE_CURRENT_SOURCE_DIR}/kddockwidgets_resources.qrc)

add_library(kddockwidgets ${KDDockWidgets_LIBRARY_MODE} ${DOCKSLIBS_SRCS} ${DOCKS_INSTALLABLE_INCLUDES} ${RESOURCES})
add_library(KDAB::kddockwidgets ALIAS kddockwidgets)
set_target_properties(kddockwidgets PROPERTIES OUTPUT_NAME "kddockwidgets${KDDockWidgets_LIBRARY_QTID}")
set_compiler_flags(kddockwidgets)

if(${PROJECT_NAME}_QT6)
  set(DOCKS_INCLUDES_INSTALL_PATH "include/kddockwidgets-qt6")
else()
  set(DOCKS_INCLUDES_INSTALL_PATH "include/")
endif()


target_include_directories(kddockwidgets
  PUBLIC
    $<INSTALL_INTERFACE:${DOCKS_INCLUDES_INSTALL_PATH}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/fwd_headers>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
)

target_compile_definitions(kddockwidgets
  PRIVATE
  QT_NO_CAST_TO_ASCII
  QT_NO_CAST_FROM_ASCII
  QT_NO_URL_CAST_FROM_STRING
  QT_NO_CAST_FROM_BYTEARRAY
)
if(${PROJECT_NAME}_STATIC)
  target_compile_definitions(kddockwidgets PUBLIC KDDOCKWIDGETS_STATICLIB)
else()
  target_compile_definitions(kddockwidgets PRIVATE BUILDING_DOCKS_LIBRARY)
endif()

if(KDDockWidgets_QTQUICK)
  target_compile_definitions(kddockwidgets PUBLIC KDDOCKWIDGETS_QTQUICK)
endif()

if(CMAKE_COMPILER_IS_GNUCXX OR IS_CLANG_BUILD)
  target_compile_options(kddockwidgets PRIVATE -Wshadow)
  if(NOT MSVC)
    target_compile_options(kddockwidgets PRIVATE -fvisibility=hidden)
  endif()

  # Disable -Wconversion for Qt6. The qsizetype to int conversions are harmless
  if(NOT ${PROJECT_NAME}_QT6)
    target_compile_options(kddockwidgets PRIVATE -Wconversion)
  endif()

  if(IS_CLANG_BUILD)
    target_compile_options(kddockwidgets PRIVATE -Wweak-vtables)
  endif()
endif()

if(${PROJECT_NAME}_QTQUICK)
  target_link_libraries(kddockwidgets PUBLIC Qt${Qt_VERSION_MAJOR}::Widgets Qt${Qt_VERSION_MAJOR}::Quick Qt${Qt_VERSION_MAJOR}::QuickControls2 PRIVATE Qt${Qt_VERSION_MAJOR}::GuiPrivate)
else()
  target_link_libraries(kddockwidgets PUBLIC Qt${Qt_VERSION_MAJOR}::Widgets PRIVATE Qt${Qt_VERSION_MAJOR}::WidgetsPrivate)
endif()

if(WIN32)
  target_link_libraries(kddockwidgets PRIVATE Qt${Qt_VERSION_MAJOR}::GuiPrivate dwmapi)
elseif(NOT APPLE AND NOT EMSCRIPTEN AND NOT ${PROJECT_NAME}_QT6 AND ${PROJECT_NAME}_X11EXTRAS)
  find_package(Qt${Qt_VERSION_MAJOR}X11Extras)
  target_link_libraries(kddockwidgets PUBLIC Qt${Qt_VERSION_MAJOR}::X11Extras)
endif()

set_target_properties(kddockwidgets PROPERTIES
  SOVERSION ${${PROJECT_NAME}_SOVERSION}
  VERSION ${${PROJECT_NAME}_VERSION}
)

#version libraries on Windows
if(WIN32)
  if(CMAKE_BUILD_TYPE)
    set(postfix ${${PROJECT_NAME}_VERSION_MAJOR})
    string(TOUPPER ${CMAKE_BUILD_TYPE} UPPER_BUILD_TYPE)
    if(${UPPER_BUILD_TYPE} MATCHES "^DEBUG")
      string(CONCAT postfix ${postfix} "d")
      set_target_properties(kddockwidgets PROPERTIES DEBUG_POSTFIX ${postfix})
    else()
      set_target_properties(kddockwidgets PROPERTIES ${UPPER_BUILD_TYPE}_POSTFIX ${postfix})
    endif()
  elseif(CMAKE_CONFIGURATION_TYPES)
    # Visual Studio generator
    set_target_properties(kddockwidgets PROPERTIES DEBUG_POSTFIX d)
  endif()
endif()

install(TARGETS kddockwidgets
        EXPORT kddockwidgetsTargets
        RUNTIME DESTINATION ${INSTALL_RUNTIME_DIR}
        LIBRARY DESTINATION ${INSTALL_LIBRARY_DIR}
        ARCHIVE DESTINATION ${INSTALL_ARCHIVE_DIR}
)
if(MSVC AND NOT ${PROJECT_NAME}_STATIC)
  install(FILES "$<TARGET_PDB_FILE_DIR:kddockwidgets>/$<TARGET_PDB_FILE_NAME:kddockwidgets>" DESTINATION ${INSTALL_LIBRARY_DIR} CONFIGURATIONS Debug RelWithDebInfo)
endif()

install(FILES ${camelcase_HEADERS} DESTINATION ${DOCKS_INCLUDES_INSTALL_PATH}/kddockwidgets)
install(FILES ${DOCKS_INSTALLABLE_INCLUDES} DESTINATION ${DOCKS_INCLUDES_INSTALL_PATH}/kddockwidgets)
install(FILES ${DOCKS_INSTALLABLE_PRIVATE_INCLUDES} DESTINATION ${DOCKS_INCLUDES_INSTALL_PATH}/kddockwidgets/private)
install(FILES private/multisplitter/Item_p.h DESTINATION ${DOCKS_INCLUDES_INSTALL_PATH}/kddockwidgets/private/multisplitter)
install(FILES private/multisplitter/Widget.h DESTINATION ${DOCKS_INCLUDES_INSTALL_PATH}/kddockwidgets/private/multisplitter)
install(FILES private/multisplitter/Separator_p.h DESTINATION ${DOCKS_INCLUDES_INSTALL_PATH}/kddockwidgets/private/multisplitter)
install(FILES private/indicators/ClassicIndicators_p.h DESTINATION ${DOCKS_INCLUDES_INSTALL_PATH}/kddockwidgets/private/indicators)
install(FILES private/indicators/SegmentedIndicators_p.h DESTINATION ${DOCKS_INCLUDES_INSTALL_PATH}/kddockwidgets/private/indicators)

if(KDDockWidgets_QTQUICK)
  install(FILES ${DOCKS_INSTALLABLE_PRIVATE_QUICK_INCLUDES} DESTINATION ${DOCKS_INCLUDES_INSTALL_PATH}/kddockwidgets/private/quick)
  install(FILES private/multisplitter/Separator_quick.h DESTINATION ${DOCKS_INCLUDES_INSTALL_PATH}/kddockwidgets/private/multisplitter)
  install(FILES private/multisplitter/Widget_quick.h DESTINATION ${DOCKS_INCLUDES_INSTALL_PATH}/kddockwidgets/private/multisplitter)
else()
  install(FILES ${DOCKS_INSTALLABLE_PRIVATE_WIDGET_INCLUDES} DESTINATION ${DOCKS_INCLUDES_INSTALL_PATH}/kddockwidgets/private/widgets)
  install(FILES private/multisplitter/Separator_qwidget.h DESTINATION ${DOCKS_INCLUDES_INSTALL_PATH}/kddockwidgets/private/multisplitter)
  install(FILES private/multisplitter/Widget_qwidget.h DESTINATION ${DOCKS_INCLUDES_INSTALL_PATH}/kddockwidgets/private/multisplitter)
endif()

# Generate library version files
include(ECMSetupVersion)
ecm_setup_version(
  ${${PROJECT_NAME}_VERSION}
  VARIABLE_PREFIX KDDOCKWIDGETS
  VERSION_HEADER "${CMAKE_CURRENT_BINARY_DIR}/kddockwidgets_version.h"
  PACKAGE_VERSION_FILE "${CMAKE_CURRENT_BINARY_DIR}/KDDockWidgets${KDDockWidgets_LIBRARY_QTID}ConfigVersion.cmake"
  SOVERSION ${${PROJECT_NAME}_SOVERSION}
  COMPATIBILITY AnyNewerVersion
)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/kddockwidgets_version.h" DESTINATION ${DOCKS_INCLUDES_INSTALL_PATH}/kddockwidgets)

install(EXPORT kddockwidgetsTargets
        FILE KDDockWidgets${KDDockWidgets_LIBRARY_QTID}Targets.cmake
        NAMESPACE KDAB::
        DESTINATION ${INSTALL_LIBRARY_DIR}/cmake/KDDockWidgets${KDDockWidgets_LIBRARY_QTID}
)
configure_file(KDDockWidgetsConfig.cmake.in KDDockWidgets${KDDockWidgets_LIBRARY_QTID}Config.cmake @ONLY)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/KDDockWidgets${KDDockWidgets_LIBRARY_QTID}Config.cmake"
              "${CMAKE_CURRENT_BINARY_DIR}/KDDockWidgets${KDDockWidgets_LIBRARY_QTID}ConfigVersion.cmake"
        DESTINATION ${INSTALL_LIBRARY_DIR}/cmake/KDDockWidgets${KDDockWidgets_LIBRARY_QTID}
)

if(${PROJECT_NAME}_DEVELOPER_MODE)
  # Under developer mode since kddw might be a sub-folder of a project setting a different value for QT_DISABLE_DEPRECATED_BEFORE
  target_compile_definitions(kddockwidgets PRIVATE QT_DISABLE_DEPRECATED_BEFORE=0x060000)

  option(KDDockWidgets_LINTER "Build the layout linter" ON)

  if(NOT ${PROJECT_NAME}_QTQUICK AND KDDockWidgets_LINTER) # TODO: We can support it
    add_executable(kddockwidgets_linter layoutlinter_main.cpp)
    target_link_libraries(kddockwidgets_linter kddockwidgets Qt${Qt_VERSION_MAJOR}::Widgets)
  endif()

endif()
