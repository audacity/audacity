muse_create_qml_module(appshell_qml FOR appshell)

qt_add_qml_module(appshell_qml
    URI Audacity.AppShell
    VERSION 1.0
    SOURCES
        aboutmodel.cpp
        aboutmodel.h
        appmenumodel.cpp
        appmenumodel.h
        DevTools/Settings/settingslistmodel.cpp
        DevTools/Settings/settingslistmodel.h
        FirstLaunchSetup/
        FirstLaunchSetup/clipvisualizationpagemodel.cpp
        FirstLaunchSetup/clipvisualizationpagemodel.h
        FirstLaunchSetup/firstlaunchsetupmodel.cpp
        FirstLaunchSetup/firstlaunchsetupmodel.h
        FirstLaunchSetup/themespagemodel.cpp
        FirstLaunchSetup/themespagemodel.h
        FirstLaunchSetup/workspacelayoutpagemodel.cpp
        FirstLaunchSetup/workspacelayoutpagemodel.h
        FirstLaunchSetup/signinaudiocompagemodel.cpp
        FirstLaunchSetup/signinaudiocompagemodel.h
        framelesswindowmodel.cpp
        framelesswindowmodel.h
        maintoolbarmodel.cpp
        maintoolbarmodel.h
        mainwindowtitleprovider.cpp
        mainwindowtitleprovider.h
        projectpagemodel.cpp
        projectpagemodel.h
        PublishPage/publishtoolbarmodel.cpp
        PublishPage/publishtoolbarmodel.h
        windowdroparea.cpp
        windowdroparea.h
    QML_FILES
        AboutDialog.qml
        AlphaWelcomePopup.qml
        AppWindow.qml
        DevTools/CrashHandler/CrashHandlerDevTools.qml
        DevTools/DevToolsMenu.qml
        DevTools/DevToolsPage.qml
        DevTools/Extensions/ExtensionsListView.qml
        DevTools/Gallery/GeneralComponentsGallery.qml
        DevTools/Interactive/InteractiveTests.qml
        DevTools/Interactive/SampleDialog.qml
        DevTools/KeyNav/KeyNavExample.qml
        DevTools/KeyNav/KeyNavSection.qml
        DevTools/KeyNav/KeyNavSubSection.qml
        DevTools/Preferences/SettingsPage.qml
        DevTools/ToastNotification/ToastNotificationTests.qml
        DevTools/Cloud/CloudTests.qml
        FirstLaunchSetup/ClipVisualizationPage.qml
        FirstLaunchSetup/DoublePage.qml
        FirstLaunchSetup/FirstLaunchSetupDialog.qml
        FirstLaunchSetup/Page.qml
        FirstLaunchSetup/PlaybackPage.qml
        FirstLaunchSetup/ThemesPage.qml
        FirstLaunchSetup/TutorialsPage.qml
        FirstLaunchSetup/UnderConstructionPage.qml
        FirstLaunchSetup/WorkspaceLayoutPage.qml
        FirstLaunchSetup/SigninAudiocomPage.qml
        FirstLaunchSetup/AppUpdatesAndUsageInfoPage.qml
        HomePage/HomeMenu.qml
        HomePage/HomePage.qml
        HomePage/PluginsPage.qml
        Main.qml
        Main.wasm.qml
        MainToolBar.qml
        platform/AppButtonBackground.qml
        platform/AppMenuBar.qml
        platform/PlatformMenuBar.qml
        ProjectPage/ProjectPage.qml
        ProjectPage/ProjectStatusBar.qml
        PublishPage/PublishPage.qml
        PublishPage/PublishToolBar.qml
        shared/AccentColorsList.qml
        shared/ThemeSamplesList.qml
        shared/internal/ThemeSample.qml
        WindowContent.qml
    RESOURCES
        FirstLaunchSetup/resources/ClipVisuals_ClassicClips.png
        FirstLaunchSetup/resources/ClipVisuals_ColorfulClips.png
        FirstLaunchSetup/resources/MuseSounds.png
        FirstLaunchSetup/resources/UnderConstruction.png
        FirstLaunchSetup/resources/VideoTutorials.png
    IMPORTS
        TARGET muse_ui_qml
        TARGET muse_uicomponents_qml
        TARGET muse_cloud_qml
        TARGET muse_dockwindow_qml
        TARGET muse_diagnostics_qml
        TARGET muse_extensions_qml
        TARGET muse_learn_qml
        TARGET muse_shortcuts_qml
        TARGET muse_tours_qml
)

if (OS_IS_MAC)
    qt_target_qml_sources(appshell_qml
        QML_FILES
            platform/mac/Main.qml
    )
elseif(OS_IS_WIN)
    qt_target_qml_sources(appshell_qml
        QML_FILES
            platform/win/AppSystemButtons.qml
            platform/win/AppTitleBar.qml
            platform/win/Main.qml
    )
elseif(OS_IS_LIN OR OS_IS_FREEBSD)
    qt_target_qml_sources(appshell_qml
        QML_FILES
            platform/linux/Main.qml
    )
else()
    message(FATAL_ERROR "Unsupported OS")
endif()

if (NOT OS_IS_MAC)
    target_sources(appshell_qml PRIVATE
        navigableappmenumodel.cpp
        navigableappmenumodel.h
    )

    # we use QKeyMapper to fix https://github.com/musescore/MuseScore/issues/10181
    find_package(Qt6 REQUIRED COMPONENTS GuiPrivate)
    target_link_libraries(appshell_qml PRIVATE Qt6::GuiPrivate)
endif(NOT OS_IS_MAC)

fixup_qml_module_dependencies(appshell_qml)

# Necessary for the auto-generated sources
target_include_directories(appshell_qml PRIVATE
    DevTools/Settings
    FirstLaunchSetup
    PublishPage
)
