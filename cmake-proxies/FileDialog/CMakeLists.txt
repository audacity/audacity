#directory lib-src/FileDialog
set(TARGET FileDialog)
set(TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET})
project(${TARGET})

add_library(${TARGET} STATIC ${LIB_SRC_DIRECTORY}FileDialog/FileDialog.cpp)
target_include_directories(${TARGET} PRIVATE ${TARGET_SOURCE})
set_target_properties(${TARGET} PROPERTIES
    CXX_STANDARD 11
    OSX_ARCHITECTURES "")

find_package(wxWidgets REQUIRED COMPONENTS net core base)
include(${wxWidgets_USE_FILE})
target_compile_definitions(${TARGET} PRIVATE ${wxWidgets_DEFINITIONS})
target_compile_options(${TARGET} PRIVATE ${wxWidgets_CXX_FLAGS})
target_link_libraries(${TARGET} PRIVATE ${wxWidgets_LIBRARIES})

if(WIN32)
    target_sources(${TARGET} PRIVATE ${LIB_SRC_DIRECTORY}FileDialog/win/FileDialogPrivate.cpp)
    target_compile_definitions(${TARGET} PRIVATE __WIN32__)
    target_include_directories(${TARGET} PRIVATE ${TARGET_SOURCE}/win)
elseif(APPLE)
    target_sources(${TARGET} PRIVATE ${LIB_SRC_DIRECTORY}FileDialog/mac/FileDialogPrivate.mm)
    target_compile_options(${TARGET} PRIVATE -Wno-deprecated-declarations)
    target_include_directories(${TARGET} PRIVATE ${TARGET_SOURCE}/mac)
else()
    target_sources(${TARGET} PRIVATE ${LIB_SRC_DIRECTORY}FileDialog/gtk/FileDialogPrivate.cpp)

    find_program(wxWidgets_CONFIG_EXECUTABLE
        NAMES wx-config wx-config-3.1 wx-config-3.0 wx-config-2.9 wx-config-2.8
        ONLY_CMAKE_FIND_ROOT_PATH)
    execute_process(
        COMMAND sh "${wxWidgets_CONFIG_EXECUTABLE}" --query-toolkit
        OUTPUT_VARIABLE WXGTK
        RESULT_VARIABLE RET
        ERROR_QUIET)
    string(STRIP "${WXGTK}" WXGTK)
    if(RET EQUAL 0 AND WXGTK STREQUAL "gtk3")
        set(GTK_PACKAGE gtk+-3.0)
    else()
        set(GTK_PACKAGE gtk+-2.0)
    endif()

    find_package(PkgConfig REQUIRED)
    pkg_check_modules(GTK REQUIRED ${GTK_PACKAGE})
    target_compile_options(${TARGET} PRIVATE -Wno-deprecated-declarations ${GTK_CFLAGS})
    target_include_directories(${TARGET} PRIVATE ${TARGET_SOURCE}/gtk ${GTK_INCLUDE_DIRS})
    target_link_libraries(${TARGET} PUBLIC ${GTK_LIBRARIES})
endif()
