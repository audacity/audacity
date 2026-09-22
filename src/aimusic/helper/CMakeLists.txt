set(VAMPSDK_ENABLE_INSTALL OFF CACHE BOOL "" FORCE)
add_subdirectory(
    ${CMAKE_CURRENT_LIST_DIR}/../../../thirdparty/vamp-plugin-sdk
    ${CMAKE_BINARY_DIR}/thirdparty/vamp-plugin-sdk
    EXCLUDE_FROM_ALL)

set(QM_VAMP_ROOT ${CMAKE_CURRENT_LIST_DIR}/../../../thirdparty/qm-vamp-plugins)
set(QM_VAMP_SOLUTION ${QM_VAMP_ROOT}/build/msvc/QMVampPlugins.sln)
set(QM_VAMP_DLL ${QM_VAMP_ROOT}/build/msvc/x64/Release/qm-vamp-plugins.dll)
set(NNLS_CHROMA_ROOT ${CMAKE_CURRENT_LIST_DIR}/../../../thirdparty/nnls-chroma)
set(NNLS_CHROMA_DLL ${NNLS_CHROMA_ROOT}/nnls-chroma.dll)

if (OS_IS_WIN)
    if (NOT EXISTS ${NNLS_CHROMA_DLL})
        message(FATAL_ERROR "Bundled NNLS Chroma / Chordino DLL is missing.")
    endif()

    # Locate MSBuild instead of assuming one Visual Studio edition and install
    # path. Any VS 2022 edition or the Build Tools is acceptable.
    set(QM_VAMP_PLATFORM_TOOLSET "v143" CACHE STRING "MSBuild platform toolset for the bundled QM Vamp plugins")
    set(QM_VAMP_SDK_VERSION "10.0" CACHE STRING "Windows SDK version (10.0 selects the latest installed)")

    find_program(QM_VAMP_MSBUILD
        NAMES MSBuild.exe
        HINTS
            "$ENV{ProgramFiles}/Microsoft Visual Studio/2022/Community/MSBuild/Current/Bin/amd64"
            "$ENV{ProgramFiles}/Microsoft Visual Studio/2022/Professional/MSBuild/Current/Bin/amd64"
            "$ENV{ProgramFiles}/Microsoft Visual Studio/2022/Enterprise/MSBuild/Current/Bin/amd64"
            "$ENV{ProgramFiles}/Microsoft Visual Studio/2022/BuildTools/MSBuild/Current/Bin/amd64"
            "$ENV{ProgramFiles(x86)}/Microsoft Visual Studio/2022/BuildTools/MSBuild/Current/Bin/amd64"
    )
    if (NOT QM_VAMP_MSBUILD)
        # vswhere is the supported way to find any installed MSBuild regardless
        # of edition, install location, or preview channel.
        set(_vswhere "$ENV{ProgramFiles(x86)}/Microsoft Visual Studio/Installer/vswhere.exe")
        if (EXISTS "${_vswhere}")
            execute_process(
                COMMAND "${_vswhere}" -latest -products * -requires Microsoft.Component.MSBuild
                        -find "MSBuild/**/Bin/**/MSBuild.exe"
                OUTPUT_VARIABLE _vswhere_msbuild
                OUTPUT_STRIP_TRAILING_WHITESPACE)
            if (_vswhere_msbuild)
                set(QM_VAMP_MSBUILD "${_vswhere_msbuild}")
            endif()
        endif()
    endif()
    if (NOT QM_VAMP_MSBUILD OR NOT EXISTS "${QM_VAMP_MSBUILD}")
        message(FATAL_ERROR
            "QM Vamp Plugins need MSBuild from Visual Studio 2022 or the Build Tools "
            "(Desktop C++ workload). Install it, or configure with "
            "-DQM_VAMP_MSBUILD=<path to MSBuild.exe>.")
    endif()

    add_custom_command(
        OUTPUT ${QM_VAMP_DLL}
        COMMAND "${QM_VAMP_MSBUILD}" ${QM_VAMP_SOLUTION} /t:Build /p:Configuration=Release /p:Platform=x64
                /p:WindowsTargetPlatformVersion=${QM_VAMP_SDK_VERSION}
                /p:PlatformToolset=${QM_VAMP_PLATFORM_TOOLSET} /m
        WORKING_DIRECTORY ${QM_VAMP_ROOT}
        DEPENDS ${QM_VAMP_SOLUTION}
        COMMENT "Building bundled QM Vamp tempo and key analyzer")
    add_custom_target(qm_vamp_plugin DEPENDS ${QM_VAMP_DLL})
endif()

add_executable(ai_music_analysis_helper main.cpp)

target_link_libraries(ai_music_analysis_helper PRIVATE Qt6::Core vamp-hostsdk)

if (OS_IS_WIN)
    add_dependencies(ai_music_analysis_helper qm_vamp_plugin)
    set_target_properties(ai_music_analysis_helper PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_INSTALL_PREFIX}/bin)
    add_custom_command(TARGET ai_music_analysis_helper POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E make_directory $<TARGET_FILE_DIR:ai_music_analysis_helper>/vamp
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${QM_VAMP_DLL} $<TARGET_FILE_DIR:ai_music_analysis_helper>/vamp/qm-vamp-plugins.dll
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${NNLS_CHROMA_DLL} $<TARGET_FILE_DIR:ai_music_analysis_helper>/vamp/nnls-chroma.dll)
endif()

install(TARGETS ai_music_analysis_helper RUNTIME DESTINATION bin)
if (OS_IS_WIN)
    install(FILES ${QM_VAMP_DLL} DESTINATION bin/vamp)
    install(FILES ${QM_VAMP_ROOT}/COPYING DESTINATION licenses RENAME QM-Vamp-Plugins-GPL-2.0.txt)
    install(FILES ${NNLS_CHROMA_DLL} DESTINATION bin/vamp)
    install(FILES ${NNLS_CHROMA_ROOT}/COPYING DESTINATION licenses RENAME NNLS-Chroma-Chordino-GPL-2.0-or-later.txt)
    install(FILES ${NNLS_CHROMA_ROOT}/CITATION.txt DESTINATION licenses RENAME NNLS-Chroma-Chordino-CITATION.txt)
endif()
