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
    set(QM_VAMP_MSBUILD "C:/Program Files/Microsoft Visual Studio/2022/Community/MSBuild/Current/Bin/amd64/MSBuild.exe")
    if (NOT EXISTS ${QM_VAMP_MSBUILD})
        message(FATAL_ERROR "QM Vamp Plugins requires Visual Studio 2022 MSBuild on Windows.")
    endif()

    add_custom_command(
        OUTPUT ${QM_VAMP_DLL}
        COMMAND ${QM_VAMP_MSBUILD} ${QM_VAMP_SOLUTION} /t:Build /p:Configuration=Release /p:Platform=x64 /p:WindowsTargetPlatformVersion=10.0.26100.0 /p:PlatformToolset=v143 /m
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
