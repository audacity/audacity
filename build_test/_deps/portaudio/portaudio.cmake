
function(portaudio_Populate remote_url local_path os arch build_type)

    if (os STREQUAL "linux")

        set(compiler "gcc10")

        # At the moment only relwithdebinfo
        # I don't think we need debug builds
        set(name "linux_${arch}_relwithdebinfo_${compiler}")

        if (NOT EXISTS ${local_path}/${name}.7z)
            message(STATUS "[portaudio] Populate: ${remote_url}/${name} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(portaudio_INCLUDE_DIRS ${local_path}/include)
        set(portaudio_LIBRARIES ${local_path}/lib/libportaudio.so)
        set(portaudio_INSTALL_LIBRARIES ${portaudio_LIBRARIES})


    elseif(os STREQUAL "macos")

        # At the moment only relwithdebinfo
        # I don't think we need debug builds
        if (arch STREQUAL "x86_64")
            set(name "macos_x86_64_relwithdebinfo_appleclang15_os109")
        elseif (arch STREQUAL "aarch64")
            set(name "macos_aarch64_relwithdebinfo_appleclang15_os1013")
        elseif (arch STREQUAL "universal")
            set(name "macos_universal_relwithdebinfo_appleclang15_os1013")
        else()
            message(FATAL_ERROR "Not supported macos arch: ${arch}")
        endif()

        if (NOT EXISTS ${local_path}/${name}.7z)
            message(STATUS "[portaudio] Populate: ${remote_url} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(portaudio_INCLUDE_DIRS ${local_path}/include)
        set(portaudio_LIBRARIES ${local_path}/lib/libportaudio.dylib)
        set(portaudio_INSTALL_LIBRARIES ${portaudio_LIBRARIES})

    elseif(os STREQUAL "windows")

        set(compiler "msvc192")

        if (build_type STREQUAL "release")
            set(build_type "relwithdebinfo")
        endif()

        set(name "windows_${arch}_${build_type}_${compiler}")

        if (NOT EXISTS ${local_path}/${name}.7z)
            message(STATUS "[portaudio] Populate: ${remote_url} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(portaudio_INCLUDE_DIRS ${local_path}/include)
        set(portaudio_LIBRARIES ${local_path}/lib/portaudio_x64.lib)
        set(portaudio_INSTALL_LIBRARIES ${local_path}/bin/portaudio_x64.dll)

    else()
        message(FATAL_ERROR "[portaudio] Not supported os: ${os}")
    endif()

    if(NOT TARGET portaudio::portaudio)
       add_library(portaudio::portaudio INTERFACE IMPORTED GLOBAL)

       target_include_directories(portaudio::portaudio INTERFACE ${portaudio_INCLUDE_DIRS} )
       target_link_libraries(portaudio::portaudio INTERFACE ${portaudio_LIBRARIES} )
    endif()

    set_property(GLOBAL PROPERTY portaudio_INCLUDE_DIRS ${portaudio_INCLUDE_DIRS})
    set_property(GLOBAL PROPERTY portaudio_LIBRARIES ${portaudio_LIBRARIES})
    set_property(GLOBAL PROPERTY portaudio_INSTALL_LIBRARIES ${portaudio_INSTALL_LIBRARIES})

endfunction()

