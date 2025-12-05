function(libmp3lame_Populate remote_url local_path os arch build_type)

    if (os STREQUAL "linux")

        set(compiler "gcc10")

        # At the moment only relwithdebinfo
        # I don't think we need debug builds
        set(name "linux_${arch}_relwithdebinfo_${compiler}")

        if (NOT EXISTS ${local_path}/${name}.7z)
            message(STATUS "[libmp3lame] Populate: ${remote_url}/${name} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(libmp3lame_INCLUDE_DIRS ${local_path}/include)
        set(libmp3lame_LIBRARIES
            ${local_path}/lib/libmp3lame.so.0.0.0
            ${local_path}/lib/libmp3lame.so.0
            ${local_path}/lib/libmp3lame.so
        )
        set(libmp3lame_INSTALL_LIBRARIES ${libmp3lame_LIBRARIES})

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
            message(STATUS "[libmp3lame] Populate: ${remote_url} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(libmp3lame_INCLUDE_DIRS ${local_path}/include)
        set(libmp3lame_LIBRARIES
            ${local_path}/lib/libmp3lame.0.dylib
            ${local_path}/lib/libmp3lame.dylib
        )
        set(libmp3lame_INSTALL_LIBRARIES ${libmp3lame_LIBRARIES})

    elseif(os STREQUAL "windows")

        set(compiler "msvc192")

        if (build_type STREQUAL "release")
            set(build_type "relwithdebinfo")
        endif()

        set(name "windows_${arch}_${build_type}_${compiler}")

        if (NOT EXISTS ${local_path}/${name}.7z)
            message(STATUS "[libmp3lame] Populate: ${remote_url} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(libmp3lame_INCLUDE_DIRS ${local_path}/include)
        set(libmp3lame_LIBRARIES ${local_path}/lib/mp3lame.lib)
        set(libmp3lame_INSTALL_LIBRARIES ${local_path}/bin/libmp3lame.dll)

    else()
        message(FATAL_ERROR "[libmp3lame] Not supported os: ${os}")
    endif()

    if(NOT TARGET libmp3lame::libmp3lame)
       add_library(libmp3lame::libmp3lame INTERFACE IMPORTED GLOBAL)

       target_include_directories(libmp3lame::libmp3lame INTERFACE ${libmp3lame_INCLUDE_DIRS} )
       target_link_libraries(libmp3lame::libmp3lame INTERFACE ${libmp3lame_LIBRARIES} )
    endif()

    set_property(GLOBAL PROPERTY libmp3lame_INCLUDE_DIRS ${libmp3lame_INCLUDE_DIRS})
    set_property(GLOBAL PROPERTY libmp3lame_LIBRARIES ${libmp3lame_LIBRARIES})
    set_property(GLOBAL PROPERTY libmp3lame_INSTALL_LIBRARIES ${libmp3lame_INSTALL_LIBRARIES})

endfunction()
