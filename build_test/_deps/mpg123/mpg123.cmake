function(mpg123_Populate remote_url local_path os arch build_type)

    if (os STREQUAL "linux")

        set(compiler "gcc10")

        # At the moment only relwithdebinfo
        # I don't think we need debug builds
        set(name "linux_${arch}_relwithdebinfo_${compiler}")

        if (NOT EXISTS ${local_path}/${name}.7z)
            message(STATUS "[mpg123] Populate: ${remote_url}/${name} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(mpg123_INCLUDE_DIRS ${local_path}/include)
        set(mpg123_LIBRARIES
            ${local_path}/lib/libmpg123.so.0.47.0
            ${local_path}/lib/libmpg123.so.0
            ${local_path}/lib/libmpg123.so
        )
        set(mpg123_INSTALL_LIBRARIES ${mpg123_LIBRARIES})

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
            message(STATUS "[mpg123] Populate: ${remote_url} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(mpg123_INCLUDE_DIRS ${local_path}/include)
        set(mpg123_LIBRARIES
            ${local_path}/lib/libmpg123.0.dylib
            ${local_path}/lib/libmpg123.dylib
        )
        set(mpg123_INSTALL_LIBRARIES ${mpg123_LIBRARIES})

    elseif(os STREQUAL "windows")

        set(compiler "msvc192")

        if (build_type STREQUAL "release")
            set(build_type "relwithdebinfo")
        endif()

        set(name "windows_${arch}_${build_type}_${compiler}")

        if (NOT EXISTS ${local_path}/${name}.7z)
            message(STATUS "[mpg123] Populate: ${remote_url} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(mpg123_INCLUDE_DIRS ${local_path}/include)
        set(mpg123_LIBRARIES ${local_path}/lib/mpg123.lib)
        set(mpg123_INSTALL_LIBRARIES ${local_path}/bin/mpg123.dll)

    else()
        message(FATAL_ERROR "[mpg123] Not supported os: ${os}")
    endif()

    if(NOT TARGET mpg123::libmpg123)
       add_library(mpg123::libmpg123 INTERFACE IMPORTED GLOBAL)

       target_include_directories(mpg123::libmpg123 INTERFACE ${mpg123_INCLUDE_DIRS} )
       target_link_libraries(mpg123::libmpg123 INTERFACE ${mpg123_LIBRARIES} )
    endif()

    set_property(GLOBAL PROPERTY mpg123_INCLUDE_DIRS ${mpg123_INCLUDE_DIRS})
    set_property(GLOBAL PROPERTY mpg123_LIBRARIES ${mpg123_LIBRARIES})
    set_property(GLOBAL PROPERTY mpg123_INSTALL_LIBRARIES ${mpg123_INSTALL_LIBRARIES})

endfunction()
