function(wavpack_Populate remote_url local_path os arch build_type)

    if (os STREQUAL "linux")

        set(compiler "gcc10")

        # At the moment only relwithdebinfo
        # I don't think we need debug builds
        set(name "linux_${arch}_relwithdebinfo_${compiler}")

        if (NOT EXISTS ${local_path}/${name}.7z)
            message(STATUS "[wavpack] Populate: ${remote_url}/${name} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(wavpack_INCLUDE_DIRS ${local_path}/include)
        set(wavpack_LIBRARIES
            ${local_path}/lib/libwavpack.so.1.2.6
            ${local_path}/lib/libwavpack.so.1
            ${local_path}/lib/libwavpack.so
        )
        set(wavpack_INSTALL_LIBRARIES ${wavpack_LIBRARIES})

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
            message(STATUS "[wavpack] Populate: ${remote_url} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(wavpack_INCLUDE_DIRS ${local_path}/include)
        set(wavpack_LIBRARIES
            ${local_path}/lib/libwavpack.1.dylib
            ${local_path}/lib/libwavpack.dylib
        )
        set(wavpack_INSTALL_LIBRARIES ${wavpack_LIBRARIES})

    elseif(os STREQUAL "windows")

        set(compiler "msvc192")

        if (build_type STREQUAL "release")
            set(build_type "relwithdebinfo")
        endif()

        set(name "windows_${arch}_${build_type}_${compiler}")

        if (NOT EXISTS ${local_path}/${name}.7z)
            message(STATUS "[wavpack] Populate: ${remote_url} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(wavpack_INCLUDE_DIRS ${local_path}/include)
        set(wavpack_LIBRARIES ${local_path}/lib/wavpackdll.lib)
        set(wavpack_INSTALL_LIBRARIES ${local_path}/bin/wavpackdll.dll)

    else()
        message(FATAL_ERROR "[wavpack] Not supported os: ${os}")
    endif()

    if(NOT TARGET wavpack::wavpack)
       add_library(wavpack::wavpack INTERFACE IMPORTED GLOBAL)

       target_include_directories(wavpack::wavpack INTERFACE ${wavpack_INCLUDE_DIRS} )
       target_link_libraries(wavpack::wavpack INTERFACE ${wavpack_LIBRARIES} )
    endif()

    set_property(GLOBAL PROPERTY wavpack_INCLUDE_DIRS ${wavpack_INCLUDE_DIRS})
    set_property(GLOBAL PROPERTY wavpack_LIBRARIES ${wavpack_LIBRARIES})
    set_property(GLOBAL PROPERTY wavpack_INSTALL_LIBRARIES ${wavpack_INSTALL_LIBRARIES})

endfunction()
