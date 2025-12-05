
function(expat_Populate remote_url local_path os arch build_type)

    if (os STREQUAL "linux")

        set(compiler "gcc10")

        # At the moment only relwithdebinfo
        # I don't think we need debug builds
        set(name "linux_${arch}_relwithdebinfo_${compiler}")

        if (NOT EXISTS ${local_path}/${name}.7z)
            message(STATUS "[expat] Populate: ${remote_url}/${name} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(expat_INCLUDE_DIRS ${local_path}/include)
        set(expat_LIBRARIES ${local_path}/lib/libexpat.so.1.8.10)
        set(expat_INSTALL_LIBRARIES ${expat_LIBRARIES})

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
            message(STATUS "[expat] Populate: ${remote_url} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(expat_INCLUDE_DIRS ${local_path}/include)
        set(expat_LIBRARIES ${local_path}/lib/libexpat.1.8.10.dylib)
        set(expat_INSTALL_LIBRARIES ${expat_LIBRARIES})

    elseif(os STREQUAL "windows")

        set(compiler "msvc192")
        set(suffix "")

        if (build_type STREQUAL "release")
            set(build_type "relwithdebinfo")
        endif()

        if (build_type STREQUAL "debug")
            set(suffix "d")
        endif()

        set(name "windows_${arch}_${build_type}_${compiler}")

        if (NOT EXISTS ${local_path}/${name}.7z)
            message(STATUS "[expat] Populate: ${remote_url} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(expat_INCLUDE_DIRS ${local_path}/include)
        set(expat_LIBRARIES ${local_path}/lib/libexpat${suffix}.lib)
        set(expat_INSTALL_LIBRARIES ${local_path}/bin/libexpat${suffix}.dll)

    else()
        message(FATAL_ERROR "[expat] Not supported os: ${os}")
    endif()

    if(NOT TARGET expat::expat)
       add_library(expat::expat INTERFACE IMPORTED GLOBAL)

       target_include_directories(expat::expat INTERFACE ${expat_INCLUDE_DIRS} )
       target_link_libraries(expat::expat INTERFACE ${expat_LIBRARIES} )
    endif()

    set_property(GLOBAL PROPERTY expat_INCLUDE_DIRS ${expat_INCLUDE_DIRS})
    set_property(GLOBAL PROPERTY expat_LIBRARIES ${expat_LIBRARIES})
    set_property(GLOBAL PROPERTY expat_INSTALL_LIBRARIES ${expat_INSTALL_LIBRARIES})

endfunction()

