function(libsndfile_Populate remote_url local_path os arch build_type)

    if (os STREQUAL "linux")

        set(compiler "gcc10")

        # At the moment only relwithdebinfo
        # I don't think we need debug builds
        set(name "linux_${arch}_relwithdebinfo_${compiler}")

        if (NOT EXISTS ${local_path}/${name}.7z)
            message(STATUS "[libsndfile] Populate: ${remote_url}/${name} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(libsndfile_INCLUDE_DIRS ${local_path}/include)
        set(libsndfile_LIBRARIES
            ${local_path}/lib/libsndfile.so.1.0.31
            ${local_path}/lib/libsndfile.so.1
            ${local_path}/lib/libsndfile.so
        )
        set(libsndfile_INSTALL_LIBRARIES ${libsndfile_LIBRARIES})

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
            message(STATUS "[libsndfile] Populate: ${remote_url} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(libsndfile_INCLUDE_DIRS ${local_path}/include)
        set(libsndfile_LIBRARIES
            ${local_path}/lib/libsndfile.1.0.31.dylib
            ${local_path}/lib/libsndfile.1.dylib
            ${local_path}/lib/libsndfile.dylib
        )
        set(libsndfile_INSTALL_LIBRARIES ${libsndfile_LIBRARIES})

    elseif(os STREQUAL "windows")

        set(compiler "msvc192")

        if (build_type STREQUAL "release")
            set(build_type "relwithdebinfo")
        endif()

        set(name "windows_${arch}_${build_type}_${compiler}")

        if (NOT EXISTS ${local_path}/${name}.7z)
            message(STATUS "[libsndfile] Populate: ${remote_url} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(libsndfile_INCLUDE_DIRS ${local_path}/include)
        set(libsndfile_LIBRARIES ${local_path}/lib/sndfile.lib)
        set(libsndfile_INSTALL_LIBRARIES ${local_path}/bin/sndfile.dll)

    else()
        message(FATAL_ERROR "[libsndfile] Not supported os: ${os}")
    endif()

    if(NOT TARGET SndFile::sndfile)
       add_library(SndFile::sndfile INTERFACE IMPORTED GLOBAL)

       target_include_directories(SndFile::sndfile INTERFACE ${libsndfile_INCLUDE_DIRS} )
       target_link_libraries(SndFile::sndfile INTERFACE ${libsndfile_LIBRARIES} )
    endif()

    set_property(GLOBAL PROPERTY libsndfile_INCLUDE_DIRS ${libsndfile_INCLUDE_DIRS})
    set_property(GLOBAL PROPERTY libsndfile_LIBRARIES ${libsndfile_LIBRARIES})
    set_property(GLOBAL PROPERTY libsndfile_INSTALL_LIBRARIES ${libsndfile_INSTALL_LIBRARIES})

endfunction()
