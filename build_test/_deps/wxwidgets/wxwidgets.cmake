
function(wxwidgets_Populate remote_url local_path os arch build_type)

    if (os STREQUAL "linux")

        set(compiler "gcc12")

        # At the moment only relwithdebinfo
        # I don't think we need debug builds
        set(name "linux_${arch}_relwithdebinfo_${compiler}")

        if (NOT EXISTS ${local_path}/${name}.7z)
            message(STATUS "[wxwidgets] Populate: ${remote_url} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(wxwidgets_INCLUDE_DIRS
            ${local_path}/include
            ${local_path}/include/wx-3.1
        )

        set(wxwidgets_LIBRARIES
            ${local_path}/lib/libwx_baseu-3.1.so
        )
        set(wxwidgets_INSTALL_LIBRARIES ${wxwidgets_LIBRARIES})

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
            message(STATUS "[wxwidgets] Populate: ${remote_url}/${name} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(wxwidgets_INCLUDE_DIRS
            ${local_path}/include
            ${local_path}/include/wx-3.1
        )

        set(wxwidgets_LIBRARIES
            ${local_path}/lib/libwx_baseu-3.1.dylib
        )
        set(wxwidgets_INSTALL_LIBRARIES ${wxwidgets_LIBRARIES})

    elseif(os STREQUAL "windows")

        set(compiler "msvc194")
        set(suffix "")

        if (build_type STREQUAL "release")
            set(build_type "relwithdebinfo")
        endif()

        if (build_type STREQUAL "debug")
            set(suffix "d")
        endif()

        set(name "windows_${arch}_${build_type}_${compiler}")

        if (NOT EXISTS ${local_path}/${name}.7z)
            message(STATUS "[wxwidgets] Populate: ${remote_url}/${name} to ${local_path} ${os} ${arch} ${build_type}")
            file(DOWNLOAD ${remote_url}/${name}.7z ${local_path}/${name}.7z)
            file(ARCHIVE_EXTRACT INPUT ${local_path}/${name}.7z DESTINATION ${local_path})
        endif()

        set(wxwidgets_INCLUDE_DIRS
            ${local_path}/include
        )

        set(wxwidgets_LIBRARIES
            ${local_path}/lib/vc_x64_dll/wxbase31u${suffix}.lib
        )

        set(wxwidgets_INSTALL_LIBRARIES
            ${local_path}/lib/vc_x64_dll/wxbase313u${suffix}_vc_x64_custom.dll
        )

    else()
        message(FATAL_ERROR "[wxwidgets] Not supported os: ${os}")
    endif()

    add_library(wxwidgets::wxwidgets INTERFACE IMPORTED GLOBAL)
    target_include_directories(wxwidgets::wxwidgets INTERFACE ${wxwidgets_INCLUDE_DIRS})
    target_link_libraries(wxwidgets::wxwidgets INTERFACE ${wxwidgets_LIBRARIES})

    set_property(GLOBAL PROPERTY wxwidgets_INCLUDE_DIRS ${wxwidgets_INCLUDE_DIRS})
    set_property(GLOBAL PROPERTY wxwidgets_LIBRARIES ${wxwidgets_LIBRARIES})
    set_property(GLOBAL PROPERTY wxwidgets_INSTALL_LIBRARIES ${wxwidgets_INSTALL_LIBRARIES})

endfunction()

