# Defines innosetup target
# The target will be only generated if InnoSetup 6 is installed.

find_program(
    INNO_SETUP_COMPILER 
    NAMES iscc ISCC
    HINTS 
        "C:/Program Files (x86)/Inno Setup 6" 
        "C:/Program Files/Inno Setup 6"
)

if( INNO_SETUP_COMPILER )
    message(STATUS "Creating target innosetup...")

    set( TEMP_PACKAGE_PATH "${CMAKE_BINARY_DIR}/innosetup" )

    add_custom_target(
        innosetup
        COMMAND
            ${CMAKE_COMMAND}
                -DBUILD_DIR=${CMAKE_BINARY_DIR}
                -DSOURCE_DIR=${CMAKE_SOURCE_DIR}
                -DOUTPUT_DIR=${TEMP_PACKAGE_PATH}
                -DINNO_SETUP_COMPILER=${INNO_SETUP_COMPILER}
                -DEMBED_MANUAL=${${_OPT}package_manual}
                -DBUILDING_64_BIT=${IS_64BIT}
                -DSIGN=${${_OPT}perform_codesign}
                -DWINDOWS_CERTIFICATE=${WINDOWS_CERTIFICATE}
                -D WINDOWS_CERTIFICATE_PASSWORD=${WINDOWS_CERTIFICATE_PASSWORD}
                -D CONFIG=$<CONFIG>
                -D USE_GPL3=${${_OPT}bundle_gplv3}
                -P "${CMAKE_SOURCE_DIR}/win/Inno_Setup_Wizard/BuildInnoSetupInstaller.cmake"
        VERBATIM
    )

    if( ${_OPT}package_manual )
        add_dependencies( innosetup manual )
    endif()

    set_target_properties( innosetup PROPERTIES FOLDER "packaging" )
endif()
