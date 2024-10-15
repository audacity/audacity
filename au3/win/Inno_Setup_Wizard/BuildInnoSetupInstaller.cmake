# This CMake script is invoked to build the InnoSetup installer for Audacity
# Required parameters:
# BUILD_DIR - should be set to CMAKE_BINARY_DIR by the caller
# SOURCE_DIR - should be set to CMAKE_SOURCE_DIR by the caller
# OUTPUT_DIR - directory, where installer will be built
# INNO_SETUP_COMPILER - InnoSetup compiler executable
# BUILDING_64_BIT - Flag, that indicates that we are building a 64-bit installer
# EMBED_MANUAL - embed a fresh copy of manual
# SIGN - sign the installer
# WINDOWS_CERTIFICATE - path to PFX file. If not present, env:WINDOWS_CERTIFICATE will be used
# WINDOWS_CERTIFICATE_PASSWORD - password for the PFX file. If not present, env:WINDOWS_CERTIFICATE_PASSWORD will be used
# USE_GPL3 - set the license to GPLv3 in the installer

if( BUILDING_64_BIT )
    set( INSTALLER_SUFFIX "x64" )
    set( INSTALLER_X64_MODE "ArchitecturesInstallIn64BitMode=x64" )
else()
    set( INSTALLER_SUFFIX "x86" )
    set( INSTALLER_X64_MODE "" )
endif()

if( SIGN )
    set( SIGN_TOOL "SignTool=byparam powershell -ExecutionPolicy Bypass -File \$q${SOURCE_DIR}/scripts/build/windows/PfxSign.ps1\$q -File $f")

    if( WINDOWS_CERTIFICATE )
        string(APPEND SIGN_TOOL " -CertFile \$q${WINDOWS_CERTIFICATE}\$q")
    endif()

    if( WINDOWS_CERTIFICATE_PASSWORD )
        message("Setting env:WINDOWS_CERTIFICATE_PASSWORD...")
        set( ENV{WINDOWS_CERTIFICATE_PASSWORD} "${WINDOWS_CERTIFICATE_PASSWORD}")
    endif()
else()
    set( SIGN_TOOL )
endif()

if( EMBED_MANUAL )
    set ( MANUAL [[Source: "Package\help\manual\*"; DestDir: "{app}\help\manual\"; Flags: ignoreversion recursesubdirs]])
else()
    set( MANUAL )
endif()

if( USE_GPL3 )
   set( GPL_VERSION 3 )
   set( GPL_URL "https://www.gnu.org/licenses/gpl-3.0.en.html")
else()
set( GPL_VERSION 2 )
set( GPL_URL "http://www.gnu.org/licenses/old-licenses/gpl-2.0.html")
endif()

# Prepare the output directory

file(COPY "${SOURCE_DIR}/win/Inno_Setup_Wizard/" DESTINATION "${OUTPUT_DIR}")
configure_file("${OUTPUT_DIR}/audacity.iss.in" "${OUTPUT_DIR}/audacity.iss")
configure_file("${OUTPUT_DIR}/audacity_InnoWizard_InfoBefore.rtf.in" "${OUTPUT_DIR}/audacity_InnoWizard_InfoBefore.rtf")

# Copy additional files

file(COPY
        "${SOURCE_DIR}/LICENSE.txt"
        "${SOURCE_DIR}/win/audacity.ico"
    DESTINATION
        "${OUTPUT_DIR}/Additional"
)

# "Install" prebuilt package

execute_process(
    COMMAND
        ${CMAKE_COMMAND}
            --install ${BUILD_DIR}
            --prefix "${OUTPUT_DIR}/Package"
            --config "${CONFIG}"
)

# Build the installer

execute_process(
    COMMAND
        ${INNO_SETUP_COMPILER} /Sbyparam=$p "audacity.iss"
    WORKING_DIRECTORY
        ${OUTPUT_DIR}
)

# Emulate CPack behavior

file( COPY "${OUTPUT_DIR}/Output/" DESTINATION "${BUILD_DIR}/package" )
