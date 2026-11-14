# This is a CMake script that properly calls the PfxSign.ps1

if( DEFINED WINDOWS_CERTIFICATE_PASSWORD )
    set( ENV{WINDOWS_CERTIFICATE_PASSWORD} "${WINDOWS_CERTIFICATE_PASSWORD}" )
endif()

if(DEFINED WINDOWS_CERTIFICATE)
    execute_process(
        COMMAND powershell 
            -ExecutionPolicy Bypass 
            -File ${PFX_SIGN_PS_LOCATION}
            -Directory ${CMAKE_INSTALL_PREFIX}
            -CertFile ${WINDOWS_CERTIFICATE}
    )
elseif(DEFINED ENV{WINDOWS_CERTIFICATE})
    execute_process(
        COMMAND powershell 
            -ExecutionPolicy Bypass 
            -File ${PFX_SIGN_PS_LOCATION}
            -Directory ${CMAKE_INSTALL_PREFIX}
    )
else()
    message(FATAL_ERROR [[
        Code signing is skipped, as certifcate is missing. 
        Please, set the path to PFX file using -DWINDOWS_CERTIFICATE=... 
        or set the environment variable WINDOWS_CERTIFICATE to base64 encoded PFX certifcate.
    ]])
endif()
