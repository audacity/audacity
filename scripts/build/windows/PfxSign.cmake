# This is a CMake script that properly calls the PfxSign.ps1

if(DEFINED ENV{WINDOWS_CODE_SIGNING_ACCESS_KEY_ID} AND DEFINED ENV{WINDOWS_CODE_SIGNING_SECRET_ACCESS_KEY})
    execute_process(
        COMMAND powershell 
            -ExecutionPolicy Bypass 
            -File ${PFX_SIGN_PS_LOCATION}
            -Directory ${CMAKE_INSTALL_PREFIX}
    )
else()
    message(FATAL_ERROR [[
        Code signing is skipped, as required environment variables are missing.
        Please set the following environment variables:
            - WINDOWS_CODE_SIGNING_ACCESS_KEY_ID
            - WINDOWS_CODE_SIGNING_SECRET_ACCESS_KEY
    ]])
endif()
