set(REMOTE_ROOT_URL https://raw.githubusercontent.com/musescore/framework_tmp/main/buildscripts/ci/checkcodestyle)
set(LOCAL_ROOT_PATH ${CMAKE_CURRENT_LIST_DIR}/_deps)

file(DOWNLOAD ${REMOTE_ROOT_URL}/ci_files.cmake ${LOCAL_ROOT_PATH}/ci_files.cmake )

include(${LOCAL_ROOT_PATH}/ci_files.cmake)

foreach(FILE_PATH ${CI_FILES})
    file(DOWNLOAD ${REMOTE_ROOT_URL}/${FILE_PATH} ${LOCAL_ROOT_PATH}/${FILE_PATH} )
endforeach()

foreach(FILE_PATH ${CI_EXEC_FILES})
    file(DOWNLOAD ${REMOTE_ROOT_URL}/${FILE_PATH} ${LOCAL_ROOT_PATH}/${FILE_PATH})
    file(CHMOD ${LOCAL_ROOT_PATH}/${FILE_PATH} PERMISSIONS OWNER_EXECUTE GROUP_EXECUTE WORLD_EXECUTE)
endforeach()
