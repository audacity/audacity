
if (MUSESCORE_REVISION STREQUAL "")
    if(EXISTS "${PROJECT_SOURCE_DIR}/local_build_revision.env")
        file(READ "${PROJECT_SOURCE_DIR}/local_build_revision.env" MUSESCORE_REVISION)
        STRING(REGEX REPLACE "\n" "" MUSESCORE_REVISION "${MUSESCORE_REVISION}")
    endif()
endif()
