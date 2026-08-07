if(NOT ARTIFACT_PATH OR NOT MAX_SIZE_BYTES)
    message(FATAL_ERROR "ARTIFACT_PATH and MAX_SIZE_BYTES are required")
endif()

if(NOT EXISTS "${ARTIFACT_PATH}")
    message(FATAL_ERROR "Release artifact does not exist: ${ARTIFACT_PATH}")
endif()

file(SIZE "${ARTIFACT_PATH}" _artifact_size)
if(_artifact_size GREATER MAX_SIZE_BYTES)
    math(EXPR _artifact_size_mb "${_artifact_size} / 1000000")
    math(EXPR _max_size_mb "${MAX_SIZE_BYTES} / 1000000")
    message(FATAL_ERROR
        "Release artifact is ${_artifact_size_mb} MB, exceeding the ${_max_size_mb} MB budget: ${ARTIFACT_PATH}"
    )
endif()

message(STATUS "Release artifact size: ${_artifact_size} bytes (budget ${MAX_SIZE_BYTES})")
