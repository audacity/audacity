foreach (required
        WORKER_PACKAGE_DIR
        WORKER_BUILD_DIR
        WORKER_OUTPUT_FILE
        SWIFT_EXECUTABLE)
    if (NOT DEFINED ${required} OR "${${required}}" STREQUAL "")
        message(FATAL_ERROR "BuildSpeechSwiftWorker.cmake requires ${required}")
    endif()
endforeach()

set(worker_environment "MACOSX_DEPLOYMENT_TARGET=15.0")
if (DEFINED SPEECH_SWIFT_PACKAGE_PATH AND NOT SPEECH_SWIFT_PACKAGE_PATH STREQUAL "")
    list(APPEND worker_environment "SPEECH_SWIFT_PACKAGE_PATH=${SPEECH_SWIFT_PACKAGE_PATH}")
endif()

# SwiftPM updates Package.resolved when the optional local dependency override
# changes the package graph. Build from a staged copy so a CMake build never
# modifies the source checkout's pinned remote dependency graph.
set(worker_staging_dir "${WORKER_BUILD_DIR}-package")
set(worker_scratch_dir "${WORKER_BUILD_DIR}")
file(REMOVE_RECURSE "${worker_staging_dir}")
file(MAKE_DIRECTORY "${worker_staging_dir}")
file(COPY
    "${WORKER_PACKAGE_DIR}/Package.swift"
    "${WORKER_PACKAGE_DIR}/Sources"
    "${WORKER_PACKAGE_DIR}/Tests"
    DESTINATION "${worker_staging_dir}"
)
if (EXISTS "${WORKER_PACKAGE_DIR}/Package.resolved")
    file(COPY "${WORKER_PACKAGE_DIR}/Package.resolved"
        DESTINATION "${worker_staging_dir}")
endif()

set(swift_build_command
    "${CMAKE_COMMAND}" -E env ${worker_environment}
    "${SWIFT_EXECUTABLE}" build
    --package-path "${worker_staging_dir}"
    --scratch-path "${worker_scratch_dir}"
    --configuration release
    --arch arm64
    --product audacity-speech-swift-worker
    --disable-sandbox
)

execute_process(
    COMMAND ${swift_build_command}
    RESULT_VARIABLE build_result
)
if (NOT build_result EQUAL 0)
    message(FATAL_ERROR "Failed to build audacity-speech-swift-worker (${build_result})")
endif()

execute_process(
    COMMAND
        "${CMAKE_COMMAND}" -E env ${worker_environment}
        "${SWIFT_EXECUTABLE}" build
        --package-path "${worker_staging_dir}"
        --scratch-path "${worker_scratch_dir}"
        --configuration release
        --arch arm64
        --show-bin-path
        --disable-sandbox
    RESULT_VARIABLE bin_path_result
    OUTPUT_VARIABLE worker_bin_dir
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
if (NOT bin_path_result EQUAL 0 OR worker_bin_dir STREQUAL "")
    message(FATAL_ERROR "Could not locate the Swift worker build output")
endif()

set(built_worker "${worker_bin_dir}/audacity-speech-swift-worker")
if (NOT EXISTS "${built_worker}")
    message(FATAL_ERROR "Swift worker was not produced at ${built_worker}")
endif()

get_filename_component(worker_output_dir "${WORKER_OUTPUT_FILE}" DIRECTORY)
file(MAKE_DIRECTORY "${worker_output_dir}")
file(COPY_FILE "${built_worker}" "${WORKER_OUTPUT_FILE}" ONLY_IF_DIFFERENT)
file(CHMOD "${WORKER_OUTPUT_FILE}"
    PERMISSIONS
        OWNER_READ OWNER_WRITE OWNER_EXECUTE
        GROUP_READ GROUP_EXECUTE
        WORLD_READ WORLD_EXECUTE
)
