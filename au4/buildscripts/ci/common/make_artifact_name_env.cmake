
# Config
set(ARTIFACTS_DIR "build.artifacts")

# Options
set(PREFIX "" CACHE STRING "Prefix")
set(ARTIFACT_INFO "" CACHE STRING "Artifact info")
set(BUILD_NUMBER "" CACHE STRING "Build number")

if (NOT ARTIFACT_INFO)
    message(FATAL_ERROR "Not set ARTIFACT_INFO")
endif()

if (NOT BUILD_NUMBER)
    file (STRINGS "${ARTIFACTS_DIR}/env/build_number.env" BUILD_NUMBER)
endif()

if (NOT BUILD_NUMBER)
    message(FATAL_ERROR "Not set BUILD_NUMBER")
endif()

set(NOT_ALLOWED_SYMBOLS \" : < > | * ? / \\ ’ " ")

foreach(SYMBOL ${NOT_ALLOWED_SYMBOLS})
    string(REPLACE "${SYMBOL}" "_" ARTIFACT_INFO ${ARTIFACT_INFO})
endforeach()

set(ARTIFACT_NAME "${PREFIX}_${BUILD_NUMBER}_${ARTIFACT_INFO}")

file(MAKE_DIRECTORY ${ARTIFACTS_DIR})
file(MAKE_DIRECTORY ${ARTIFACTS_DIR}/env)

file(WRITE ${ARTIFACTS_DIR}/env/artifact_name.env ${ARTIFACT_NAME})
message(STATUS "ARTIFACT_NAME: ${ARTIFACT_NAME} (${ARTIFACTS_DIR}/env/artifact_name.env)")

