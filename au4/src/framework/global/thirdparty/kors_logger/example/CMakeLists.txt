cmake_minimum_required(VERSION 3.14)

project(logger_example LANGUAGES CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

option(KORS_LOGGER_QT_SUPPORT "Enabling Qt support" ON)

if (KORS_LOGGER_QT_SUPPORT)
    find_package(Qt5Core)
    add_definitions(-DKORS_LOGGER_QT_SUPPORT)
endif()

include(${CMAKE_CURRENT_LIST_DIR}/../logger.cmake)

include_directories(${KORS_LOGGER_INC})

add_executable(${PROJECT_NAME}
    main.cpp
    ${KORS_LOGGER_SRC}
    log.h
    logger.h
    logstream.h
)

target_link_libraries(${PROJECT_NAME}
    stdc++fs
    pthread
)

if (KORS_LOGGER_QT_SUPPORT)
    target_link_libraries(${PROJECT_NAME} Qt${QT_VERSION_MAJOR}::Core)
endif()
