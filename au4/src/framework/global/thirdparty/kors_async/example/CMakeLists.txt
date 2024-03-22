cmake_minimum_required(VERSION 3.5)

project(async_example LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

include(${CMAKE_CURRENT_LIST_DIR}/../async/async.cmake)

add_executable(${PROJECT_NAME}
    ${KORS_ASYNC_SRC}
    main.cpp
    # aliases
    async/asyncable.h
    async/processevents.h
    async/channel.h
    async/notification.h
    async/async.h
    async/promise.h
)
