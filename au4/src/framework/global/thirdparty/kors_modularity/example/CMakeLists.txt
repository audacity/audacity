cmake_minimum_required(VERSION 3.14)

project(modularity_example LANGUAGES CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(${CMAKE_CURRENT_LIST_DIR}/../modularity/modularity.cmake)

add_executable(${PROJECT_NAME}
    main.cpp
    ${KORS_MODULARITY_SRC}
    app.cpp
    app.h
    # aliases
    modularity/imoduleinterface.h
    modularity/imodulesetup.h
    modularity/ioc.h
    # module alpha
    alpha/alphamodule.cpp
    alpha/alphamodule.h
    alpha/ialphaservice.h
    alpha/internal/alphaservice.cpp
    alpha/internal/alphaservice.h
    # module vita
    vita/vitamodule.cpp
    vita/vitamodule.h
    vita/ivitaservice.h
    vita/internal/vitaservice.cpp
    vita/internal/vitaservice.h


)
