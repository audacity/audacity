# Runtime-client and job-state boundary. No provider process is launched here.
declare_module(aijobs)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/iruntimeclient.h
    ${CMAKE_CURRENT_LIST_DIR}/fakeruntimeclient.h
    ${CMAKE_CURRENT_LIST_DIR}/fakeruntimeclient.cpp
    ${CMAKE_CURRENT_LIST_DIR}/runtimehostsupervisor.h
    ${CMAKE_CURRENT_LIST_DIR}/runtimehostsupervisor.cpp
)

set(MODULE_LINK aicore)

setup_module()

add_subdirectory(runtimehost)
