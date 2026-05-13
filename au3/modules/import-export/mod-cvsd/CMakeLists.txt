#[[
Export of files with a command passed to a command-line shell
]]

set( TARGET mod-cvsd )

set( SOURCES
        ImportCVSD.cpp
        ExportCVSD.cpp
        CVSD.h
        CVSD.cpp
)

set( LIBRARIES
        PRIVATE
        Audacity
)

set (EXTRA_CLUSTER_NODES "${LIBRARIES}" PARENT_SCOPE)

message(STATUS "DEBUG [${TARGET}]: root is currently set to: ${root}")

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
