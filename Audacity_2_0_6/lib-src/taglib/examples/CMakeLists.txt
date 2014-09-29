if(BUILD_EXAMPLES)
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_SOURCE_DIR}/../taglib
		     ${CMAKE_CURRENT_SOURCE_DIR}/../taglib/toolkit
		     ${CMAKE_CURRENT_SOURCE_DIR}/../taglib/ape
		     ${CMAKE_CURRENT_SOURCE_DIR}/../taglib/mpeg
		     ${CMAKE_CURRENT_SOURCE_DIR}/../taglib/mpeg/id3v1
		     ${CMAKE_CURRENT_SOURCE_DIR}/../taglib/mpeg/id3v2
		     ${CMAKE_CURRENT_SOURCE_DIR}/../bindings/c/  )

if(ENABLE_STATIC)
    add_definitions(-DTAGLIB_STATIC)
endif(ENABLE_STATIC)

########### next target ###############

ADD_EXECUTABLE(tagreader tagreader.cpp)

TARGET_LINK_LIBRARIES(tagreader  tag )


########### next target ###############

ADD_EXECUTABLE(tagreader_c tagreader_c.c)

TARGET_LINK_LIBRARIES(tagreader_c  tag_c )


########### next target ###############

ADD_EXECUTABLE(tagwriter tagwriter.cpp)

TARGET_LINK_LIBRARIES(tagwriter  tag )


########### next target ###############

ADD_EXECUTABLE(framelist framelist.cpp)

TARGET_LINK_LIBRARIES(framelist  tag )


########### next target ###############

ADD_EXECUTABLE(strip-id3v1 strip-id3v1.cpp)

TARGET_LINK_LIBRARIES(strip-id3v1  tag )


endif(BUILD_EXAMPLES)

