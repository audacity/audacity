if(BUILD_TESTS)

INCLUDE_DIRECTORIES(
  ${CMAKE_CURRENT_SOURCE_DIR}/../taglib
  ${CMAKE_CURRENT_SOURCE_DIR}/../taglib/toolkit
  ${CMAKE_CURRENT_SOURCE_DIR}/../taglib/asf
  ${CMAKE_CURRENT_SOURCE_DIR}/../taglib/mpeg/id3v1
  ${CMAKE_CURRENT_SOURCE_DIR}/../taglib/mpeg/id3v2
  ${CMAKE_CURRENT_SOURCE_DIR}/../taglib/mpeg/id3v2/frames
  ${CMAKE_CURRENT_SOURCE_DIR}/../taglib/mpeg
  ${CMAKE_CURRENT_SOURCE_DIR}/../taglib/mp4
  ${CMAKE_CURRENT_SOURCE_DIR}/../taglib/trueaudio
)

SET(test_runner_SRCS
  main.cpp
  test_list.cpp
  test_map.cpp
  test_mpeg.cpp
  test_synchdata.cpp
  test_trueaudio.cpp
  test_bytevector.cpp
  test_string.cpp
  test_fileref.cpp
  test_id3v1.cpp
  test_id3v2.cpp
)
IF(WITH_MP4)
   SET(test_runner_SRCS ${test_runner_SRCS} test_mp4.cpp)
ENDIF(WITH_MP4)

IF(WITH_ASF)
   SET(test_runner_SRCS ${test_runner_SRCS} test_asf.cpp)
ENDIF(WITH_ASF)

ADD_EXECUTABLE(test_runner ${test_runner_SRCS})
TARGET_LINK_LIBRARIES(test_runner tag ${CPPUNIT_LIBRARIES})

ADD_CUSTOM_TARGET(check
    ./test_runner
    DEPENDS test_runner
)

endif(BUILD_TESTS)
