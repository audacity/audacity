# libscorealign depends on portsmf
if( NOT TARGET portsmf )
   addlib( portsmf            portsmf     PORTSMF     NO    YES   "portSMF >= 0.1" )
endif()

add_library( ${TARGET} STATIC )
target_link_libraries( ${TARGET} PRIVATE "portsmf" )

def_vars()

list( APPEND SOURCES
      ${TARGET_ROOT}/audioreader.h
      ${TARGET_ROOT}/scorealign.h
      ${TARGET_ROOT}/scorealign-glue.h
      ${TARGET_ROOT}/ScoreAlignParams.h
   PRIVATE
      ${TARGET_ROOT}/audiomixerreader.cpp
      ${TARGET_ROOT}/audiomixerreader.h
      ${TARGET_ROOT}/audioreader.cpp
      ${TARGET_ROOT}/comp_chroma.cpp
      ${TARGET_ROOT}/comp_chroma.h
      ${TARGET_ROOT}/curvefit.cpp
      ${TARGET_ROOT}/curvefit.h
      ${TARGET_ROOT}/gen_chroma.cpp
      ${TARGET_ROOT}/gen_chroma.h
      ${TARGET_ROOT}/hillclimb.cpp
      ${TARGET_ROOT}/hillclimb.h
      ${TARGET_ROOT}/regression.cpp
      ${TARGET_ROOT}/regression.h
      ${TARGET_ROOT}/sautils.cpp
      ${TARGET_ROOT}/sautils.h
      ${TARGET_ROOT}/scorealign.cpp
      ${TARGET_ROOT}/scorealign-glue.cpp
      ${TARGET_ROOT}/fft3/FFT3.cpp
      ${TARGET_ROOT}/fft3/FFT3.h
)

list( APPEND INCLUDES
   PUBLIC
      ${TARGET_ROOT}
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

