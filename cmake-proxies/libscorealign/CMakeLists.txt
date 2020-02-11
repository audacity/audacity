
add_library( ${TARGET} STATIC )

def_vars()

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/audioreader.cpp
      ${TARGET_ROOT}/comp_chroma.cpp
      ${TARGET_ROOT}/curvefit.cpp
      ${TARGET_ROOT}/gen_chroma.cpp
      ${TARGET_ROOT}/hillclimb.cpp
      ${TARGET_ROOT}/regression.cpp
      ${TARGET_ROOT}/sautils.cpp
      ${TARGET_ROOT}/scorealign.cpp
      ${TARGET_ROOT}/fft3/FFT3.cpp
)

list( APPEND INCLUDES
   PUBLIC
      ${TARGET_ROOT}
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

