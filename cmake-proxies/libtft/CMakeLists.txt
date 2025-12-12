add_library( ${TARGET} STATIC )

configure_file(${TARGET_ROOT}/src/version.h.in ${TARGET_ROOT}/src/version.h)
set(SOURCES
      ${TARGET_ROOT}/src/WaveletCalculator.cpp 
      ${TARGET_ROOT}/src/WaveletCalculator.h
      ${TARGET_ROOT}/src/StftCalculator.cpp 
      ${TARGET_ROOT}/src/StftCalculator.h
      ${TARGET_ROOT}/src/RealFFTf.cpp 
      ${TARGET_ROOT}/src/RealFFTf.h
      ${TARGET_ROOT}/src/TimeFrequencyCalculator.h
      ${TARGET_ROOT}/src/DyadicFilter.cpp
      ${TARGET_ROOT}/src/DyadicFilter.h
      ${TARGET_ROOT}/src/WaveletVoice.h 
      ${TARGET_ROOT}/src/WaveletVoice.cpp
      ${TARGET_ROOT}/src/WaveletVoiceUnbuffered.h 
      ${TARGET_ROOT}/src/WaveletVoiceUnbuffered.cpp
      ${TARGET_ROOT}/src/ConfinedGaussianWaveletVoice.h 
      ${TARGET_ROOT}/src/ConfinedGaussianWaveletVoice.cpp
)

set(INCLUDES
      ${TARGET_ROOT}/src
)

set_target_properties( ${TARGET} PROPERTIES FOLDER "lib-src" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_include_directories( ${TARGET} PUBLIC ${INCLUDES} )
set_property(TARGET ${TARGET} PROPERTY POSITION_INDEPENDENT_CODE ON)

