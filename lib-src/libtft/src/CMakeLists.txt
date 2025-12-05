cmake_minimum_required(VERSION 3.10)

# set the project name
project(tft VERSION 0.1)

# specify the C++ standard
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

configure_file(version.h.in version.h)
set(SOURCE_FILES
WaveletCalculator.cpp WaveletCalculator.h
StftCalculator.cpp StftCalculator.h
TimeFrequencyCalculator.h
DyadicFilter.cpp DyadicFilter.h
WaveletVoice.h WaveletVoice.cpp
WaveletVoiceUnbuffered.h WaveletVoiceUnbuffered.cpp
ConfinedGaussianWaveletVoice.h ConfinedGaussianWaveletVoice.cpp
RealFFTf.h RealFFTf.cpp
)
add_library(tft ${SOURCE_FILES})
target_include_directories(tft PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
