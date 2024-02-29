#[[
Provides time-stretching and pitch-shifting functionality.

Based on the more versatile time-stretching from StaffPad, but tailored for the needs of Audacity.
]]

add_definitions( -DCMAKE_CURRENT_SOURCE_DIR="${CMAKE_CURRENT_SOURCE_DIR}" )

set( SOURCES
   StaffPad/CircularSampleBuffer.h
   StaffPad/FourierTransform_pffft.cpp
   StaffPad/FourierTransform_pffft.h
   StaffPad/SamplesFloat.h
   StaffPad/SimdComplexConversions_sse2.h
   StaffPad/SimdTypes.h
   StaffPad/SimdTypes_neon.h
   StaffPad/SimdTypes_scalar.h
   StaffPad/SimdTypes_sse2.h
   StaffPad/TimeAndPitch.h
   StaffPad/TimeAndPitch.cpp
   StaffPad/TimeAndPitch.h
   StaffPad/VectorOps.h
   AudioContainer.cpp
   AudioContainer.h
   FormantShifter.cpp
   FormantShifter.h
   FormantShifterLogger.cpp
   FormantShifterLogger.h
   FormantShifterLoggerInterface.h
   StaffPadTimeAndPitch.cpp
   StaffPadTimeAndPitch.h
   TimeAndPitchInterface.cpp
   TimeAndPitchInterface.h
)
set( LIBRARIES
PUBLIC
   pffft
)
audacity_library( lib-time-and-pitch "${SOURCES}" "${LIBRARIES}"
   "" ""
)
