#[[
Provides time-stretching and pitch-shifting functionality.

Based on the more versatile time-stretching from StaffPad, but tailored for the needs of Audacity.
]]

set( SOURCES
   StaffPad/pffft/pffft.c
   StaffPad/pffft/pffft.h
   StaffPad/pffft/pfsimd_macros.h
   StaffPad/CircularSampleBuffer.h
   StaffPad/FourierTransform_pffft.cpp
   StaffPad/FourierTransform_pffft.h
   StaffPad/SamplesFloat.h
   StaffPad/SimdTypes.h
   StaffPad/SimdTypes_neon.h
   StaffPad/SimdTypes_scalar.h
   StaffPad/SimdTypes_sse2.h
   StaffPad/TimeAndPitch.h
   StaffPad/TimeAndPitchAudioReader.h
   StaffPad/TimeAndPitch.cpp
   StaffPad/TimeAndPitch.h
   StaffPad/VectorOps.h
   AudioContainer.cpp
   AudioContainer.h
   StaffPadTimeAndPitch.cpp
   StaffPadTimeAndPitch.h
   TimeAndPitchInterface.cpp
   TimeAndPitchInterface.h
)
set( LIBRARIES
)
audacity_library( lib-time-and-pitch "${SOURCES}" ""
   "" ""
)
