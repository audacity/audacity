#[[
Provides time-stretching and pitch-shifting functionality.

Based on the more versatile time-stretching from StaffPad, but tailored for the needs of Audacity.
]]

set( TARGET au3-time-and-pitch )

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
   DummyFormantShifterLogger.cpp
   DummyFormantShifterLogger.h
   FormantShifter.cpp
   FormantShifter.h
   FormantShifterLogger.cpp
   FormantShifterLogger.h
   FormantShifterLoggerInterface.h
   StaffPadTimeAndPitch.cpp
   StaffPadTimeAndPitch.h
   StretchMethod.h
   TimeAndPitchExperimentalSettings.cpp
   TimeAndPitchExperimentalSettings.h
   TimeAndPitchFactory.cpp
   TimeAndPitchFactory.h
   TimeAndPitchInterface.cpp
   TimeAndPitchInterface.h

   # Speed-only stretchers, for the other stretch methods
   TimeStretchers.h
   PaulStretcher.cpp
   SbsmsStretcher.cpp
   SoundTouchStretcher.cpp
   VarispeedStretcher.cpp
)
set( LIBRARIES
PRIVATE
   au3-fft
   au3-files
   au3-math
   au3-preferences
   au3-strings
   au3-utility
   pffft
   wxBase
   $<$<BOOL:${AU_USE_SBSMS}>:libsbsms>
   $<$<BOOL:${AU_USE_SOUNDTOUCH}>:soundtouch>
)
set( DEFINES
PRIVATE
   $<$<BOOL:${AU_USE_SOUNDTOUCH}>:USE_SOUNDTOUCH=1>
   $<$<BOOL:${AU_USE_SBSMS}>:USE_SBSMS=1>
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "${DEFINES}" ""
)
