addlib( libsbsms           sbsms       SBSMS       NO    YES   "sbsms >= 2.2.0" )
addlib( soundtouch         soundtouch  SOUNDTOUCH  NO    YES   "soundtouch >= 1.7.1" )

set( SOURCES
   ChangePitch.cpp
   ChangePitch.h
   ChangeSpeed.cpp
   ChangeSpeed.h
   ChangeTempo.cpp
   ChangeTempo.h
   ModSoundTouchMain.cpp
   SBSMSEffect.cpp
   SBSMSEffect.h
   SoundTouchEffect.cpp
   SoundTouchEffect.h
   TimeScale.cpp
   TimeScale.h
)
set( LIBS
   Audacity
   libsbsms
   soundtouch
)
audacity_module( mod-soundtouch "${SOURCES}" "${LIBS}"
   "" "" )
