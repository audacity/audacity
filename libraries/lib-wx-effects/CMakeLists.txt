#[[
Utilities for wxWidgets implementation of user interface for Effects
]]

set( SOURCES
   BasicEffectUIServices.cpp
   BasicEffectUIServices.h
   EffectEditor.cpp
   EffectEditor.h
   EffectUIServices.cpp
   EffectUIServices.h
   StatefulEffect.cpp
   StatefulEffect.h
   StatefulEffectUIServices.cpp
   StatefulEffectUIServices.h
   StatefulPerTrackEffect.cpp
   StatefulPerTrackEffect.h
   StatelessPerTrackEffect.cpp
   StatelessPerTrackEffect.h
)
set( LIBRARIES
   lib-effects-interface
   lib-wx-init-interface
)
audacity_library( lib-wx-effects "${SOURCES}" "${LIBRARIES}"
   "" ""
)
