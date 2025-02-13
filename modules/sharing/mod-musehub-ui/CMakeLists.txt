#[[
Access MuseHub features
]]

set( TARGET mod-musehub-ui )

set( SOURCES
   MusehubModule.cpp
   GetEffectsDialog.cpp
   GetEffectsDialog.h
)

list(APPEND LIBRARIES
   lib-wx-wrappers-interface
   lib-musehub-interface
)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
