#[[
Non-modal mixer board dialog provides alternative mute/solo buttons and volume
sliders for all tracks in one small frame
]]

list( APPEND SOURCES
   MixerBoard.cpp
   MixerBoard.h
)
audacity_module( mod-mixer-board "${SOURCES}" "Audacity"
   "" "" )
