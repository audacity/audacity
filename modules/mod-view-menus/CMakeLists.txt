list( APPEND SOURCES
   ToolbarMenus.cpp
   ViewMenus.cpp
)
audacity_module( mod-view-menus "${SOURCES}" "Audacity"
   "" "" )
