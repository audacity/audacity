# CCI pakage appears to set the wrong value for the `cmake_target_name`

if(${_OPT}use_rapidjson STREQUAL "local")
   if(NOT TARGET rapidjson::rapidjson)
      add_library( rapidjson::rapidjson ALIAS rapidjson )
   endif()
endif()
