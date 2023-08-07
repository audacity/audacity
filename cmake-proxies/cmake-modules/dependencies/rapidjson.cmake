# CCI pakage appears to set the wrong value for the `cmake_target_name`

if(NOT ${_OPT}use_rapidjson STREQUAL "off")
   if(NOT TARGET rapidjson::rapidjson)
      if(TARGET rapidjson)
         add_library( rapidjson::rapidjson ALIAS rapidjson )
      else()
         # At least on Arch RapidJSONConfig.cmake does not define a target at all
         # so we have to do it ourselves
         add_library( rapidjson::rapidjson INTERFACE IMPORTED GLOBAL)
         if (RAPIDJSON_INCLUDE_DIRS )
            target_include_directories( rapidjson::rapidjson INTERFACE ${RAPIDJSON_INCLUDE_DIRS} )
         elseif(RapidJSON_INCLUDE_DIR)
            target_include_directories( rapidjson::rapidjson INTERFACE ${RapidJSON_INCLUDE_DIR} )
         endif()
      endif()
   endif()
endif()
