if(NOT USE_LIBMPG123)
   return()
endif()

if(NOT TARGET mpg123::libmpg123)
   add_library(mpg123::libmpg123 ALIAS MPG123::libmpg123)
endif()
