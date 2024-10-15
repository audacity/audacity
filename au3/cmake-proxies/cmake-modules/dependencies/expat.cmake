if(NOT TARGET expat::expat)
    set_target_properties(EXPAT::EXPAT PROPERTIES IMPORTED_GLOBAL TRUE)
    add_library(expat::expat ALIAS EXPAT::EXPAT)
endif()
