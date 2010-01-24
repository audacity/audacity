ADD_SUBDIRECTORY( id3v1 ) 
ADD_SUBDIRECTORY( id3v2 ) 

INSTALL(FILES  mpegfile.h mpegproperties.h mpegheader.h xingheader.h DESTINATION ${INCLUDE_INSTALL_DIR}/taglib )
