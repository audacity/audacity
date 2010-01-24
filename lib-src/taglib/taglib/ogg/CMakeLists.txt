ADD_SUBDIRECTORY( vorbis ) 
ADD_SUBDIRECTORY( speex ) 
ADD_SUBDIRECTORY( flac ) 

INSTALL( FILES  oggfile.h  	oggpage.h  	oggpageheader.h  	xiphcomment.h DESTINATION ${INCLUDE_INSTALL_DIR}/taglib )

