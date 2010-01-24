
set(CMAKE_INCLUDE_CURRENT_DIR ON)
INCLUDE_DIRECTORIES(
    ${CMAKE_CURRENT_SOURCE_DIR}/toolkit
    ${CMAKE_CURRENT_SOURCE_DIR}/asf
    ${CMAKE_CURRENT_SOURCE_DIR}/mpeg
    ${CMAKE_CURRENT_SOURCE_DIR}/ogg
    ${CMAKE_CURRENT_SOURCE_DIR}/ogg/flac
    ${CMAKE_CURRENT_SOURCE_DIR}/flac
    ${CMAKE_CURRENT_SOURCE_DIR}/mpc
    ${CMAKE_CURRENT_SOURCE_DIR}/mp4
    ${CMAKE_CURRENT_SOURCE_DIR}/ogg/vorbis
    ${CMAKE_CURRENT_SOURCE_DIR}/ogg/speex
    ${CMAKE_CURRENT_SOURCE_DIR}/mpeg/id3v2
    ${CMAKE_CURRENT_SOURCE_DIR}/mpeg/id3v1
    ${CMAKE_CURRENT_SOURCE_DIR}/ape
    ${CMAKE_CURRENT_SOURCE_DIR}/wavpack
    ${CMAKE_CURRENT_SOURCE_DIR}/trueaudio
    ${CMAKE_CURRENT_SOURCE_DIR}/riff
    ${CMAKE_CURRENT_SOURCE_DIR}/riff/aiff
    ${CMAKE_CURRENT_SOURCE_DIR}/riff/wav
    ${CMAKE_CURRENT_BINARY_DIR}/taglib
    ${CMAKE_CURRENT_BINARY_DIR}/..
)
if(ZLIB_FOUND)
    INCLUDE_DIRECTORIES(${ZLIB_INCLUDE_DIR})
endif(ZLIB_FOUND)

ADD_SUBDIRECTORY( toolkit )
ADD_SUBDIRECTORY( asf )
ADD_SUBDIRECTORY( mpeg )
ADD_SUBDIRECTORY( ogg )
ADD_SUBDIRECTORY( flac )
ADD_SUBDIRECTORY( ape )
ADD_SUBDIRECTORY( mpc )
ADD_SUBDIRECTORY( mp4 )
ADD_SUBDIRECTORY( wavpack )
ADD_SUBDIRECTORY( trueaudio )
ADD_SUBDIRECTORY( riff )

########### next target ###############

SET(mpeg_SRCS
mpeg/mpegfile.cpp
mpeg/mpegproperties.cpp
mpeg/mpegheader.cpp
mpeg/xingheader.cpp
)

SET(id3v1_SRCS
mpeg/id3v1/id3v1tag.cpp
mpeg/id3v1/id3v1genres.cpp
)


SET(id3v2_SRCS
mpeg/id3v2/id3v2framefactory.cpp
mpeg/id3v2/id3v2synchdata.cpp
mpeg/id3v2/id3v2tag.cpp
mpeg/id3v2/id3v2header.cpp
mpeg/id3v2/id3v2frame.cpp
mpeg/id3v2/id3v2footer.cpp
mpeg/id3v2/id3v2extendedheader.cpp
)


SET(frames_SRCS
mpeg/id3v2/frames/attachedpictureframe.cpp
mpeg/id3v2/frames/commentsframe.cpp
mpeg/id3v2/frames/generalencapsulatedobjectframe.cpp
mpeg/id3v2/frames/popularimeterframe.cpp
mpeg/id3v2/frames/privateframe.cpp
mpeg/id3v2/frames/relativevolumeframe.cpp
mpeg/id3v2/frames/textidentificationframe.cpp
mpeg/id3v2/frames/uniquefileidentifierframe.cpp
mpeg/id3v2/frames/unknownframe.cpp
mpeg/id3v2/frames/unsynchronizedlyricsframe.cpp
mpeg/id3v2/frames/urllinkframe.cpp
)

SET(ogg_SRCS
ogg/oggfile.cpp
ogg/oggpage.cpp
ogg/oggpageheader.cpp
ogg/xiphcomment.cpp
)

SET(vorbis_SRCS
ogg/vorbis/vorbisfile.cpp
ogg/vorbis/vorbisproperties.cpp
)


SET(flacs_SRCS
flac/flacfile.cpp
flac/flacproperties.cpp
)

SET(oggflacs_SRCS
ogg/flac/oggflacfile.cpp
)

SET(mpc_SRCS
mpc/mpcfile.cpp
mpc/mpcproperties.cpp
)

IF(WITH_MP4)
SET(mp4_SRCS
mp4/mp4file.cpp
mp4/mp4atom.cpp
mp4/mp4tag.cpp
mp4/mp4item.cpp
mp4/mp4properties.cpp
)
ELSE(WITH_MP4)
SET(mp4_SRCS)
ENDIF(WITH_MP4)

SET(ape_SRCS
ape/apetag.cpp
ape/apefooter.cpp
ape/apeitem.cpp
)

SET(wavpack_SRCS
wavpack/wavpackfile.cpp
wavpack/wavpackproperties.cpp
)

SET(speex_SRCS
ogg/speex/speexfile.cpp
ogg/speex/speexproperties.cpp
)

SET(trueaudio_SRCS
trueaudio/trueaudiofile.cpp
trueaudio/trueaudioproperties.cpp
)

IF(WITH_ASF)
SET(asf_SRCS
asf/asftag.cpp
asf/asffile.cpp
asf/asfproperties.cpp
asf/asfattribute.cpp
)
ELSE(WITH_ASF)
SET(asf_SRCS)
ENDIF(WITH_ASF)

SET(riff_SRCS
riff/rifffile.cpp
)

SET(aiff_SRCS
riff/aiff/aifffile.cpp
riff/aiff/aiffproperties.cpp
)

SET(wav_SRCS
riff/wav/wavfile.cpp
riff/wav/wavproperties.cpp
)

SET(toolkit_SRCS
toolkit/tstring.cpp
toolkit/tstringlist.cpp
toolkit/tbytevector.cpp
toolkit/tbytevectorlist.cpp
toolkit/tfile.cpp
toolkit/tdebug.cpp
toolkit/unicode.cpp
)

SET(tag_LIB_SRCS ${mpeg_SRCS} ${id3v1_SRCS} ${id3v2_SRCS} ${frames_SRCS} ${ogg_SRCS}
		 ${vorbis_SRCS} ${oggflacs_SRCS} ${mpc_SRCS} ${ape_SRCS} ${toolkit_SRCS} ${flacs_SRCS}
		 ${wavpack_SRCS} ${speex_SRCS} ${trueaudio_SRCS} ${riff_SRCS} ${aiff_SRCS} ${wav_SRCS}
		 ${mp4_SRCS} ${asf_SRCS}
		 tag.cpp
		 tagunion.cpp
		 fileref.cpp
		 audioproperties.cpp
)


ADD_LIBRARY(tag SHARED ${tag_LIB_SRCS})

TARGET_LINK_LIBRARIES(tag )
if(ZLIB_FOUND)
	TARGET_LINK_LIBRARIES(tag ${ZLIB_LIBRARIES})
endif(ZLIB_FOUND)

SET_TARGET_PROPERTIES(tag PROPERTIES
        VERSION ${TAGLIB_LIB_MAJOR_VERSION}.${TAGLIB_LIB_MINOR_VERSION}.${TAGLIB_LIB_PATCH_VERSION}
        SOVERSION ${TAGLIB_LIB_MAJOR_VERSION}
        INSTALL_NAME_DIR ${LIB_INSTALL_DIR}
        DEFINE_SYMBOL MAKE_TAGLIB_LIB
)
INSTALL(TARGETS tag
	LIBRARY DESTINATION ${LIB_INSTALL_DIR}
	RUNTIME DESTINATION ${BIN_INSTALL_DIR}
	ARCHIVE DESTINATION  ${LIB_INSTALL_DIR}
)

INSTALL( FILES  tag.h fileref.h audioproperties.h taglib_export.h DESTINATION ${INCLUDE_INSTALL_DIR}/taglib)
