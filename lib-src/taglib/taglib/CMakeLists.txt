set(CMAKE_INCLUDE_CURRENT_DIR ON)
include_directories(
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
  ${CMAKE_CURRENT_SOURCE_DIR}/ogg/opus
  ${CMAKE_CURRENT_SOURCE_DIR}/mpeg/id3v2
  ${CMAKE_CURRENT_SOURCE_DIR}/mpeg/id3v2/frames
  ${CMAKE_CURRENT_SOURCE_DIR}/mpeg/id3v1
  ${CMAKE_CURRENT_SOURCE_DIR}/ape
  ${CMAKE_CURRENT_SOURCE_DIR}/wavpack
  ${CMAKE_CURRENT_SOURCE_DIR}/trueaudio
  ${CMAKE_CURRENT_SOURCE_DIR}/riff
  ${CMAKE_CURRENT_SOURCE_DIR}/riff/aiff
  ${CMAKE_CURRENT_SOURCE_DIR}/riff/wav
  ${CMAKE_CURRENT_SOURCE_DIR}/mod
  ${CMAKE_CURRENT_SOURCE_DIR}/s3m
  ${CMAKE_CURRENT_SOURCE_DIR}/it
  ${CMAKE_CURRENT_SOURCE_DIR}/xm
)

if(ZLIB_FOUND)
  include_directories(${ZLIB_INCLUDE_DIR})
endif()

set(tag_HDRS
  tag.h
  fileref.h
  audioproperties.h
  taglib_export.h
  ${CMAKE_BINARY_DIR}/taglib_config.h
  toolkit/taglib.h
  toolkit/tstring.h
  toolkit/tlist.h
  toolkit/tlist.tcc
  toolkit/tstringlist.h
  toolkit/tbytevector.h
  toolkit/tbytevectorlist.h
  toolkit/tbytevectorstream.h
  toolkit/tiostream.h
  toolkit/tfile.h
  toolkit/tfilestream.h
  toolkit/tmap.h
  toolkit/tmap.tcc
  toolkit/tpropertymap.h
  toolkit/trefcounter.h
  toolkit/tdebuglistener.h
  mpeg/mpegfile.h
  mpeg/mpegproperties.h
  mpeg/mpegheader.h
  mpeg/xingheader.h
  mpeg/id3v1/id3v1tag.h
  mpeg/id3v1/id3v1genres.h
  mpeg/id3v2/id3v2extendedheader.h
  mpeg/id3v2/id3v2frame.h
  mpeg/id3v2/id3v2header.h
  mpeg/id3v2/id3v2synchdata.h
  mpeg/id3v2/id3v2footer.h
  mpeg/id3v2/id3v2framefactory.h
  mpeg/id3v2/id3v2tag.h
  mpeg/id3v2/frames/attachedpictureframe.h
  mpeg/id3v2/frames/commentsframe.h
  mpeg/id3v2/frames/generalencapsulatedobjectframe.h
  mpeg/id3v2/frames/ownershipframe.h
  mpeg/id3v2/frames/popularimeterframe.h
  mpeg/id3v2/frames/privateframe.h
  mpeg/id3v2/frames/relativevolumeframe.h
  mpeg/id3v2/frames/textidentificationframe.h
  mpeg/id3v2/frames/uniquefileidentifierframe.h
  mpeg/id3v2/frames/unknownframe.h
  mpeg/id3v2/frames/unsynchronizedlyricsframe.h
  mpeg/id3v2/frames/urllinkframe.h
  ogg/oggfile.h
  ogg/oggpage.h
  ogg/oggpageheader.h
  ogg/xiphcomment.h
  ogg/vorbis/vorbisfile.h
  ogg/vorbis/vorbisproperties.h
  ogg/flac/oggflacfile.h
  ogg/speex/speexfile.h
  ogg/speex/speexproperties.h
  ogg/opus/opusfile.h
  ogg/opus/opusproperties.h
  flac/flacfile.h
  flac/flacpicture.h
  flac/flacproperties.h
  flac/flacmetadatablock.h
  ape/apefile.h
  ape/apeproperties.h
  ape/apetag.h
  ape/apefooter.h
  ape/apeitem.h
  mpc/mpcfile.h
  mpc/mpcproperties.h
  wavpack/wavpackfile.h
  wavpack/wavpackproperties.h
  trueaudio/trueaudiofile.h
  trueaudio/trueaudioproperties.h
  riff/rifffile.h
  riff/aiff/aifffile.h
  riff/aiff/aiffproperties.h
  riff/wav/wavfile.h
  riff/wav/wavproperties.h
  riff/wav/infotag.h
  asf/asffile.h
  asf/asfproperties.h
  asf/asftag.h
  asf/asfattribute.h
  asf/asfpicture.h
  mp4/mp4file.h
  mp4/mp4atom.h
  mp4/mp4tag.h
  mp4/mp4item.h
  mp4/mp4properties.h
  mp4/mp4coverart.h
  mod/modfilebase.h
  mod/modfile.h
  mod/modtag.h
  mod/modproperties.h
  it/itfile.h
  it/itproperties.h
  s3m/s3mfile.h
  s3m/s3mproperties.h
  xm/xmfile.h
  xm/xmproperties.h
)

set(mpeg_SRCS
  mpeg/mpegfile.cpp
  mpeg/mpegproperties.cpp
  mpeg/mpegheader.cpp
  mpeg/xingheader.cpp
)

set(id3v1_SRCS
  mpeg/id3v1/id3v1tag.cpp
  mpeg/id3v1/id3v1genres.cpp
)

set(id3v2_SRCS
  mpeg/id3v2/id3v2framefactory.cpp
  mpeg/id3v2/id3v2synchdata.cpp
  mpeg/id3v2/id3v2tag.cpp
  mpeg/id3v2/id3v2header.cpp
  mpeg/id3v2/id3v2frame.cpp
  mpeg/id3v2/id3v2footer.cpp
  mpeg/id3v2/id3v2extendedheader.cpp
  )

set(frames_SRCS
  mpeg/id3v2/frames/attachedpictureframe.cpp
  mpeg/id3v2/frames/commentsframe.cpp
  mpeg/id3v2/frames/generalencapsulatedobjectframe.cpp
  mpeg/id3v2/frames/ownershipframe.cpp
  mpeg/id3v2/frames/popularimeterframe.cpp
  mpeg/id3v2/frames/privateframe.cpp
  mpeg/id3v2/frames/relativevolumeframe.cpp
  mpeg/id3v2/frames/textidentificationframe.cpp
  mpeg/id3v2/frames/uniquefileidentifierframe.cpp
  mpeg/id3v2/frames/unknownframe.cpp
  mpeg/id3v2/frames/unsynchronizedlyricsframe.cpp
  mpeg/id3v2/frames/urllinkframe.cpp
)

set(ogg_SRCS
  ogg/oggfile.cpp
  ogg/oggpage.cpp
  ogg/oggpageheader.cpp
  ogg/xiphcomment.cpp
)

set(vorbis_SRCS
  ogg/vorbis/vorbisfile.cpp
  ogg/vorbis/vorbisproperties.cpp
)

set(flacs_SRCS
  flac/flacfile.cpp
  flac/flacpicture.cpp
  flac/flacproperties.cpp
  flac/flacmetadatablock.cpp
  flac/flacunknownmetadatablock.cpp
)

set(oggflacs_SRCS
  ogg/flac/oggflacfile.cpp
)

set(mpc_SRCS
  mpc/mpcfile.cpp
  mpc/mpcproperties.cpp
)

set(mp4_SRCS
  mp4/mp4file.cpp
  mp4/mp4atom.cpp
  mp4/mp4tag.cpp
  mp4/mp4item.cpp
  mp4/mp4properties.cpp
  mp4/mp4coverart.cpp
)

set(ape_SRCS
  ape/apetag.cpp
  ape/apefooter.cpp
  ape/apeitem.cpp
  ape/apefile.cpp
  ape/apeproperties.cpp
)

set(wavpack_SRCS
  wavpack/wavpackfile.cpp
  wavpack/wavpackproperties.cpp
)

set(speex_SRCS
  ogg/speex/speexfile.cpp
  ogg/speex/speexproperties.cpp
)

set(opus_SRCS
  ogg/opus/opusfile.cpp
  ogg/opus/opusproperties.cpp
)

set(trueaudio_SRCS
  trueaudio/trueaudiofile.cpp
  trueaudio/trueaudioproperties.cpp
)

set(asf_SRCS
  asf/asftag.cpp
  asf/asffile.cpp
  asf/asfproperties.cpp
  asf/asfattribute.cpp
  asf/asfpicture.cpp
)

set(riff_SRCS
  riff/rifffile.cpp
)

set(aiff_SRCS
  riff/aiff/aifffile.cpp
  riff/aiff/aiffproperties.cpp
)

set(wav_SRCS
  riff/wav/wavfile.cpp
  riff/wav/wavproperties.cpp
  riff/wav/infotag.cpp
)

set(mod_SRCS
  mod/modfilebase.cpp
  mod/modfile.cpp
  mod/modtag.cpp
  mod/modproperties.cpp
)

set(s3m_SRCS
  s3m/s3mfile.cpp
  s3m/s3mproperties.cpp
)

set(it_SRCS
  it/itfile.cpp
  it/itproperties.cpp
)

set(xm_SRCS
  xm/xmfile.cpp
  xm/xmproperties.cpp
)

set(toolkit_SRCS
  toolkit/tstring.cpp
  toolkit/tstringlist.cpp
  toolkit/tbytevector.cpp
  toolkit/tbytevectorlist.cpp
  toolkit/tbytevectorstream.cpp
  toolkit/tiostream.cpp
  toolkit/tfile.cpp
  toolkit/tfilestream.cpp
  toolkit/tdebug.cpp
  toolkit/tpropertymap.cpp
  toolkit/trefcounter.cpp
  toolkit/tdebuglistener.cpp
  toolkit/unicode.cpp
)

set(tag_LIB_SRCS
  ${mpeg_SRCS} ${id3v1_SRCS} ${id3v2_SRCS} ${frames_SRCS} ${ogg_SRCS}
  ${vorbis_SRCS} ${oggflacs_SRCS} ${mpc_SRCS} ${ape_SRCS} ${toolkit_SRCS} ${flacs_SRCS}
  ${wavpack_SRCS} ${speex_SRCS} ${trueaudio_SRCS} ${riff_SRCS} ${aiff_SRCS} ${wav_SRCS}
  ${asf_SRCS} ${mp4_SRCS} ${mod_SRCS} ${s3m_SRCS} ${it_SRCS} ${xm_SRCS} ${opus_SRCS}
  tag.cpp
  tagunion.cpp
  fileref.cpp
  audioproperties.cpp
)

add_library(tag ${tag_LIB_SRCS} ${tag_HDRS})

if(ZLIB_FOUND)
	target_link_libraries(tag ${ZLIB_LIBRARIES})
endif()

set_target_properties(tag PROPERTIES
  VERSION ${TAGLIB_SOVERSION_MAJOR}.${TAGLIB_SOVERSION_MINOR}.${TAGLIB_SOVERSION_PATCH}
  SOVERSION ${TAGLIB_SOVERSION_MAJOR}
  INSTALL_NAME_DIR ${LIB_INSTALL_DIR}
  DEFINE_SYMBOL MAKE_TAGLIB_LIB
  LINK_INTERFACE_LIBRARIES ""
  PUBLIC_HEADER "${tag_HDRS}"
)
if(BUILD_FRAMEWORK)
  set_target_properties(tag PROPERTIES FRAMEWORK TRUE)
endif()

install(TARGETS tag
	FRAMEWORK DESTINATION ${FRAMEWORK_INSTALL_DIR}
	LIBRARY DESTINATION ${LIB_INSTALL_DIR}
	RUNTIME DESTINATION ${BIN_INSTALL_DIR}
	ARCHIVE DESTINATION ${LIB_INSTALL_DIR}
	PUBLIC_HEADER DESTINATION ${INCLUDE_INSTALL_DIR}/taglib
)

