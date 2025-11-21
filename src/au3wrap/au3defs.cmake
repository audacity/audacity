
set(PKGLIBDIR "${_PKGLIBDIR}" )
set(LIBDIR "${CMAKE_INSTALL_FULL_LIBDIR}" )

set(AU3_DEF

    -DAUDACITY_VERSION=4
    -DAUDACITY_RELEASE=0
    -DAUDACITY_REVISION=0
    -DAUDACITY_MODLEVEL=0

    # Version string for visual display
    -DAUDACITY_VERSION_STRING=L"${AUDACITY_VERSION}.${AUDACITY_RELEASE}.${AUDACITY_REVISION}${AUDACITY_SUFFIX}"

    # This value is used in the resource compiler for Windows
    -DAUDACITY_FILE_VERSION=L"${AUDACITY_VERSION},${AUDACITY_RELEASE},${AUDACITY_REVISION},${AUDACITY_MODLEVEL}"

    -Dsafenew=new

    -DUTILITY_API=
    -DPROJECT_API=
    -DSTRINGS_API=
    -DEXCEPTIONS_API=
    -DREGISTRIES_API=
    -DPREFERENCES_API=
    -DXML_API=
    -DBASIC_UI_API=
    -DCOMPONENTS_API=
    -DSTRING_UTILS_API=
    -DFILES_API=
    -DPROJECT_FILE_IO_API=
    -DPROJECT_HISTORY_API=
    -DMATH_API=
    -DFFT_API=
    -DTRANSACTIONS_API=
    -DSTRETCHING_SEQUENCE_API=
    -DWAVE_TRACK_API=
    -DWAVE_TRACK_FFT_API=
    -DSAMPLE_TRACK_API=
    -DMIXER_API=
    -DAUDIO_GRAPH_API=
    -DPLAYABLE_TRACK_API=
    -DTRACK_API=
    -DCHANNEL_API=
    -DTIME_AND_PITCH_API=
    -DPROJECT_RATE_API=
    -DTRACK_SELECTION_API=
    -DAUDIO_DEVICES_API=
    -DAUDIO_IO_API=
    -DREALTIME_EFFECTS_API=
    -DMODULE_MANAGER_API=
    -DWX_INIT_API=
    -DVIEWPORT_API=
    -DSNAPPING_API=
    -DNUMERIC_FORMATS_API=
    -DLABEL_TRACK_API=
    -DBUILTIN_EFFECTS_API=

    -DTIME_FREQUENCY_SELECTION_API=
    -DSCREEN_GEOMETRY_API=
    -DSQLITE_HELPERS_API=

    -DGRAPHICS_API=
    -DWAVE_TRACK_PAINT_API=

    -DEXPERIMENTAL_SPECTRAL_EDITING

    -DEFFECTS_API=
    -DBUILTIN_EFFECTS_API=
    -DVST3_API=
    -DUSE_LV2
    -DLV2_API=
    -DPKGLIBDIR="${PKGLIBDIR}"
    -DLIBDIR="${LIBDIR}"
    -DIPC_API=

    -DCOMMAND_PARAMETERS_API=
    -DAUDACITY_APPLICATION_LOGIC_API=
    -DMENUS_API=
    -DDYNAMIC_RANGE_PROCESSOR_API=

    -DIMPORT_EXPORT_API=
    -DTAGS_API=
    -DFILE_FORMATS_API=

    -DFFMPEG_SUPPORT_API=
)

set(AU3_LIBRARIES ${AUDACITY_ROOT}/libraries)
set(AU3_MODULES ${AUDACITY_ROOT}/modules)

# AU3 include directories - only external dependencies and special paths
# Library-specific includes are now handled by each library's CMakeLists.txt
set(AU3_INCLUDE
    ${wxwidgets_INCLUDE_DIRS}
    ${expat_INCLUDE_DIRS}
    ${PortAudio_INCLUDE_DIR}
    ${libmp3lame_INCLUDE_DIRS}
    ${wavpack_INCLUDE_DIRS}
    ${libsndfile_INCLUDE_DIRS}
    ${vorbis_INCLUDE_DIRS}
    ${flac_INCLUDE_DIRS}
    ${ogg_INCLUDE_DIRS}
    ${opus_INCLUDE_DIRS}

    ${AUDACITY_ROOT}/lib-src/sqlite
    ${AUDACITY_ROOT}/lib-src/pffft
    ${AUDACITY_ROOT}/lib-src/libsoxr/src
    ${AUDACITY_ROOT}/lib-src/portmixer
    # As for the above libraries, SoundTouch includes: Even though soundtouch is linked via AU3_LINK, the include paths
    # don't propagate through the static library chain, so we need to add them explicitly
    ${AUDACITY_ROOT}/lib-src/soundtouch/include
    # also include the Generated soundtouch_config.h (needed by STTypes.h when compiling AU3 code that uses SoundTouch)
    ${CMAKE_BINARY_DIR}/src/au3wrap/au3-soundtouch/private
    # SBSMS includes: Similar to SoundTouch, we need to add SBSMS include paths explicitly
    ${AUDACITY_ROOT}/lib-src/libsbsms/include
    # also include the Generated config.h for SBSMS
    ${CMAKE_BINARY_DIR}/src/au3wrap/au3-sbsms/private

    ${AUDACITY_ROOT}

    # FFmpeg module (not yet converted to library)
    ${AU3_MODULES}/import-export/mod-ffmpeg
)

set(WXBASE_RESTRICTIONS
   "wxUSE_GUI=0"

   # Don't use app.h
   _WX_APP_H_BASE_

   # Don't use evtloop.h
   _WX_EVTLOOP_H_

   # Don't use image.h
   _WX_IMAGE_H

   # Don't use colour.h
   _WX_COLOUR_H_BASE_

   # Don't use brush.h
   _WX_BRUSH_H_BASE_

   # Don't use pen.h
   _WX_PEN_H_BASE_
)

set(AU3_LINK
    wxwidgets::wxwidgets
    expat::expat
    portaudio::portaudio
    libmp3lame::libmp3lame
    wavpack::wavpack
    mpg123::libmpg123
    SndFile::sndfile
    Vorbis::vorbis
    FLAC::FLAC
    Ogg::ogg
    Opus::opus
    sqlite
    soxr
    pffft
    portmixer
    soundtouch
    libsbsms
)

set(WXBASE_DEFS
    ${WXBASE_RESTRICTIONS}
)

include(GetPlatformInfo)

if (OS_IS_LIN)
    set(WXBASE_DEFS ${WXBASE_DEFS} __WXGTK__)
elseif(OS_IS_MAC)
    set(WXBASE_DEFS ${WXBASE_DEFS} __WXMAC__)
    find_library(CoreAudio NAMES CoreAudio)
    find_library(CoreAudioKit NAMES CoreAudioKit)
    set(AU3_LINK ${AU3_LINK} zlib::zlib ${CoreAudio} ${CoreAudioKit})
elseif(OS_IS_WIN)
   set(WXBASE_DEFS ${WXBASE_DEFS} __WXMSW__ WXUSINGDLL)
   set(AU3_LINK ${AU3_LINK} zlib::zlib winmm mmdevapi mfplat)
endif()

set(AU3_DEF ${AU3_DEF} ${WXBASE_DEFS})
