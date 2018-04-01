#directory cmake-proxies/portaudio-v19
set( TARGET portaudio-v19 )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
#${LIB_SRC_DIRECTORY}portaudio-v19/bindings/java/c/src/com_portaudio_BlockingStream.c
#${LIB_SRC_DIRECTORY}portaudio-v19/bindings/java/c/src/com_portaudio_PortAudio.c
#${LIB_SRC_DIRECTORY}portaudio-v19/bindings/java/c/src/jpa_tools.c
#${LIB_SRC_DIRECTORY}portaudio-v19/examples/paex_mono_asio_channel_select.c
#${LIB_SRC_DIRECTORY}portaudio-v19/examples/paex_ocean_shore.c
#${LIB_SRC_DIRECTORY}portaudio-v19/examples/paex_pink.c
#${LIB_SRC_DIRECTORY}portaudio-v19/examples/paex_read_write_wire.c
#${LIB_SRC_DIRECTORY}portaudio-v19/examples/paex_record.c
#${LIB_SRC_DIRECTORY}portaudio-v19/examples/paex_record_file.c
#${LIB_SRC_DIRECTORY}portaudio-v19/examples/paex_saw.c
#${LIB_SRC_DIRECTORY}portaudio-v19/examples/paex_sine.c
#${LIB_SRC_DIRECTORY}portaudio-v19/examples/paex_wmme_ac3.c
#${LIB_SRC_DIRECTORY}portaudio-v19/examples/paex_wmme_surround.c
#${LIB_SRC_DIRECTORY}portaudio-v19/examples/paex_write_sine.c
#${LIB_SRC_DIRECTORY}portaudio-v19/examples/paex_write_sine_nonint.c
#${LIB_SRC_DIRECTORY}portaudio-v19/examples/pa_devs.c
#${LIB_SRC_DIRECTORY}portaudio-v19/examples/pa_fuzz.c
#${LIB_SRC_DIRECTORY}portaudio-v19/pablio/pablio.c
#${LIB_SRC_DIRECTORY}portaudio-v19/pablio/test_rw.c
#${LIB_SRC_DIRECTORY}portaudio-v19/pablio/test_rw_echo.c
#${LIB_SRC_DIRECTORY}portaudio-v19/pablio/test_w_saw.c
#${LIB_SRC_DIRECTORY}portaudio-v19/pablio/test_w_saw8.c
#${LIB_SRC_DIRECTORY}portaudio-v19/qa/paqa_devs.c
#${LIB_SRC_DIRECTORY}portaudio-v19/qa/paqa_errs.c
#${LIB_SRC_DIRECTORY}portaudio-v19/qa/paqa_latency.c
#${LIB_SRC_DIRECTORY}portaudio-v19/qa/loopback/src/audio_analyzer.c
#${LIB_SRC_DIRECTORY}portaudio-v19/qa/loopback/src/biquad_filter.c
#${LIB_SRC_DIRECTORY}portaudio-v19/qa/loopback/src/paqa.c
#${LIB_SRC_DIRECTORY}portaudio-v19/qa/loopback/src/paqa_tools.c
#${LIB_SRC_DIRECTORY}portaudio-v19/qa/loopback/src/test_audio_analyzer.c
#${LIB_SRC_DIRECTORY}portaudio-v19/qa/loopback/src/write_wav.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/common/pa_allocation.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/common/pa_converters.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/common/pa_cpuload.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/common/pa_debugprint.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/common/pa_dither.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/common/pa_dynload.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/common/pa_front.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/common/pa_process.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/common/pa_ringbuffer.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/common/pa_stream.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/common/pa_trace.c
#${LIB_SRC_DIRECTORY}portaudio-v19/src/hostapi/alsa/pa_linux_alsa.c
#${LIB_SRC_DIRECTORY}portaudio-v19/src/hostapi/asihpi/pa_linux_asihpi.c
#${LIB_SRC_DIRECTORY}portaudio-v19/src/hostapi/coreaudio/pa_mac_core.c
#${LIB_SRC_DIRECTORY}portaudio-v19/src/hostapi/coreaudio/pa_mac_core_blocking.c
#${LIB_SRC_DIRECTORY}portaudio-v19/src/hostapi/coreaudio/pa_mac_core_old.c
#${LIB_SRC_DIRECTORY}portaudio-v19/src/hostapi/coreaudio/pa_mac_core_utilities.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/hostapi/dsound/pa_win_ds.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/hostapi/dsound/pa_win_ds_dynlink.c
#${LIB_SRC_DIRECTORY}portaudio-v19/src/hostapi/jack/pa_jack.c
#${LIB_SRC_DIRECTORY}portaudio-v19/src/hostapi/jack/pa_jack_dynload.c
#${LIB_SRC_DIRECTORY}portaudio-v19/src/hostapi/oss/pa_unix_oss.c
#${LIB_SRC_DIRECTORY}portaudio-v19/src/hostapi/oss/recplay.c
#${LIB_SRC_DIRECTORY}portaudio-v19/src/hostapi/skeleton/pa_hostapi_skeleton.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/hostapi/wasapi/pa_win_wasapi.c
#${LIB_SRC_DIRECTORY}portaudio-v19/src/hostapi/wdmks/pa_win_wdmks.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/hostapi/wmme/pa_win_wmme.c
#${LIB_SRC_DIRECTORY}portaudio-v19/src/os/unix/pa_unix_hostapis.c
#${LIB_SRC_DIRECTORY}portaudio-v19/src/os/unix/pa_unix_util.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/os/win/pa_win_coinitialize.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/os/win/pa_win_hostapis.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/os/win/pa_win_util.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/os/win/pa_win_waveformat.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/os/win/pa_win_wdmks_utils.c
${LIB_SRC_DIRECTORY}portaudio-v19/src/os/win/pa_x86_plain_converters.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest1.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_buffer.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_callbackstop.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_clip.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_converters.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_dither.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_dsound_find_best_latency_params.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_dsound_low_level_latency_params.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_dsound_surround.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_hang.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_in_overflow.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_jack_wasapi.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_latency.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_leftright.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_longsine.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_many.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_maxsines.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_mono.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_multi_sine.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_out_underflow.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_prime.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_read_record.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_ringmix.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_sine8.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_sine_channelmaps.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_sine_formats.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_sine_srate.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_sine_time.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_start_stop.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_stop.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_stop_playout.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_suggested_vs_streaminfo_latency.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_sync.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_timing.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_toomanysines.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_two_rates.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_underflow.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_wire.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_wmme_find_best_latency_params.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_wmme_low_level_latency_params.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/patest_write_stop.c
#${LIB_SRC_DIRECTORY}portaudio-v19/test/pa_minlat.c

##${LIB_SRC_DIRECTORY}portaudio-v19/examples/paex_sine_c++.cpp
#${LIB_SRC_DIRECTORY}portaudio-v19/src/hostapi/asio/iasiothiscallresolver.cpp
#${LIB_SRC_DIRECTORY}portaudio-v19/src/hostapi/asio/pa_asio.cpp

)
# This defines the #define on both Windows and Linux.
add_definitions( 
-D_LIB
 )
add_library( ${TARGET} STATIC ${SOURCES})

#if(MSVC)
#FIX-ME What a horrible hacky place for a config file!
    add_definitions(/FI"${top_dir}/win/Projects/${TARGET}/Debug/config.h")
#else()
    # GCC or Clang
#    add_definitions(-include ${top_dir}/win/Projects/${TARGET}/config.h)
#endif()

target_include_directories( ${TARGET} PRIVATE 
${TARGET_SOURCE}/include
${TARGET_SOURCE}/src/common
${TARGET_SOURCE}/src/os/win
${TARGET_SOURCE}
)

target_link_libraries( ${TARGET} )