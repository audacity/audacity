| Name                     | Type   | Default    | Description                                                     |
| :------------------------| :----- | :--------- | :-------------------------------------------------------------- |
| CMAKE_BUILD_TYPE         | STRING | Debug      | Type of the build: Debug, Release, RelWithDebInfo, MinSizeRel   |
| CMAKE_INSTALL_PREFIX     | PATH   | /usr/local | Install path prefix, prepended onto install directories.        |
| lib_preference           | STRING | local      | Library preference [system (if available), local]               |
| obey_system_dependencies | BOOL   | Off        | Use only system packages to satisfy dependencies                |
| use_expat                | STRING | system     | Use expat library [system (if available), local, off]           |
| use_ffmpeg               | STRING | loaded     | Use ffmpeg library [loaded, linked, off]                        |
| use_flac                 | STRING | local      | Use flac library [system (if available), local, off]            |
| use_id3tag               | STRING | local      | Use id3tag library [system (if available), local, off]          |
| use_ladspa               | BOOL   | ON         | Use LADSPA plug-in support [on, off]                            |
| use_libmad               | STRING | local      | Use libmad library [system (if available), local, off]          |
| use_libmp3lame           | STRING | local      | Use libmp3lame library [system (if available), local, off]      |
| use_lv2                  | STRING | local      | Use lv2 library [system (if available), local, off]             |
| use_mad                  | STRING | local      | Use mad library [system (if available), local, off]             |
| use_midi                 | STRING | local      | Use midi library [system (if available), local, off]            |
| use_nyquist              | STRING | local      | Use nyquist library [local, off]                                |
| use_ogg                  | STRING | local      | Use ogg library [system (if available), local, off]             |
| use_pa_alsa              | BOOL   | YES        | Use the portaudio ALSA interface if available                   |
| use_pa_jack              | STRING | linked     | Use the JACK audio interface if available [loaded, linked, off] |
| use_pa_oss               | BOOL   | YES        | Use the OSS audio interface if available                        |
| use_pch                  | BOOL   | YES        | Use precompiled headers [yes, no]                               |
| use_portaudio            | STRING | local      | Use portaudio library [local]                                   |
| use_portmixer            | STRING | local      | Use portmixer library [local, off]                              |
| use_portsmf              | STRING | local      | Use portsmf library [system (if available), local, off]         |
| use_sbsms                | STRING | local      | Use sbsms library [system (if available), local, off]           |
| use_sndfile              | STRING | local      | Use sndfile library [system (if available), local]              |
| use_soundtouch           | STRING | local      | Use soundtouch library [system (if available), local, off]      |
| use_soxr                 | STRING | local      | Use soxr library [system (if available), local]                 |
| use_sqlite               | STRING | local      | Use sqlite library [system (if available), local]               |
| use_twolame              | STRING | local      | Use twolame library [system (if available), local, off]         |
| use_vamp                 | STRING | local      | Use vamp library [system (if available), local, off]            |
| use_vorbis               | STRING | local      | Use vorbis library [system (if available), local, off]          |
| use_vst                  | BOOL   | ON         | Use VST2 plug-in support [on, off]                              |
| use_wxwidgets            | STRING | local      | Use wxwidgets library [system (if available), local, off]       |
| use_zlib                 | STRING | local      | Use zlib library [system (if available), local, off]            |
| use_curl                 | STRING | local      | Use curl library [system (if available), local]                 |
