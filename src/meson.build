
configure_file(
  input : 'audacity.desktop.in',
  output: 'audacity.desktop',
  configuration: desktop_conf,
  install_dir: join_paths(get_option('datadir'), 'applications')
)

install_data('audacity.xml',
  install_dir: join_paths(get_option('datadir'), 'mime', 'packages')
)

subdir('blockfile')
subdir('commands')
subdir('effects')
subdir('export')
subdir('import')
subdir('ondemand')
subdir('prefs')
subdir('toolbars')
subdir('tracks')
subdir('widgets')
subdir('xml')


libaudacity_tests_sources = files(
  'BlockFile.cpp',
  'DirManager.cpp',
  'Dither.cpp',
  'FileFormats.cpp',
  'Internat.cpp',
  'Prefs.cpp',
  'SampleFormat.cpp',
  'Sequence.cpp',

  'xml/XMLTagHandler.cpp',
) + [
  blockfile_sources,
]

audacity_sources = files(
  'AboutDialog.cpp',
  'AColor.cpp',
  'AudacityApp.cpp',
  'AudacityException.cpp',
  'AudacityLogger.cpp',
  'AudioIO.cpp',
  'AutoRecovery.cpp',
  'BatchCommandDialog.cpp',
  'BatchCommands.cpp',
  'BatchProcessDialog.cpp',
  'Benchmark.cpp',
  'Dependencies.cpp',
  'DeviceChange.cpp',
  'DeviceManager.cpp',
  'Diags.cpp',
  'Envelope.cpp',
  'FFmpeg.cpp',
  'FFT.cpp',
  'FileException.cpp',
  'FileIO.cpp',
  'FileNames.cpp',
  'FreqWindow.cpp',
  'HelpText.cpp',
  'HistoryWindow.cpp',
  'ImageManipulation.cpp',
  'InconsistencyException.cpp',
  'InterpolateAudio.cpp',
  'LabelDialog.cpp',
  'LabelTrack.cpp',
  'LangChoice.cpp',
  'Languages.cpp',
  'Legacy.cpp',
  'Lyrics.cpp',
  'LyricsWindow.cpp',
  'Matrix.cpp',
  'Menus.cpp',
  'Mix.cpp',
  'MixerBoard.cpp',
  'ModuleManager.cpp',
  'NoteTrack.cpp',
  'PitchName.cpp',
  'PlatformCompatibility.cpp',
  'PluginManager.cpp',
  'Printing.cpp',
  'Profiler.cpp',
  'Project.cpp',
  'RealFFTf.cpp',
  'RealFFTf48x.cpp',
  'Resample.cpp',
  'RingBuffer.cpp',
  'Screenshot.cpp',
  'SelectedRegion.cpp',
  'SelectionState.cpp',
  'Shuttle.cpp',
  'ShuttleGui.cpp',
  'ShuttlePrefs.cpp',
  'Snap.cpp',
  'SoundActivatedRecord.cpp',
  'Spectrum.cpp',
  'SplashDialog.cpp',
  'SseMathFuncs.cpp',
  'Tags.cpp',
  'Theme.cpp',
  'TimeDialog.cpp',
  'TimerRecordDialog.cpp',
  'TimeTrack.cpp',
  'Track.cpp',
  'TrackArtist.cpp',
  'TrackPanel.cpp',
  'TrackPanelAx.cpp',
  'TrackPanelResizeHandle.cpp',
  'TrackPanelResizerCell.cpp',
  'UIHandle.cpp',
  'UndoManager.cpp',
  'UserException.cpp',
  'ViewInfo.cpp',
  'VoiceKey.cpp',
  'WaveClip.cpp',
  'WaveTrack.cpp',
  'WrappedType.cpp',
) + [
  libaudacity_tests_sources,

  commands_sources,
  effects_sources,
  export_sources,
  import_sources,
  ondemand_sources,
  prefs_sources,
  toolbars_sources,
  tracks_sources,
  widgets_sources,
  files(
    'xml/XMLFileReader.cpp',
    'xml/XMLWriter.cpp',
  ),
]


# -D__STDC_CONSTANT_MACROS \
# -DWXINTL_NO_GETTEXT_MACRO \

srcInclude = include_directories('.')

libaudacity_tests = static_library('audacity_testslib',
  libaudacity_tests_sources,
  include_directories: rootInclude,
  dependencies: [
    wxwidgets,
  ],
)

audacity = executable('audacity',
  audacity_sources,
  include_directories: rootInclude,
  dependencies: [
    wxwidgets,
    filedialog,
    widgetextra,
    libnyquist,
    gtk,
    soundtouch,
    lilv,
    suil,
    gstreamer,
    portaudio,
    portaudiocpp,
    libflac,
    libflacpp,
    vorbis,
    vorbisenc,
    vorbisfile,
    libogg,
    expat,
    portmidi,
    libavcodec, libavformat, libavutil,
    lame,
    soxr,
    id3tag,
    mad,
    twolame,
    lv2,
    vamp,
    vamphost,
    sbsms,
    sndfile,
    portsmf,

  ],
  build_by_default: false,
  install: false,
)