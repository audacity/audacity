bin_PROGRAMS = audacity

desktopdir = $(datadir)/applications
desktop_DATA = audacity.desktop

mimedir = $(datarootdir)/mime/packages
dist_mime_DATA = audacity.xml

check_LTLIBRARIES = libaudacity.la

libaudacity_la_CPPFLAGS = $(WX_CXXFLAGS)
libaudacity_la_LIBADD = $(WX_LIBS)

libaudacity_la_SOURCES = \
	BlockFile.cpp \
	BlockFile.h \
	DirManager.cpp \
	DirManager.h \
	Dither.cpp \
	Dither.h \
	FileFormats.cpp \
	FileFormats.h \
	Internat.cpp \
	Internat.h \
	Prefs.cpp \
	Prefs.h \
	SampleFormat.cpp \
	SampleFormat.h \
	Sequence.cpp \
	Sequence.h \
	blockfile/LegacyAliasBlockFile.cpp \
	blockfile/LegacyAliasBlockFile.h \
	blockfile/LegacyBlockFile.cpp \
	blockfile/LegacyBlockFile.h \
	blockfile/ODDecodeBlockFile.cpp \
	blockfile/ODDecodeBlockFile.h \
	blockfile/ODPCMAliasBlockFile.cpp \
	blockfile/ODPCMAliasBlockFile.h \
	blockfile/PCMAliasBlockFile.cpp \
	blockfile/PCMAliasBlockFile.h \
	blockfile/SilentBlockFile.cpp \
	blockfile/SilentBlockFile.h \
	blockfile/SimpleBlockFile.cpp \
	blockfile/SimpleBlockFile.h \
	xml/XMLTagHandler.cpp \
	xml/XMLTagHandler.h \
	$(NULL)

audacity_CPPFLAGS = \
	-std=c++11 \
	-Wno-deprecated-declarations \
	-D__STDC_CONSTANT_MACROS \
	-DLIBDIR=\"$(libdir)\" \
	$(EXPAT_CFLAGS) \
	$(FILEDIALOG_CFLAGS) \
	$(PORTAUDIO_CFLAGS) \
	$(PORTMIXER_CFLAGS) \
	$(SNDFILE_CFLAGS) \
	$(SOXR_CFLAGS) \
	$(WIDGETEXTRA_CFLAGS) \
	$(WX_CXXFLAGS) \
	$(NULL)

# Until we upgrade to a newer version of wxWidgets...will get rid of hundreds of these:
#
#  /usr/local/include/wx-2.8/wx/buffer.h: In member function 'void wxMemoryBuffer::AppendByte(char)':
#  /usr/local/include/wx-2.8/wx/debug.h:194:43: warning: typedef 'wxDummyCheckInt' locally defined but not used [-Wunused-local-typedefs]
#       #define wxFORCE_SEMICOLON typedef int wxDummyCheckInt
#                                           ^
#  /usr/local/include/wx-2.8/wx/debug.h:224:9: note: in expansion of macro 'wxFORCE_SEMICOLON'
#           wxFORCE_SEMICOLON /* just to force a semicolon */
#           ^
#  /usr/local/include/wx-2.8/wx/debug.h:233:38: note: in expansion of macro 'wxCHECK2_MSG'
#   #define wxCHECK_RET(cond, msg)       wxCHECK2_MSG(cond, return, msg)
#                                        ^
#  /usr/local/include/wx-2.8/wx/buffer.h:289:9: note: in expansion of macro 'wxCHECK_RET'
#           wxCHECK_RET( m_bufdata->m_data, wxT("invalid wxMemoryBuffer") );
#           ^
audacity_CXXFLAGS = \
   -Wno-unused-local-typedefs

audacity_LDFLAGS = -rdynamic
audacity_LDADD = \
	$(EXPAT_LIBS) \
	$(FILEDIALOG_LIBS) \
	$(PORTAUDIO_LIBS) \
	$(PORTMIXER_LIBS) \
	$(SNDFILE_LIBS) \
	$(SOXR_LIBS) \
	$(WIDGETEXTRA_LIBS) \
	$(WX_LIBS) \
	$(NULL)

audacity_SOURCES = \
	$(libaudacity_la_SOURCES) \
	AboutDialog.cpp \
	AboutDialog.h \
	AColor.cpp \
	AColor.h \
	AllThemeResources.h \
	Audacity.h \
	AudacityApp.cpp \
	AudacityApp.h \
	AudacityLogger.cpp \
	AudacityLogger.h \
	AudioIO.cpp \
	AudioIO.h \
	AudioIOListener.h \
	AutoRecovery.cpp \
	AutoRecovery.h \
	BatchCommandDialog.cpp \
	BatchCommandDialog.h \
	BatchCommands.cpp \
	BatchCommands.h \
	BatchProcessDialog.cpp \
	BatchProcessDialog.h \
	Benchmark.cpp \
	Benchmark.h \
	Dependencies.cpp \
	Dependencies.h \
	DeviceChange.cpp \
	DeviceChange.h \
	DeviceManager.cpp \
	DeviceManager.h \
	Diags.cpp \
	Diags.h \
	Envelope.cpp \
	Envelope.h \
	Experimental.h \
	FFmpeg.cpp \
	FFmpeg.h \
	FFT.cpp \
	FFT.h \
	FileIO.cpp \
	FileIO.h \
	FileNames.cpp \
	FileNames.h \
	float_cast.h \
	FreqWindow.cpp \
	FreqWindow.h \
	HelpText.cpp \
	HelpText.h \
	HistoryWindow.cpp \
	HistoryWindow.h \
	ImageManipulation.cpp \
	ImageManipulation.h \
	InterpolateAudio.cpp \
	InterpolateAudio.h \
	LabelDialog.cpp \
	LabelDialog.h \
	LabelTrack.cpp \
	LabelTrack.h \
	LangChoice.cpp \
	LangChoice.h \
	Languages.cpp \
	Languages.h \
	Legacy.cpp \
	Legacy.h \
	Lyrics.cpp \
	Lyrics.h \
	LyricsWindow.cpp \
	LyricsWindow.h \
	MacroMagic.h \
	Matrix.cpp \
	Matrix.h \
	MemoryX.h \
	Menus.cpp \
	Menus.h \
	Mix.cpp \
	Mix.h \
	MixerBoard.cpp \
	MixerBoard.h \
	ModuleManager.cpp \
	ModuleManager.h \
        NumberScale.h \
	PitchName.cpp \
	PitchName.h \
	PlatformCompatibility.cpp \
	PlatformCompatibility.h \
	PluginManager.cpp \
	PluginManager.h \
	Printing.cpp \
	Printing.h \
	Profiler.cpp \
	Profiler.h \
	Project.cpp \
	Project.h \
	RealFFTf.cpp \
	RealFFTf.h \
	RealFFTf48x.cpp \
	RealFFTf48x.h \
	Resample.cpp \
	Resample.h \
	RevisionIdent.h \
	RingBuffer.cpp \
	RingBuffer.h \
	Screenshot.cpp \
	Screenshot.h \
	SelectedRegion.cpp \
	SelectedRegion.h \
	Shuttle.cpp \
	Shuttle.h \
	ShuttleGui.cpp \
	ShuttleGui.h \
	ShuttlePrefs.cpp \
	ShuttlePrefs.h \
	Snap.cpp \
	Snap.h \
	SoundActivatedRecord.cpp \
	SoundActivatedRecord.h \
	Spectrum.cpp \
	Spectrum.h \
	SplashDialog.cpp \
	SplashDialog.h \
	SseMathFuncs.cpp \
	SseMathFuncs.h \
	Tags.cpp \
	Tags.h \
	Theme.cpp \
	Theme.h \
	ThemeAsCeeCode.h \
	TimeDialog.cpp \
	TimeDialog.h \
	TimerRecordDialog.cpp \
	TimerRecordDialog.h \
	TimeTrack.cpp \
	TimeTrack.h \
	Track.cpp \
	Track.h \
	TrackArtist.cpp \
	TrackArtist.h \
	TrackPanel.cpp \
	TrackPanel.h \
	TrackPanelAx.cpp \
	TrackPanelAx.h \
	TrackPanelCell.h \
	TrackPanelCellIterator.h \
	TrackPanelListener.h \
	TranslatableStringArray.h \
	UndoManager.cpp \
	UndoManager.h \
	ViewInfo.cpp \
	ViewInfo.h \
	VoiceKey.cpp \
	VoiceKey.h \
	WaveClip.cpp \
	WaveClip.h \
	WaveTrack.cpp \
	WaveTrack.h \
	WaveTrackLocation.h \
	WrappedType.cpp \
	WrappedType.h \
	wxFileNameWrapper.h \
	commands/AppCommandEvent.cpp \
	commands/AppCommandEvent.h \
	commands/BatchEvalCommand.cpp \
	commands/BatchEvalCommand.h \
	commands/Command.cpp \
	commands/Command.h \
	commands/CommandBuilder.cpp \
	commands/CommandBuilder.h \
	commands/CommandDirectory.cpp \
	commands/CommandDirectory.h \
	commands/CommandFunctors.h \
	commands/CommandHandler.cpp \
	commands/CommandHandler.h \
	commands/CommandManager.cpp \
	commands/CommandManager.h \
	commands/CommandMisc.h \
	commands/CommandSignature.cpp \
	commands/CommandSignature.h \
	commands/CommandTargets.h \
	commands/CommandType.cpp \
	commands/CommandType.h \
	commands/CompareAudioCommand.cpp \
	commands/CompareAudioCommand.h \
	commands/ExecMenuCommand.cpp \
	commands/ExecMenuCommand.h \
	commands/GetAllMenuCommands.cpp \
	commands/GetAllMenuCommands.h \
	commands/GetProjectInfoCommand.cpp \
	commands/GetProjectInfoCommand.h \
	commands/GetTrackInfoCommand.cpp \
	commands/GetTrackInfoCommand.h \
	commands/HelpCommand.cpp \
	commands/HelpCommand.h \
	commands/ImportExportCommands.cpp \
	commands/ImportExportCommands.h \
	commands/Keyboard.cpp \
	commands/Keyboard.h \
	commands/MessageCommand.cpp \
	commands/MessageCommand.h \
	commands/OpenSaveCommands.cpp \
	commands/OpenSaveCommands.h \
	commands/PreferenceCommands.cpp \
	commands/PreferenceCommands.h \
	commands/ResponseQueue.cpp \
	commands/ResponseQueue.h \
	commands/ScreenshotCommand.cpp \
	commands/ScreenshotCommand.h \
	commands/ScriptCommandRelay.cpp \
	commands/ScriptCommandRelay.h \
	commands/SelectCommand.cpp \
	commands/SelectCommand.h \
	commands/SetProjectInfoCommand.cpp \
	commands/SetProjectInfoCommand.h \
	commands/SetTrackInfoCommand.cpp \
	commands/SetTrackInfoCommand.h \
	commands/Validators.h \
	effects/Amplify.cpp \
	effects/Amplify.h \
	effects/AutoDuck.cpp \
	effects/AutoDuck.h \
	effects/BassTreble.cpp \
	effects/BassTreble.h \
	effects/Biquad.cpp \
	effects/Biquad.h \
	effects/ChangePitch.cpp \
	effects/ChangePitch.h \
	effects/ChangeSpeed.cpp \
	effects/ChangeSpeed.h \
	effects/ChangeTempo.cpp \
	effects/ChangeTempo.h \
	effects/ClickRemoval.cpp \
	effects/ClickRemoval.h \
	effects/Compressor.cpp \
	effects/Compressor.h \
	effects/Contrast.cpp \
	effects/Contrast.h \
	effects/Distortion.cpp \
	effects/Distortion.h \
	effects/DtmfGen.cpp \
	effects/DtmfGen.h \
	effects/Echo.cpp \
	effects/Echo.h \
	effects/Effect.cpp \
	effects/Effect.h \
	effects/EffectManager.cpp \
	effects/EffectManager.h \
	effects/EffectRack.cpp \
	effects/EffectRack.h \
	effects/Equalization.cpp \
	effects/Equalization.h \
	effects/Equalization48x.cpp \
	effects/Equalization48x.h \
	effects/Fade.cpp \
	effects/Fade.h \
	effects/FindClipping.cpp \
	effects/FindClipping.h \
	effects/Generator.cpp \
	effects/Generator.h \
	effects/Invert.cpp \
	effects/Invert.h \
	effects/LoadEffects.cpp \
	effects/LoadEffects.h \
	effects/Noise.cpp \
	effects/Noise.h \
	effects/NoiseReduction.cpp \
	effects/NoiseReduction.h \
	effects/NoiseRemoval.cpp \
	effects/NoiseRemoval.h \
	effects/Normalize.cpp \
	effects/Normalize.h \
	effects/Paulstretch.cpp \
	effects/Paulstretch.h \
	effects/Phaser.cpp \
	effects/Phaser.h \
	effects/Repair.cpp \
	effects/Repair.h \
	effects/Repeat.cpp \
	effects/Repeat.h \
	effects/Reverb.cpp \
	effects/Reverb.h \
	effects/Reverb_libSoX.h \
	effects/Reverse.cpp \
	effects/Reverse.h \
	effects/SBSMSEffect.cpp \
	effects/SBSMSEffect.h \
	effects/ScienFilter.cpp \
	effects/ScienFilter.h \
	effects/Silence.cpp \
	effects/Silence.h \
	effects/SimpleMono.cpp \
	effects/SimpleMono.h \
	effects/SoundTouchEffect.cpp \
	effects/SoundTouchEffect.h \
	effects/StereoToMono.cpp \
	effects/StereoToMono.h \
	effects/TimeScale.cpp \
	effects/TimeScale.h \
	effects/TimeWarper.cpp \
	effects/TimeWarper.h \
	effects/ToneGen.cpp \
	effects/ToneGen.h \
	effects/TruncSilence.cpp \
	effects/TruncSilence.h \
	effects/TwoPassSimpleMono.cpp \
	effects/TwoPassSimpleMono.h \
	effects/Wahwah.cpp \
	effects/Wahwah.h \
	export/Export.cpp \
	export/Export.h \
	export/ExportCL.cpp \
	export/ExportCL.h \
	export/ExportFLAC.cpp \
	export/ExportFLAC.h \
	export/ExportMP2.cpp \
	export/ExportMP2.h \
	export/ExportMP3.cpp \
	export/ExportMP3.h \
	export/ExportMultiple.cpp \
	export/ExportMultiple.h \
	export/ExportOGG.cpp \
	export/ExportOGG.h \
	export/ExportPCM.cpp \
	export/ExportPCM.h \
	import/Import.cpp \
	import/Import.h \
	import/ImportFLAC.cpp \
	import/ImportFLAC.h \
	import/ImportForwards.h \
	import/ImportLOF.cpp \
	import/ImportLOF.h \
	import/ImportMP3.cpp \
	import/ImportMP3.h \
	import/ImportOGG.cpp \
	import/ImportOGG.h \
	import/ImportPCM.cpp \
	import/ImportPCM.h \
	import/ImportPlugin.h \
	import/ImportRaw.cpp \
	import/ImportRaw.h \
	import/RawAudioGuess.cpp \
	import/RawAudioGuess.h \
	import/FormatClassifier.cpp \
	import/FormatClassifier.h \
	import/MultiFormatReader.cpp \
	import/MultiFormatReader.h \
	import/SpecPowerMeter.cpp \
	import/SpecPowerMeter.h \
	ondemand/ODComputeSummaryTask.cpp \
	ondemand/ODComputeSummaryTask.h \
	ondemand/ODDecodeFFmpegTask.cpp \
	ondemand/ODDecodeFFmpegTask.h \
	ondemand/ODDecodeTask.cpp \
	ondemand/ODDecodeTask.h \
	ondemand/ODManager.cpp \
	ondemand/ODManager.h \
	ondemand/ODTask.cpp \
	ondemand/ODTask.h \
	ondemand/ODTaskThread.cpp \
	ondemand/ODTaskThread.h \
	ondemand/ODWaveTrackTaskQueue.cpp \
	ondemand/ODWaveTrackTaskQueue.h \
	prefs/BatchPrefs.cpp \
	prefs/BatchPrefs.h \
	prefs/DevicePrefs.cpp \
	prefs/DevicePrefs.h \
	prefs/DirectoriesPrefs.cpp \
	prefs/DirectoriesPrefs.h \
	prefs/EffectsPrefs.cpp \
	prefs/EffectsPrefs.h \
	prefs/ExtImportPrefs.cpp \
	prefs/ExtImportPrefs.h \
	prefs/GUIPrefs.cpp \
	prefs/GUIPrefs.h \
	prefs/GUISettings.h \
	prefs/ImportExportPrefs.cpp \
	prefs/ImportExportPrefs.h \
	prefs/KeyConfigPrefs.cpp \
	prefs/KeyConfigPrefs.h \
	prefs/LibraryPrefs.cpp \
	prefs/LibraryPrefs.h \
	prefs/MidiIOPrefs.cpp \
	prefs/MidiIOPrefs.h \
	prefs/ModulePrefs.cpp \
	prefs/ModulePrefs.h \
	prefs/MousePrefs.cpp \
	prefs/MousePrefs.h \
	prefs/PlaybackPrefs.cpp \
	prefs/PlaybackPrefs.h \
	prefs/PrefsDialog.cpp \
	prefs/PrefsDialog.h \
	prefs/PrefsPanel.h \
	prefs/ProjectsPrefs.cpp \
	prefs/ProjectsPrefs.h \
	prefs/QualityPrefs.cpp \
	prefs/QualityPrefs.h \
	prefs/RecordingPrefs.cpp \
	prefs/RecordingPrefs.h \
	prefs/SpectrogramSettings.cpp \
	prefs/SpectrogramSettings.h \
	prefs/SpectrumPrefs.cpp \
	prefs/SpectrumPrefs.h \
	prefs/ThemePrefs.cpp \
	prefs/ThemePrefs.h \
	prefs/TracksPrefs.cpp \
	prefs/TracksPrefs.h \
	prefs/WarningsPrefs.cpp \
	prefs/WarningsPrefs.h \
	prefs/WaveformPrefs.cpp \
	prefs/WaveformPrefs.h \
	prefs/WaveformSettings.cpp \
	prefs/WaveformSettings.h \
	toolbars/ControlToolBar.cpp \
	toolbars/ControlToolBar.h \
	toolbars/DeviceToolBar.cpp \
	toolbars/DeviceToolBar.h \
	toolbars/EditToolBar.cpp \
	toolbars/EditToolBar.h \
	toolbars/MeterToolBar.cpp \
	toolbars/MeterToolBar.h \
	toolbars/MixerToolBar.cpp \
	toolbars/MixerToolBar.h \
	toolbars/ScrubbingToolBar.cpp \
	toolbars/ScrubbingToolBar.h \
	toolbars/SelectionBar.cpp \
	toolbars/SelectionBar.h \
	toolbars/SpectralSelectionBar.cpp \
	toolbars/SpectralSelectionBar.h \
	toolbars/SpectralSelectionBarListener.h \
	toolbars/ToolBar.cpp \
	toolbars/ToolBar.h \
	toolbars/ToolDock.cpp \
	toolbars/ToolDock.h \
	toolbars/ToolManager.cpp \
	toolbars/ToolManager.h \
	toolbars/ToolsToolBar.cpp \
	toolbars/ToolsToolBar.h \
	toolbars/TranscriptionToolBar.cpp \
	toolbars/TranscriptionToolBar.h \
	tracks/ui/EditCursorOverlay.cpp \
	tracks/ui/EditCursorOverlay.h \
	tracks/ui/PlayIndicatorOverlay.cpp \
	tracks/ui/PlayIndicatorOverlay.h \
	tracks/ui/Scrubbing.cpp \
	tracks/ui/Scrubbing.h \
	widgets/AButton.cpp \
	widgets/AButton.h \
	widgets/ASlider.cpp \
	widgets/ASlider.h \
	widgets/AttachableScrollBar.cpp \
	widgets/AttachableScrollBar.h \
	widgets/BackedPanel.cpp \
	widgets/BackedPanel.h \
	widgets/ErrorDialog.cpp \
	widgets/ErrorDialog.h \
	widgets/ExpandingToolBar.cpp \
	widgets/ExpandingToolBar.h \
	widgets/FileHistory.cpp \
	widgets/FileHistory.h \
	widgets/Grabber.cpp \
	widgets/Grabber.h \
	widgets/Grid.cpp \
	widgets/Grid.h \
	widgets/HelpSystem.cpp \
	widgets/HelpSystem.h \
	widgets/HtmlWindow.cpp \
	widgets/HtmlWindow.h \
	widgets/ImageRoll.cpp \
	widgets/ImageRoll.h \
	widgets/KeyView.cpp \
	widgets/KeyView.h \
	widgets/LinkingHtmlWindow.cpp \
	widgets/LinkingHtmlWindow.h \
	widgets/Meter.cpp \
	widgets/Meter.h \
	widgets/MultiDialog.cpp \
	widgets/MultiDialog.h \
	widgets/NumericTextCtrl.cpp \
	widgets/NumericTextCtrl.h \
	widgets/numformatter.cpp \
	widgets/numformatter.h \
	widgets/Overlay.cpp \
	widgets/Overlay.h \
	widgets/OverlayPanel.cpp \
	widgets/OverlayPanel.h \
	widgets/ProgressDialog.cpp \
	widgets/ProgressDialog.h \
	widgets/Ruler.cpp \
	widgets/Ruler.h \
	widgets/valnum.cpp \
	widgets/valnum.h \
	widgets/Warning.cpp \
	widgets/Warning.h \
	widgets/wxPanelWrapper.cpp \
	widgets/wxPanelWrapper.h \
	xml/XMLFileReader.cpp \
	xml/XMLFileReader.h \
	xml/XMLWriter.cpp \
	xml/XMLWriter.h \
	$(NULL)

if USE_AUDIO_UNITS
audacity_CPPFLAGS += $(AUDIOUNITS_CFLAGS)
audacity_LDADD += $(AUDIOUNITS_LIBS)
audacity_SOURCES += \
	effects/audiounits/AudioUnitEffect.cpp \
	effects/audiounits/AudioUnitEffect.h \
	$(NULL)
endif

if USE_FFMPEG
audacity_CPPFLAGS += $(FFMPEG_CFLAGS)
audacity_LDADD += $(FFMPEG_LIBS)
audacity_SOURCES += \
	export/ExportFFmpeg.cpp \
	export/ExportFFmpeg.h \
	export/ExportFFmpegDialogs.cpp \
	export/ExportFFmpegDialogs.h \
	import/ImportFFmpeg.cpp \
	import/ImportFFmpeg.h \
	$(NULL)
endif

if USE_GSTREAMER
audacity_CPPFLAGS += $(GSTREAMER_CFLAGS)
audacity_LDADD += $(GSTREAMER_LIBS)
audacity_SOURCES += \
	import/ImportGStreamer.cpp \
	import/ImportGStreamer.h \
	$(NULL)
endif

if USE_LADSPA
audacity_CPPFLAGS += $(LADSPA_CFLAGS)
audacity_LDADD += $(LADSPA_LIBS)
audacity_SOURCES += \
	effects/ladspa/ladspa.h \
	effects/ladspa/LadspaEffect.cpp \
	effects/ladspa/LadspaEffect.h \
	$(NULL)
endif

if USE_LAME
audacity_CPPFLAGS += $(LAME_CFLAGS)
audacity_LDADD += $(LAME_LIBS)
endif

if USE_LIBFLAC
audacity_CPPFLAGS += $(FLAC_CFLAGS)
audacity_LDADD += $(FLAC_LIBS)
audacity_SOURCES += \
	ondemand/ODDecodeFlacTask.cpp \
	ondemand/ODDecodeFlacTask.h \
	$(NULL)
endif

if USE_LIBID3TAG
audacity_CPPFLAGS += $(ID3TAG_CFLAGS)
audacity_LDADD += $(ID3TAG_LIBS)
endif

if USE_LIBMAD
audacity_CPPFLAGS += $(LIBMAD_CFLAGS)
audacity_LDADD += $(LIBMAD_LIBS)
endif

if USE_LIBNYQUIST
audacity_CPPFLAGS += $(LIBNYQUIST_CFLAGS)
audacity_LDADD += $(LIBNYQUIST_LIBS)
audacity_SOURCES += \
	effects/nyquist/LoadNyquist.cpp \
	effects/nyquist/LoadNyquist.h \
	effects/nyquist/Nyquist.cpp \
	effects/nyquist/Nyquist.h \
	$(NULL)
endif

if USE_LIBSOUNDTOUCH
audacity_CPPFLAGS += $(SOUNDTOUCH_CFLAGS)
audacity_LDADD += $(SOUNDTOUCH_LIBS)
endif

if USE_LIBTWOLAME
audacity_CPPFLAGS += $(LIBTWOLAME_CFLAGS)
audacity_LDADD += $(LIBTWOLAME_LIBS)
endif

if USE_LIBVORBIS
audacity_CPPFLAGS += $(LIBVORBIS_CFLAGS)
audacity_LDADD += $(LIBVORBIS_LIBS)
endif

if USE_LV2
audacity_CPPFLAGS += $(LV2_CFLAGS)
audacity_LDADD += $(LV2_LIBS)
audacity_SOURCES += \
	effects/lv2/LoadLV2.cpp \
	effects/lv2/LoadLV2.h \
	effects/lv2/LV2Effect.cpp \
	effects/lv2/LV2Effect.h \
	$(NULL)
endif

if USE_PORTSMF
audacity_CPPFLAGS += $(PORTSMF_CFLAGS)
audacity_LDADD += $(PORTSMF_LIBS)
audacity_SOURCES += \
	NoteTrack.cpp \
	NoteTrack.h \
	import/ImportMIDI.cpp \
	import/ImportMIDI.h \
	$(NULL)
endif

if USE_QUICKTIME
audacity_CPPFLAGS += $(QUICKTIME_CFLAGS)
audacity_LDADD += $(QUICKTIME_LIBS)
audacity_SOURCES += \
	import/ImportQT.cpp \
	import/ImportQT.h \
	$(NULL)
endif

if USE_SBSMS
audacity_CPPFLAGS += $(SBSMS_CFLAGS)
audacity_LDADD += $(SBSMS_LIBS)
endif

if USE_VAMP
audacity_CPPFLAGS += $(VAMP_CFLAGS)
audacity_LDADD += $(VAMP_LIBS)
audacity_SOURCES += \
	effects/vamp/LoadVamp.cpp \
	effects/vamp/LoadVamp.h \
	effects/vamp/VampEffect.cpp \
	effects/vamp/VampEffect.h \
	$(NULL)
endif

if USE_VST
audacity_CPPFLAGS += $(VST_CFLAGS)
audacity_LDADD += $(VST_LIBS)
audacity_SOURCES += \
	effects/VST/aeffectx.h \
	effects/VST/VSTEffect.cpp \
	effects/VST/VSTEffect.h \
	effects/VST/VSTControlGTK.cpp \
	effects/VST/VSTControlGTK.h \
	$(NULL)
endif

# TODO: Check *.cpp and *.h files if they are needed.
EXTRA_DIST = audacity.desktop.in xml/audacityproject.dtd \
	AudacityHeaders.cpp \
	AudacityHeaders.h \
	CrossFade.cpp \
	CrossFade.h \
	effects/ScoreAlignDialog.cpp \
	effects/ScoreAlignDialog.h \
	$(NULL)
