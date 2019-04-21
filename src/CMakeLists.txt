#directory src
project (Audacity)
find_package(wxWidgets REQUIRED COMPONENTS net core base html qa adv)
include(${wxWidgets_USE_FILE})

set_property(GLOBAL PROPERTY USE_FOLDERS On) 


# Needed for configwin.h included by Audacity.h
include_directories(${top_dir}/win)

#include_directories(${top_dir}/lib-src/)
include_directories(${top_dir}/include)
include_directories(${top_dir}/src) # for AudacityHeaders.h
include_directories(${top_dir}/lib-src/expat/lib)
include_directories(${top_dir}/lib-src/FileDialog)
include_directories(${top_dir}/lib-src/FileDialog/win)
include_directories(${top_dir}/lib-src/ffmpeg)
include_directories(${top_dir}/lib-src/ffmpeg/win32)
include_directories(${top_dir}/lib-src/lib-widget-extra)
include_directories(${top_dir}/lib-src/libid3tag) #for id3tag.h
include_directories(${top_dir}/lib-src/libmad/msvc++)
include_directories(${top_dir}/lib-src/libmad)
include_directories(${top_dir}/lib-src/libnyquist)
include_directories(${top_dir}/lib-src/libogg/include)
include_directories(${top_dir}/lib-src/libscorealign)
include_directories(${top_dir}/win/Projects/libsndfile) 
include_directories(${top_dir}/lib-src/libsoxr/src) #really?  Src?
include_directories(${top_dir}/lib-src/libvamp)
include_directories(${top_dir}/lib-src/libvorbis/include)
include_directories(${top_dir}/lib-src/lib-widget-extra)
include_directories(${top_dir}/lib-src/portmixer/include)
include_directories(${top_dir}/lib-src/portsmf)
include_directories(${top_dir}/lib-src/sbsms/include)
include_directories(${top_dir}/lib-src/soundtouch/include)
include_directories(${top_dir}/lib-src/twolame/libtwolame)
include_directories(${top_dir}/lib-src/portmidi/pm_common)
include_directories(${top_dir}/lib-src/portmidi/porttime)
include_directories(${top_dir}/lib-src/lv2/lilv)
include_directories(${top_dir}/lib-src/lv2/lv2)
include_directories(${top_dir}/lib-src/lv2/suil)
include_directories(${top_dir}/lib-src/lame)

#if(MSVC)
    add_definitions(/FI"AudacityHeaders.h")
#else()
    # GCC or Clang
#    add_definitions(-include AudacityHeaders.h)
#endif()




# Consider moving many of these # defines to Audacity.h.
# disable warning of insecure functions like getEnv
add_definitions( -D_CRT_SECURE_NO_WARNINGS )

add_definitions( -DBUILDING_AUDACITY )
add_definitions( -DXML_STATIC )
add_definitions( -D__STDC_CONSTANT_MACROS )
add_definitions( -DWXINTL_NO_GETTEXT_MACRO )
add_definitions( -DWXUSINGDLL )
add_definitions( -D__WXMSW__ )
add_definitions( -DwxDEBUG_LEVEL=0 )
add_definitions( -DNDEBUG )
add_definitions( -DWIN32 )
add_definitions( -DSTRICT )




# disable warning of deprecated features.
# NB we may want this ON for better wx code style.
add_compile_options(/wd4996)




set( SOURCE 
   ${CMAKE_SOURCE_DIRECTORY}AudacityHeaders.cpp  #first so cotire can precompile
   ${CMAKE_SOURCE_DIRECTORY}AboutDialog.cpp
   ${CMAKE_SOURCE_DIRECTORY}AColor.cpp
   ${CMAKE_SOURCE_DIRECTORY}AudacityApp.cpp
   ${CMAKE_SOURCE_DIRECTORY}AudacityException.cpp
   ${CMAKE_SOURCE_DIRECTORY}AudacityLogger.cpp
   ${CMAKE_SOURCE_DIRECTORY}AudioIO.cpp
   ${CMAKE_SOURCE_DIRECTORY}AutoRecovery.cpp
   ${CMAKE_SOURCE_DIRECTORY}BatchCommandDialog.cpp
   ${CMAKE_SOURCE_DIRECTORY}BatchCommands.cpp
   ${CMAKE_SOURCE_DIRECTORY}BatchProcessDialog.cpp
   ${CMAKE_SOURCE_DIRECTORY}Benchmark.cpp
   ${CMAKE_SOURCE_DIRECTORY}BlockFile.cpp
   #${CMAKE_SOURCE_DIRECTORY}CrossFade.cpp # abandoned code.
   ${CMAKE_SOURCE_DIRECTORY}Dependencies.cpp
   ${CMAKE_SOURCE_DIRECTORY}DeviceChange.cpp
   ${CMAKE_SOURCE_DIRECTORY}DeviceManager.cpp
   ${CMAKE_SOURCE_DIRECTORY}Diags.cpp
   ${CMAKE_SOURCE_DIRECTORY}DirManager.cpp
   ${CMAKE_SOURCE_DIRECTORY}Dither.cpp
   ${CMAKE_SOURCE_DIRECTORY}Envelope.cpp
   ${CMAKE_SOURCE_DIRECTORY}FFmpeg.cpp
   ${CMAKE_SOURCE_DIRECTORY}FFT.cpp
   ${CMAKE_SOURCE_DIRECTORY}FileException.cpp
   ${CMAKE_SOURCE_DIRECTORY}FileFormats.cpp
   ${CMAKE_SOURCE_DIRECTORY}FileIO.cpp
   ${CMAKE_SOURCE_DIRECTORY}FileNames.cpp
   ${CMAKE_SOURCE_DIRECTORY}FreqWindow.cpp
   ${CMAKE_SOURCE_DIRECTORY}HelpText.cpp
   ${CMAKE_SOURCE_DIRECTORY}HistoryWindow.cpp
   ${CMAKE_SOURCE_DIRECTORY}ImageManipulation.cpp
   ${CMAKE_SOURCE_DIRECTORY}InconsistencyException.cpp
   ${CMAKE_SOURCE_DIRECTORY}Internat.cpp
   ${CMAKE_SOURCE_DIRECTORY}InterpolateAudio.cpp
   ${CMAKE_SOURCE_DIRECTORY}LabelDialog.cpp
   ${CMAKE_SOURCE_DIRECTORY}LabelTrack.cpp
   ${CMAKE_SOURCE_DIRECTORY}LangChoice.cpp
   ${CMAKE_SOURCE_DIRECTORY}Languages.cpp
   ${CMAKE_SOURCE_DIRECTORY}Legacy.cpp
   ${CMAKE_SOURCE_DIRECTORY}Lyrics.cpp
   ${CMAKE_SOURCE_DIRECTORY}LyricsWindow.cpp
   ${CMAKE_SOURCE_DIRECTORY}Matrix.cpp
   ${CMAKE_SOURCE_DIRECTORY}Menus.cpp
   ${CMAKE_SOURCE_DIRECTORY}#MenusMac.cpp   # Not wanted on Windows.
   ${CMAKE_SOURCE_DIRECTORY}Mix.cpp
   ${CMAKE_SOURCE_DIRECTORY}MixerBoard.cpp
   ${CMAKE_SOURCE_DIRECTORY}ModuleManager.cpp
   ${CMAKE_SOURCE_DIRECTORY}NoteTrack.cpp
   ${CMAKE_SOURCE_DIRECTORY}PitchName.cpp
   ${CMAKE_SOURCE_DIRECTORY}PlatformCompatibility.cpp
   ${CMAKE_SOURCE_DIRECTORY}PluginManager.cpp
   ${CMAKE_SOURCE_DIRECTORY}Prefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}Printing.cpp
   ${CMAKE_SOURCE_DIRECTORY}Profiler.cpp
   ${CMAKE_SOURCE_DIRECTORY}Project.cpp
   ${CMAKE_SOURCE_DIRECTORY}RealFFTf.cpp
   ${CMAKE_SOURCE_DIRECTORY}RealFFTf48x.cpp
   ${CMAKE_SOURCE_DIRECTORY}Resample.cpp
   ${CMAKE_SOURCE_DIRECTORY}RingBuffer.cpp
   ${CMAKE_SOURCE_DIRECTORY}SampleFormat.cpp
   ${CMAKE_SOURCE_DIRECTORY}Screenshot.cpp
   ${CMAKE_SOURCE_DIRECTORY}SelectedRegion.cpp
   ${CMAKE_SOURCE_DIRECTORY}SelectionState.cpp
   ${CMAKE_SOURCE_DIRECTORY}Sequence.cpp
   ${CMAKE_SOURCE_DIRECTORY}Shuttle.cpp
   ${CMAKE_SOURCE_DIRECTORY}ShuttleGui.cpp
   ${CMAKE_SOURCE_DIRECTORY}ShuttlePrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}Snap.cpp
   ${CMAKE_SOURCE_DIRECTORY}SoundActivatedRecord.cpp
   ${CMAKE_SOURCE_DIRECTORY}Spectrum.cpp
   ${CMAKE_SOURCE_DIRECTORY}SplashDialog.cpp
   ${CMAKE_SOURCE_DIRECTORY}SseMathFuncs.cpp
   ${CMAKE_SOURCE_DIRECTORY}Tags.cpp
   ${CMAKE_SOURCE_DIRECTORY}Theme.cpp
   ${CMAKE_SOURCE_DIRECTORY}TimeDialog.cpp
   ${CMAKE_SOURCE_DIRECTORY}TimerRecordDialog.cpp
   ${CMAKE_SOURCE_DIRECTORY}TimeTrack.cpp
   ${CMAKE_SOURCE_DIRECTORY}Track.cpp
   ${CMAKE_SOURCE_DIRECTORY}TrackArtist.cpp
   ${CMAKE_SOURCE_DIRECTORY}TrackPanel.cpp
   ${CMAKE_SOURCE_DIRECTORY}TrackPanelAx.cpp
   ${CMAKE_SOURCE_DIRECTORY}TrackPanelResizeHandle.cpp
   ${CMAKE_SOURCE_DIRECTORY}TrackPanelResizerCell.cpp
   ${CMAKE_SOURCE_DIRECTORY}UIHandle.cpp
   ${CMAKE_SOURCE_DIRECTORY}UndoManager.cpp
   ${CMAKE_SOURCE_DIRECTORY}UserException.cpp
   ${CMAKE_SOURCE_DIRECTORY}ViewInfo.cpp
   ${CMAKE_SOURCE_DIRECTORY}VoiceKey.cpp
   ${CMAKE_SOURCE_DIRECTORY}WaveClip.cpp
   ${CMAKE_SOURCE_DIRECTORY}WaveTrack.cpp
   ${CMAKE_SOURCE_DIRECTORY}WrappedType.cpp
   #An anomoly - a source file plucked from a lib.
   ${top_dir}/lib-src/lib-widget-extra/NonGuiThread.cpp
)

# We're not yet adding each subdirectory.
# instead we do what we would do there here.
#add_subdirectory( blockfile )
set( BLOCKFILE_SOURCE
   ${CMAKE_SOURCE_DIRECTORY}blockfile/LegacyAliasBlockFile.cpp
   ${CMAKE_SOURCE_DIRECTORY}blockfile/LegacyBlockFile.cpp
   ${CMAKE_SOURCE_DIRECTORY}blockfile/NotYetAvailableException.cpp
   ${CMAKE_SOURCE_DIRECTORY}blockfile/ODDecodeBlockFile.cpp
   ${CMAKE_SOURCE_DIRECTORY}blockfile/ODPCMAliasBlockFile.cpp
   ${CMAKE_SOURCE_DIRECTORY}blockfile/PCMAliasBlockFile.cpp
   ${CMAKE_SOURCE_DIRECTORY}blockfile/SilentBlockFile.cpp
   ${CMAKE_SOURCE_DIRECTORY}blockfile/SimpleBlockFile.cpp
)   
source_group( blockfile FILES ${BLOCKFILE_SOURCE} )


#add_subdirectory( commands )
set( COMMANDS_SOURCE
   ${CMAKE_SOURCE_DIRECTORY}commands/AppCommandEvent.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/AudacityCommand.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/BatchEvalCommand.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/Command.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/CommandBuilder.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/CommandContext.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/CommandDirectory.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/CommandHandler.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/CommandManager.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/CommandSignature.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/CommandTargets.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/CommandType.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/CompareAudioCommand.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/Demo.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/DragCommand.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/GetInfoCommand.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/GetTrackInfoCommand.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/HelpCommand.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/ImportExportCommands.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/Keyboard.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/LoadCommands.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/MessageCommand.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/OpenSaveCommands.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/PreferenceCommands.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/ResponseQueue.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/ScreenshotCommand.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/ScriptCommandRelay.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/SelectCommand.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/SetClipCommand.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/SetEnvelopeCommand.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/SetLabelCommand.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/SetProjectCommand.cpp
   ${CMAKE_SOURCE_DIRECTORY}commands/SetTrackInfoCommand.cpp
)
source_group( commands FILES ${COMMANDS_SOURCE} )

#add_subdirectory( effects )
set( EFFECTS_SOURCE
   ${CMAKE_SOURCE_DIRECTORY}effects/Amplify.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/AutoDuck.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/BassTreble.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Biquad.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/ChangePitch.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/ChangeSpeed.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/ChangeTempo.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/ClickRemoval.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Compressor.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Contrast.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Distortion.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/DtmfGen.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Echo.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Effect.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/EffectManager.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/EffectRack.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Equalization.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Equalization48x.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Fade.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/FindClipping.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Generator.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Invert.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Leveller.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/LoadEffects.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Noise.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/NoiseReduction.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/NoiseRemoval.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Normalize.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Paulstretch.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Phaser.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Repair.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Repeat.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Reverb.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Reverse.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/SBSMSEffect.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/ScienFilter.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/ScoreAlignDialog.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Silence.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/SimpleMono.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/SoundTouchEffect.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/StereoToMono.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/TimeScale.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/TimeWarper.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/ToneGen.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/TruncSilence.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/TwoPassSimpleMono.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/Wahwah.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/audiounits/AudioUnitEffect.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/ladspa/LadspaEffect.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/lv2/LoadLV2.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/lv2/LV2Effect.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/nyquist/LoadNyquist.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/nyquist/Nyquist.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/vamp/LoadVamp.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/vamp/VampEffect.cpp
   #${CMAKE_SOURCE_DIRECTORY}effects/VST/VSTControlGTK.cpp #Not for Windows.
   ${CMAKE_SOURCE_DIRECTORY}effects/VST/VSTControlMSW.cpp
   ${CMAKE_SOURCE_DIRECTORY}effects/VST/VSTEffect.cpp
)
source_group( effects FILES ${EFFECTS_SOURCE} )

#add_subdirectory( export )
set( EXPORT_SOURCE
   ${CMAKE_SOURCE_DIRECTORY}export/Export.cpp
   ${CMAKE_SOURCE_DIRECTORY}export/ExportCL.cpp
   ${CMAKE_SOURCE_DIRECTORY}export/ExportFFmpeg.cpp
   ${CMAKE_SOURCE_DIRECTORY}export/ExportFFmpegDialogs.cpp
   ${CMAKE_SOURCE_DIRECTORY}export/ExportFLAC.cpp
   ${CMAKE_SOURCE_DIRECTORY}export/ExportMP2.cpp
   ${CMAKE_SOURCE_DIRECTORY}export/ExportMP3.cpp
   ${CMAKE_SOURCE_DIRECTORY}export/ExportMultiple.cpp
   ${CMAKE_SOURCE_DIRECTORY}export/ExportOGG.cpp
   ${CMAKE_SOURCE_DIRECTORY}export/ExportPCM.cpp
)
source_group( export FILES ${EXPORT_SOURCE} )

#add_subdirectory( import )
set( IMPORT_SOURCE
   ${CMAKE_SOURCE_DIRECTORY}import/FormatClassifier.cpp
   ${CMAKE_SOURCE_DIRECTORY}import/Import.cpp
   ${CMAKE_SOURCE_DIRECTORY}import/ImportFFmpeg.cpp
   ${CMAKE_SOURCE_DIRECTORY}import/ImportFLAC.cpp
   ${CMAKE_SOURCE_DIRECTORY}import/ImportGStreamer.cpp
   ${CMAKE_SOURCE_DIRECTORY}import/ImportLOF.cpp
   ${CMAKE_SOURCE_DIRECTORY}import/ImportMIDI.cpp
   ${CMAKE_SOURCE_DIRECTORY}import/ImportMP3.cpp
   ${CMAKE_SOURCE_DIRECTORY}import/ImportOGG.cpp
   ${CMAKE_SOURCE_DIRECTORY}import/ImportPCM.cpp
   ${CMAKE_SOURCE_DIRECTORY}import/ImportQT.cpp
   ${CMAKE_SOURCE_DIRECTORY}import/ImportRaw.cpp
   ${CMAKE_SOURCE_DIRECTORY}import/MultiFormatReader.cpp
   ${CMAKE_SOURCE_DIRECTORY}import/RawAudioGuess.cpp
   ${CMAKE_SOURCE_DIRECTORY}import/SpecPowerMeter.cpp
)
source_group( import FILES ${IMPORT_SOURCE} )

#add_subdirectory( ondemand )
set( ONDEMAND_SOURCE
   ${CMAKE_SOURCE_DIRECTORY}ondemand/ODComputeSummaryTask.cpp
   ${CMAKE_SOURCE_DIRECTORY}ondemand/ODDecodeFFmpegTask.cpp
   ${CMAKE_SOURCE_DIRECTORY}ondemand/ODDecodeFlacTask.cpp
   ${CMAKE_SOURCE_DIRECTORY}ondemand/ODDecodeTask.cpp
   ${CMAKE_SOURCE_DIRECTORY}ondemand/ODManager.cpp
   ${CMAKE_SOURCE_DIRECTORY}ondemand/ODTask.cpp
   ${CMAKE_SOURCE_DIRECTORY}ondemand/ODTaskThread.cpp
   ${CMAKE_SOURCE_DIRECTORY}ondemand/ODWaveTrackTaskQueue.cpp
)
source_group( ondemand FILES ${ONDEMAND_SOURCE} )

#add_subdirectory( prefs )
set( PREFS_SOURCE
   ${CMAKE_SOURCE_DIRECTORY}prefs/BatchPrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/DevicePrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/DirectoriesPrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/EffectsPrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/ExtImportPrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/GUIPrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/ImportExportPrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/KeyConfigPrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/LibraryPrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/MidiIOPrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/ModulePrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/MousePrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/PlaybackPrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/PrefsDialog.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/ProjectsPrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/QualityPrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/RecordingPrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/SpectrogramSettings.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/SpectrumPrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/ThemePrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/TracksBehaviorsPrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/TracksPrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/WarningsPrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/WaveformPrefs.cpp
   ${CMAKE_SOURCE_DIRECTORY}prefs/WaveformSettings.cpp
)
source_group( prefs FILES ${PREFS_SOURCE} )

#add_subdirectory( toolbars )
set( TOOLBARS_SOURCE
   ${CMAKE_SOURCE_DIRECTORY}toolbars/ControlToolBar.cpp
   ${CMAKE_SOURCE_DIRECTORY}toolbars/DeviceToolBar.cpp
   ${CMAKE_SOURCE_DIRECTORY}toolbars/EditToolBar.cpp
   ${CMAKE_SOURCE_DIRECTORY}toolbars/MeterToolBar.cpp
   ${CMAKE_SOURCE_DIRECTORY}toolbars/MixerToolBar.cpp
   ${CMAKE_SOURCE_DIRECTORY}toolbars/ScrubbingToolBar.cpp
   ${CMAKE_SOURCE_DIRECTORY}toolbars/SelectionBar.cpp
   ${CMAKE_SOURCE_DIRECTORY}toolbars/SpectralSelectionBar.cpp
   ${CMAKE_SOURCE_DIRECTORY}toolbars/ToolBar.cpp
   ${CMAKE_SOURCE_DIRECTORY}toolbars/ToolDock.cpp
   ${CMAKE_SOURCE_DIRECTORY}toolbars/ToolManager.cpp
   ${CMAKE_SOURCE_DIRECTORY}toolbars/ToolsToolBar.cpp
   ${CMAKE_SOURCE_DIRECTORY}toolbars/TranscriptionToolBar.cpp
)
source_group( toolbars FILES ${TOOLBARS_SOURCE} )

#add_subdirectory( tracks )
set( TRACKS_SOURCE
   ${CMAKE_SOURCE_DIRECTORY}tracks/labeltrack/ui/LabelDefaultClickHandle.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/labeltrack/ui/LabelGlyphHandle.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/labeltrack/ui/LabelTextHandle.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/labeltrack/ui/LabelTrackControls.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/labeltrack/ui/LabelTrackUI.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/labeltrack/ui/LabelTrackVRulerControls.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/playabletrack/notetrack/ui/NoteTrackButtonHandle.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/playabletrack/notetrack/ui/NoteTrackControls.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/playabletrack/notetrack/ui/NoteTrackSliderHandles.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/playabletrack/notetrack/ui/NoteTrackUI.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/playabletrack/notetrack/ui/NoteTrackVRulerControls.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/playabletrack/notetrack/ui/NoteTrackVZoomHandle.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/playabletrack/notetrack/ui/StretchHandle.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/playabletrack/ui/PlayableTrackButtonHandles.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/playabletrack/wavetrack/ui/CutlineHandle.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/playabletrack/wavetrack/ui/SampleHandle.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/playabletrack/wavetrack/ui/WaveTrackControls.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/playabletrack/wavetrack/ui/WaveTrackSliderHandles.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/playabletrack/wavetrack/ui/WaveTrackUI.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/playabletrack/wavetrack/ui/WaveTrackVRulerControls.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/playabletrack/wavetrack/ui/WaveTrackVZoomHandle.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/timetrack/ui/TimeTrackControls.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/timetrack/ui/TimeTrackUI.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/timetrack/ui/TimeTrackVRulerControls.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/ui/BackgroundCell.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/ui/ButtonHandle.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/ui/CommonTrackPanelCell.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/ui/EditCursorOverlay.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/ui/EnvelopeHandle.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/ui/PlayIndicatorOverlay.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/ui/Scrubbing.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/ui/SelectHandle.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/ui/SliderHandle.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/ui/TimeShiftHandle.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/ui/TrackButtonHandles.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/ui/TrackControls.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/ui/TrackSelectHandle.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/ui/TrackUI.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/ui/TrackVRulerControls.cpp
   ${CMAKE_SOURCE_DIRECTORY}tracks/ui/ZoomHandle.cpp
)
source_group( tracks FILES ${TRACKS_SOURCE} )

#add_subdirectory( widgets )
set( WIDGETS_SOURCE
   ${CMAKE_SOURCE_DIRECTORY}widgets/AButton.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/ASlider.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/AttachableScrollBar.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/BackedPanel.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/ErrorDialog.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/ExpandingToolBar.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/FileHistory.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/Grabber.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/Grid.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/HelpSystem.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/HtmlWindow.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/ImageRoll.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/KeyView.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/LinkingHtmlWindow.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/Meter.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/MultiDialog.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/NumericTextCtrl.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/numformatter.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/Overlay.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/OverlayPanel.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/PopupMenuTable.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/ProgressDialog.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/Ruler.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/valnum.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/Warning.cpp
   ${CMAKE_SOURCE_DIRECTORY}widgets/wxPanelWrapper.cpp
)
source_group( widgets FILES ${WIDGETS_SOURCE} )

#add_subdirectory( xml )
set( XML_SOURCE
   ${CMAKE_SOURCE_DIRECTORY}xml/XMLFileReader.cpp
   ${CMAKE_SOURCE_DIRECTORY}xml/XMLTagHandler.cpp
   ${CMAKE_SOURCE_DIRECTORY}xml/XMLWriter.cpp
)
source_group( xml FILES ${XML_SOURCE} )

set( SOURCE 
   ${SOURCE}
   ${BLOCKFILE_SOURCE}
   ${COMMANDS_SOURCE}
   ${EFFECTS_SOURCE}
   ${EXPORT_SOURCE}
   ${IMPORT_SOURCE}
   ${ONDEMAND_SOURCE}
   ${PREFS_SOURCE}
   ${TOOLBARS_SOURCE}
   ${TRACKS_SOURCE}
   ${WIDGETS_SOURCE}
   ${XML_SOURCE}
   )

add_executable( Audacity ${SOURCE} )

message( STATUS "link directory is: ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/Debug" )
message( STATUS "wxWidgets libraries in: ${wxWidgets_LIBRARIES}" )

target_link_libraries(Audacity
${wxWidgets_LIBRARIES}
expat
FileDialog
FLACXX-static
libsndfile
libsoxr
portmixer
portmidi-static
)
if(MSVC)
    set_property(TARGET Audacity APPEND PROPERTY LINK_FLAGS "/SUBSYSTEM:WINDOWS /LIBPATH:${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/Debug")
    #set_property(TARGET Audacity APPEND PROPERTY LINK_FLAGS )
endif()

#Use cotire to get precompiled headers, and a unity build.
set_target_properties(Audacity 
PROPERTIES COTIRE_CXX_PREFIX_HEADER_INIT 
"AudacityHeaders.h")
cotire(Audacity)

# (commented out) Debugging info for the curious about Cotire.
#[[

# cotire sets the following properties
get_target_property(_unitySource Audacity COTIRE_CXX_UNITY_SOURCE)
get_target_property(_prefixHeader Audacity COTIRE_CXX_PREFIX_HEADER)
get_target_property(_precompiledHeader Audacity COTIRE_CXX_PRECOMPILED_HEADER)
get_target_property(_unityTargetName Audacity COTIRE_UNITY_TARGET_NAME)

if (_unitySource)
	message(STATUS "Audacity unity source: ${_unitySource}")
endif()
if (_prefixHeader)
	message(STATUS "Audacity prefix header: ${_prefixHeader}")
endif()
if (_precompiledHeader)
	message(STATUS "Audacity precompiled header: ${_precompiledHeader}")
endif()
if (TARGET ${_unityTargetName})
	message(STATUS "Audacity unity target: ${_unityTargetName}")
endif()

]]#
