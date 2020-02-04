
set( TARGET Audacity )
set( TARGET_ROOT ${topdir}/src )

message( STATUS "========== Configuring ${TARGET} ==========" )

# Allow modules to link against the Audacity executable
set( CMAKE_ENABLE_EXPORTS ON )

add_executable( ${TARGET} )

def_vars()

#
#
#
list( APPEND SOURCES
   PRIVATE
      AColor.cpp
      AColor.h
      AboutDialog.cpp
      AboutDialog.h
      AdornedRulerPanel.cpp
      AdornedRulerPanel.h
      AllThemeResources.h
      AttachedVirtualFunction.h
      Audacity.h
      AudacityApp.cpp
      AudacityApp.h
      AudacityException.cpp
      AudacityException.h
      AudacityHeaders.cpp
      AudacityHeaders.h
      AudacityLogger.cpp
      AudacityLogger.h
      AudioIO.cpp
      AudioIO.h
      AudioIOBase.cpp
      AudioIOBase.h
      AudioIOListener.h
      AutoRecovery.cpp
      AutoRecovery.h
      AutoRecoveryDialog.cpp
      AutoRecoveryDialog.h
      BatchCommandDialog.cpp
      BatchCommandDialog.h
      BatchCommands.cpp
      BatchCommands.h
      BatchProcessDialog.cpp
      BatchProcessDialog.h
      Benchmark.cpp
      Benchmark.h
      BlockFile.cpp
      BlockFile.h
      CellularPanel.cpp
      CellularPanel.h
      ClassicThemeAsCeeCode.h
      ClientData.h
      ClientDataHelpers.h
      Clipboard.cpp
      Clipboard.h
      CommonCommandFlags.cpp
      CommonCommandFlags.h
      CrashReport.cpp
      CrashReport.h
      DarkThemeAsCeeCode.h
      Dependencies.cpp
      Dependencies.h
      DeviceChange.cpp
      DeviceChange.h
      DeviceManager.cpp
      DeviceManager.h
      Diags.cpp
      Diags.h
      DirManager.cpp
      DirManager.h
      Dither.cpp
      Dither.h
      Envelope.cpp
      Envelope.h
      EnvelopeEditor.cpp
      EnvelopeEditor.h
      Experimental.h
      FFT.cpp
      FFT.h
      FFmpeg.cpp
      FFmpeg.h
      FileException.cpp
      FileException.h
      FileFormats.cpp
      FileFormats.h
      FileIO.cpp
      FileIO.h
      FileNames.cpp
      FileNames.h
      FreqWindow.cpp
      FreqWindow.h
      HelpText.cpp
      HelpText.h
      HiContrastThemeAsCeeCode.h
      HistoryWindow.cpp
      HistoryWindow.h
      HitTestResult.h
      ImageManipulation.cpp
      ImageManipulation.h
      InconsistencyException.cpp
      InconsistencyException.h
      Internat.cpp
      Internat.h
      InterpolateAudio.cpp
      InterpolateAudio.h
      KeyboardCapture.cpp
      KeyboardCapture.h
      LabelDialog.cpp
      LabelDialog.h
      LabelTrack.cpp
      LabelTrack.h
      LangChoice.cpp
      LangChoice.h
      Languages.cpp
      Languages.h
      Legacy.cpp
      Legacy.h
      LightThemeAsCeeCode.h
      Lyrics.cpp
      Lyrics.h
      LyricsWindow.cpp
      LyricsWindow.h
      MacroMagic.h
      Makefile.am
      Makefile.in
      Matrix.cpp
      Matrix.h
      MemoryX.h
      Menus.cpp
      Menus.h
      MissingAliasFileDialog.cpp
      MissingAliasFileDialog.h
      Mix.cpp
      Mix.h
      MixerBoard.cpp
      MixerBoard.h
      ModuleManager.cpp
      ModuleManager.h
      NoteTrack.cpp
      NoteTrack.h
      NumberScale.h
      PitchName.cpp
      PitchName.h
      PlatformCompatibility.cpp
      PlatformCompatibility.h
      PluginManager.cpp
      PluginManager.h
      Prefs.cpp
      Prefs.h
      Printing.cpp
      Printing.h
      Profiler.cpp
      Profiler.h
      Project.cpp
      Project.h
      ProjectAudioIO.cpp
      ProjectAudioIO.h
      ProjectAudioManager.cpp
      ProjectAudioManager.h
      ProjectFSCK.cpp
      ProjectFSCK.h
      ProjectFileIO.cpp
      ProjectFileIO.h
      ProjectFileIORegistry.cpp
      ProjectFileIORegistry.h
      ProjectFileManager.cpp
      ProjectFileManager.h
      ProjectHistory.cpp
      ProjectHistory.h
      ProjectManager.cpp
      ProjectManager.h
      ProjectSelectionManager.cpp
      ProjectSelectionManager.h
      ProjectSettings.cpp
      ProjectSettings.h
      ProjectStatus.cpp
      ProjectStatus.h
      ProjectWindow.cpp
      ProjectWindow.h
      ProjectWindowBase.cpp
      ProjectWindowBase.h
      RealFFTf.cpp
      RealFFTf.h
      RealFFTf48x.cpp
      RealFFTf48x.h
      RefreshCode.h
      Registrar.h
      Resample.cpp
      Resample.h
      RevisionIdent.h
      RingBuffer.cpp
      RingBuffer.h
      SampleFormat.cpp
      SampleFormat.h
      Screenshot.cpp
      Screenshot.h
      SelectUtilities.cpp
      SelectUtilities.h
      SelectedRegion.cpp
      SelectedRegion.h
      SelectionState.cpp
      SelectionState.h
      Sequence.cpp
      Sequence.h
      Shuttle.cpp
      Shuttle.h
      ShuttleGetDefinition.cpp
      ShuttleGetDefinition.h
      ShuttleGui.cpp
      ShuttleGui.h
      ShuttlePrefs.cpp
      ShuttlePrefs.h
      Snap.cpp
      Snap.h
      SoundActivatedRecord.cpp
      SoundActivatedRecord.h
      Spectrum.cpp
      Spectrum.h
      SpectrumAnalyst.cpp
      SpectrumAnalyst.h
      SplashDialog.cpp
      SplashDialog.h
      SseMathFuncs.cpp
      SseMathFuncs.h
      Tags.cpp
      Tags.h
      Theme.cpp
      Theme.h
      ThemeAsCeeCode.h
      TimeDialog.cpp
      TimeDialog.h
      TimeTrack.cpp
      TimeTrack.h
      TimerRecordDialog.cpp
      TimerRecordDialog.h
      Track.cpp
      Track.h
      TrackArtist.cpp
      TrackArtist.h
      TrackInfo.cpp
      TrackInfo.h
      TrackPanel.cpp
      TrackPanel.h
      TrackPanelAx.cpp
      TrackPanelAx.h
      TrackPanelCell.h
      TrackPanelDrawable.h
      TrackPanelDrawingContext.h
      TrackPanelListener.h
      TrackPanelMouseEvent.h
      TrackPanelResizeHandle.cpp
      TrackPanelResizeHandle.h
      TrackPanelResizerCell.cpp
      TrackPanelResizerCell.h
      TrackUtilities.cpp
      TrackUtilities.h
      UIHandle.cpp
      UIHandle.h
      UndoManager.cpp
      UndoManager.h
      UserException.cpp
      UserException.h
      ViewInfo.cpp
      ViewInfo.h
      VoiceKey.cpp
      VoiceKey.h
      WaveClip.cpp
      WaveClip.h
      WaveTrack.cpp
      WaveTrack.h
      WaveTrackLocation.h
      WrappedType.cpp
      WrappedType.h
      ZoomInfo.cpp
      ZoomInfo.h
      float_cast.h
      wxFileNameWrapper.h

      # Blockfile

      blockfile/LegacyAliasBlockFile.cpp
      blockfile/LegacyAliasBlockFile.h
      blockfile/LegacyBlockFile.cpp
      blockfile/LegacyBlockFile.h
      blockfile/NotYetAvailableException.cpp
      blockfile/NotYetAvailableException.h
      blockfile/ODDecodeBlockFile.cpp
      blockfile/ODDecodeBlockFile.h
      blockfile/ODPCMAliasBlockFile.cpp
      blockfile/ODPCMAliasBlockFile.h
      blockfile/PCMAliasBlockFile.cpp
      blockfile/PCMAliasBlockFile.h
      blockfile/SilentBlockFile.cpp
      blockfile/SilentBlockFile.h
      blockfile/SimpleBlockFile.cpp
      blockfile/SimpleBlockFile.h

      # Commands

      commands/AppCommandEvent.cpp
      commands/AppCommandEvent.h
      commands/AudacityCommand.cpp
      commands/AudacityCommand.h
      commands/BatchEvalCommand.cpp
      commands/BatchEvalCommand.h
      commands/Command.cpp
      commands/Command.h
      commands/CommandBuilder.cpp
      commands/CommandBuilder.h
      commands/CommandContext.cpp
      commands/CommandContext.h
      commands/CommandDirectory.cpp
      commands/CommandDirectory.h
      commands/CommandFlag.h
      commands/CommandFunctors.h
      commands/CommandHandler.cpp
      commands/CommandHandler.h
      commands/CommandManager.cpp
      commands/CommandManager.h
      commands/CommandManagerWindowClasses.h
      commands/CommandMisc.h
      commands/CommandSignature.cpp
      commands/CommandSignature.h
      commands/CommandTargets.cpp
      commands/CommandTargets.h
      commands/CommandType.cpp
      commands/CommandType.h
      commands/CompareAudioCommand.cpp
      commands/CompareAudioCommand.h
      commands/Demo.cpp
      commands/Demo.h
      commands/DragCommand.cpp
      commands/DragCommand.h
      commands/GetInfoCommand.cpp
      commands/GetInfoCommand.h
      commands/GetTrackInfoCommand.cpp
      commands/GetTrackInfoCommand.h
      commands/HelpCommand.cpp
      commands/HelpCommand.h
      commands/ImportExportCommands.cpp
      commands/ImportExportCommands.h
      commands/Keyboard.cpp
      commands/Keyboard.h
      commands/LoadCommands.cpp
      commands/LoadCommands.h
      commands/MessageCommand.cpp
      commands/MessageCommand.h
      commands/OpenSaveCommands.cpp
      commands/OpenSaveCommands.h
      commands/PreferenceCommands.cpp
      commands/PreferenceCommands.h
      commands/ResponseQueue.cpp
      commands/ResponseQueue.h
      commands/ScreenshotCommand.cpp
      commands/ScreenshotCommand.h
      commands/ScriptCommandRelay.cpp
      commands/ScriptCommandRelay.h
      commands/SelectCommand.cpp
      commands/SelectCommand.h
      commands/SetClipCommand.cpp
      commands/SetClipCommand.h
      commands/SetEnvelopeCommand.cpp
      commands/SetEnvelopeCommand.h
      commands/SetLabelCommand.cpp
      commands/SetLabelCommand.h
      commands/SetProjectCommand.cpp
      commands/SetProjectCommand.h
      commands/SetTrackInfoCommand.cpp
      commands/SetTrackInfoCommand.h
      commands/Validators.h

      # Built-in Effects

      effects/Amplify.cpp
      effects/Amplify.h
      effects/AutoDuck.cpp
      effects/AutoDuck.h
      effects/BassTreble.cpp
      effects/BassTreble.h
      effects/Biquad.cpp
      effects/Biquad.h
      effects/ChangePitch.cpp
      effects/ChangePitch.h
      effects/ChangeSpeed.cpp
      effects/ChangeSpeed.h
      effects/ChangeTempo.cpp
      effects/ChangeTempo.h
      effects/ClickRemoval.cpp
      effects/ClickRemoval.h
      effects/Compressor.cpp
      effects/Compressor.h
      effects/Contrast.cpp
      effects/Contrast.h
      effects/Distortion.cpp
      effects/Distortion.h
      effects/DtmfGen.cpp
      effects/DtmfGen.h
      effects/EBUR128.cpp
      effects/EBUR128.h
      effects/Echo.cpp
      effects/Echo.h
      effects/Effect.cpp
      effects/Effect.h
      effects/EffectManager.cpp
      effects/EffectManager.h
      effects/EffectUI.cpp
      effects/EffectUI.h
      effects/Equalization.cpp
      effects/Equalization.h
      effects/Equalization48x.cpp
      effects/Equalization48x.h
      effects/Fade.cpp
      effects/Fade.h
      effects/FindClipping.cpp
      effects/FindClipping.h
      effects/Generator.cpp
      effects/Generator.h
      effects/Invert.cpp
      effects/Invert.h
      effects/LoadEffects.cpp
      effects/LoadEffects.h
      effects/Loudness.cpp
      effects/Loudness.h
      effects/Noise.cpp
      effects/Noise.h
      effects/NoiseReduction.cpp
      effects/NoiseReduction.h
      effects/NoiseRemoval.cpp
      effects/NoiseRemoval.h
      effects/Normalize.cpp
      effects/Normalize.h
      effects/Paulstretch.cpp
      effects/Paulstretch.h
      effects/Phaser.cpp
      effects/Phaser.h
      effects/RealtimeEffectManager.cpp
      effects/RealtimeEffectManager.h
      effects/Repair.cpp
      effects/Repair.h
      effects/Repeat.cpp
      effects/Repeat.h
      effects/Reverb.cpp
      effects/Reverb.h
      effects/Reverb_libSoX.h
      effects/Reverse.cpp
      effects/Reverse.h
      effects/SBSMSEffect.cpp
      effects/SBSMSEffect.h
      effects/ScienFilter.cpp
      effects/ScienFilter.h
      effects/ScoreAlignDialog.cpp
      effects/ScoreAlignDialog.h
      effects/Silence.cpp
      effects/Silence.h
      effects/SimpleMono.cpp
      effects/SimpleMono.h
      effects/SoundTouchEffect.cpp
      effects/SoundTouchEffect.h
      effects/StereoToMono.cpp
      effects/StereoToMono.h
      effects/TimeScale.cpp
      effects/TimeScale.h
      effects/TimeWarper.cpp
      effects/TimeWarper.h
      effects/ToneGen.cpp
      effects/ToneGen.h
      effects/TruncSilence.cpp
      effects/TruncSilence.h
      effects/TwoPassSimpleMono.cpp
      effects/TwoPassSimpleMono.h
      effects/Wahwah.cpp
      effects/Wahwah.h

      # VST Effects

      $<$<BOOL:USE_VST>:effects/VST/VSTControl.h>
      $<$<BOOL:USE_VST>:effects/VST/VSTEffect.cpp>
      $<$<BOOL:USE_VST>:effects/VST/VSTEffect.h>
      $<$<BOOL:USE_VST>:effects/VST/aeffectx.h>
      $<$<AND:$<BOOL:USE_VST>,$<PLATFORM_ID:Linux>>:effects/VST/VSTControlGTK.cpp>
      $<$<AND:$<BOOL:USE_VST>,$<PLATFORM_ID:Linux>>:effects/VST/VSTControlGTK.h>
      $<$<AND:$<BOOL:USE_VST>,$<PLATFORM_ID:Windows>>:effects/VST/VSTControlMSW.cpp>
      $<$<AND:$<BOOL:USE_VST>,$<PLATFORM_ID:Windows>>:effects/VST/VSTControlMSW.h>
      $<$<AND:$<BOOL:USE_VST>,$<PLATFORM_ID:Darwin>>:effects/VST/VSTControlOSX.h>
      $<$<AND:$<BOOL:USE_VST>,$<PLATFORM_ID:Darwin>>:effects/VST/VSTControlOSX.mm>

      # Audio Unit Effects

      $<$<AND:$<BOOL:USE_AUDIO_UNITS>,$<PLATFORM_ID:Darwin>>:effects/audiounits/AUControl.h>
      $<$<AND:$<BOOL:USE_AUDIO_UNITS>,$<PLATFORM_ID:Darwin>>:effects/audiounits/AUControl.mm>
      $<$<AND:$<BOOL:USE_AUDIO_UNITS>,$<PLATFORM_ID:Darwin>>:effects/audiounits/AudioUnitEffect.cpp>
      $<$<AND:$<BOOL:USE_AUDIO_UNITS>,$<PLATFORM_ID:Darwin>>:effects/audiounits/AudioUnitEffect.h>

      # Ladspa Effects

      $<$<BOOL:USE_LADSPA>:effects/ladspa/LadspaEffect.cpp>
      $<$<BOOL:USE_LADSPA>:effects/ladspa/LadspaEffect.h>
      $<$<BOOL:USE_LADSPA>:effects/ladspa/ladspa.h>

      # LV2 Effects

      $<$<BOOL:USE_LV2>:effects/lv2/LV2Effect.cpp>
      $<$<BOOL:USE_LV2>:effects/lv2/LV2Effect.h>
      $<$<BOOL:USE_LV2>:effects/lv2/LoadLV2.cpp>
      $<$<BOOL:USE_LV2>:effects/lv2/LoadLV2.h>
      $<$<BOOL:USE_LV2>:effects/lv2/NativeWindow.h>
      $<$<BOOL:USE_LV2>:effects/lv2/lv2_external_ui.h>
      $<$<BOOL:USE_LV2>:effects/lv2/zix/common.h>
      $<$<BOOL:USE_LV2>:effects/lv2/zix/ring.cpp>
      $<$<BOOL:USE_LV2>:effects/lv2/zix/ring.h>

      # Nyquist Effects

      $<$<BOOL:USE_NYQUIST>:effects/nyquist/LoadNyquist.cpp>
      $<$<BOOL:USE_NYQUIST>:effects/nyquist/LoadNyquist.h>
      $<$<BOOL:USE_NYQUIST>:effects/nyquist/Nyquist.cpp>
      $<$<BOOL:USE_NYQUIST>:effects/nyquist/Nyquist.h>

      # VAMP Effects

      $<$<BOOL:USE_VAMP>:effects/vamp/LoadVamp.cpp>
      $<$<BOOL:USE_VAMP>:effects/vamp/LoadVamp.h>
      $<$<BOOL:USE_VAMP>:effects/vamp/VampEffect.cpp>
      $<$<BOOL:USE_VAMP>:effects/vamp/VampEffect.h>

      # Export

      export/Export.cpp
      export/Export.h

      # Standard exporters
      export/ExportCL.cpp
      export/ExportMP3.cpp
      export/ExportMP3.h
      export/ExportMultiple.cpp
      export/ExportMultiple.h
      export/ExportPCM.cpp


      # Optional exporters
      $<$<BOOL:USE_FFMPEG>:export/ExportFFmpeg.cpp>
      $<$<BOOL:USE_FFMPEG>:export/ExportFFmpegDialogs.cpp>
      $<$<BOOL:USE_FFMPEG>:export/ExportFFmpegDialogs.h>
      $<$<BOOL:USE_FLAC>:export/ExportFLAC.cpp>
      $<$<BOOL:USE_TWOLAME>:export/ExportMP2.cpp>
      $<$<BOOL:USE_LIBVORBIS>:export/ExportOGG.cpp>

      # Import

      import/FormatClassifier.cpp
      import/FormatClassifier.h
      import/Import.cpp
      import/Import.h
      import/ImportForwards.h
      import/MultiFormatReader.cpp
      import/MultiFormatReader.h
      import/RawAudioGuess.cpp
      import/RawAudioGuess.h
      import/SpecPowerMeter.cpp
      import/SpecPowerMeter.h

      # Standard importers

      import/ImportLOF.cpp
      import/ImportPCM.cpp
      import/ImportPlugin.h
      import/ImportRaw.cpp
      import/ImportRaw.h

      # Optional importers

      $<$<BOOL:USE_FFMPEG>:import/ImportFFmpeg.cpp>
      $<$<BOOL:USE_FLAC>:import/ImportFLAC.cpp>
      $<$<BOOL:USE_GSTREAMER>:import/ImportGStreamer.cpp>
      $<$<BOOL:USE_MIDI>:import/ImportMIDI.cpp>
      $<$<BOOL:USE_MIDI>:import/ImportMIDI.h>
      $<$<BOOL:USE_LIBMAD>:import/ImportMP3.cpp>
      $<$<BOOL:USE_LIBVORBIS>:import/ImportOGG.cpp>
#      import/ImportQT.cpp

      # Menus

      menus/ClipMenus.cpp
      menus/EditMenus.cpp
      menus/ExtraMenus.cpp
      menus/FileMenus.cpp
      menus/HelpMenus.cpp
      menus/LabelMenus.cpp
      menus/NavigationMenus.cpp
      menus/PluginMenus.cpp
      menus/SelectMenus.cpp
      menus/ToolbarMenus.cpp
      menus/TrackMenus.cpp
      menus/TransportMenus.cpp
      menus/ViewMenus.cpp
      menus/WindowMenus.cpp

      # On demand loading

      ondemand/ODComputeSummaryTask.cpp
      ondemand/ODComputeSummaryTask.h
      ondemand/ODDecodeFFmpegTask.cpp
      ondemand/ODDecodeFFmpegTask.h
      ondemand/ODDecodeFlacTask.cpp
      ondemand/ODDecodeFlacTask.h
      ondemand/ODDecodeTask.cpp
      ondemand/ODDecodeTask.h
      ondemand/ODManager.cpp
      ondemand/ODManager.h
      ondemand/ODTask.cpp
      ondemand/ODTask.h
      ondemand/ODTaskThread.cpp
      ondemand/ODTaskThread.h
      ondemand/ODWaveTrackTaskQueue.cpp
      ondemand/ODWaveTrackTaskQueue.h

      # Preferences

      prefs/BatchPrefs.cpp
      prefs/BatchPrefs.h
      prefs/DevicePrefs.cpp
      prefs/DevicePrefs.h
      prefs/DirectoriesPrefs.cpp
      prefs/DirectoriesPrefs.h
      prefs/EffectsPrefs.cpp
      prefs/EffectsPrefs.h
      prefs/ExtImportPrefs.cpp
      prefs/ExtImportPrefs.h
      prefs/GUIPrefs.cpp
      prefs/GUIPrefs.h
      prefs/GUISettings.h
      prefs/ImportExportPrefs.cpp
      prefs/ImportExportPrefs.h
      prefs/KeyConfigPrefs.cpp
      prefs/KeyConfigPrefs.h
      prefs/LibraryPrefs.cpp
      prefs/LibraryPrefs.h
      prefs/MidiIOPrefs.cpp
      prefs/MidiIOPrefs.h
      prefs/ModulePrefs.cpp
      prefs/ModulePrefs.h
      prefs/MousePrefs.cpp
      prefs/MousePrefs.h
      prefs/PlaybackPrefs.cpp
      prefs/PlaybackPrefs.h
      prefs/PrefsDialog.cpp
      prefs/PrefsDialog.h
      prefs/PrefsPanel.h
      prefs/ProjectsPrefs.cpp
      prefs/ProjectsPrefs.h
      prefs/QualityPrefs.cpp
      prefs/QualityPrefs.h
      prefs/RecordingPrefs.cpp
      prefs/RecordingPrefs.h
      prefs/SpectrogramSettings.cpp
      prefs/SpectrogramSettings.h
      prefs/SpectrumPrefs.cpp
      prefs/SpectrumPrefs.h
      prefs/ThemePrefs.cpp
      prefs/ThemePrefs.h
      prefs/TracksBehaviorsPrefs.cpp
      prefs/TracksBehaviorsPrefs.h
      prefs/TracksPrefs.cpp
      prefs/TracksPrefs.h
      prefs/WarningsPrefs.cpp
      prefs/WarningsPrefs.h
      prefs/WaveformPrefs.cpp
      prefs/WaveformPrefs.h
      prefs/WaveformSettings.cpp
      prefs/WaveformSettings.h

      # Toolbars

      toolbars/ControlToolBar.cpp
      toolbars/ControlToolBar.h
      toolbars/DeviceToolBar.cpp
      toolbars/DeviceToolBar.h
      toolbars/EditToolBar.cpp
      toolbars/EditToolBar.h
      toolbars/MeterToolBar.cpp
      toolbars/MeterToolBar.h
      toolbars/MixerToolBar.cpp
      toolbars/MixerToolBar.h
      toolbars/ScrubbingToolBar.cpp
      toolbars/ScrubbingToolBar.h
      toolbars/SelectionBar.cpp
      toolbars/SelectionBar.h
      toolbars/SelectionBarListener.h
      toolbars/SpectralSelectionBar.cpp
      toolbars/SpectralSelectionBar.h
      toolbars/SpectralSelectionBarListener.h
      toolbars/TimerToolBar.cpp
      toolbars/TimerToolBar.h
      toolbars/ToolBar.cpp
      toolbars/ToolBar.h
      toolbars/ToolDock.cpp
      toolbars/ToolDock.h
      toolbars/ToolManager.cpp
      toolbars/ToolManager.h
      toolbars/ToolsToolBar.cpp
      toolbars/ToolsToolBar.h
      toolbars/TranscriptionToolBar.cpp
      toolbars/TranscriptionToolBar.h

      # Tracks

      tracks/labeltrack/ui/LabelDefaultClickHandle.cpp
      tracks/labeltrack/ui/LabelDefaultClickHandle.h
      tracks/labeltrack/ui/LabelGlyphHandle.cpp
      tracks/labeltrack/ui/LabelGlyphHandle.h
      tracks/labeltrack/ui/LabelTextHandle.cpp
      tracks/labeltrack/ui/LabelTextHandle.h
      tracks/labeltrack/ui/LabelTrackControls.cpp
      tracks/labeltrack/ui/LabelTrackControls.h
      tracks/labeltrack/ui/LabelTrackVRulerControls.cpp
      tracks/labeltrack/ui/LabelTrackVRulerControls.h
      tracks/labeltrack/ui/LabelTrackView.cpp
      tracks/labeltrack/ui/LabelTrackView.h
      tracks/playabletrack/notetrack/ui/NoteTrackButtonHandle.cpp
      tracks/playabletrack/notetrack/ui/NoteTrackButtonHandle.h
      tracks/playabletrack/notetrack/ui/NoteTrackControls.cpp
      tracks/playabletrack/notetrack/ui/NoteTrackControls.h
      tracks/playabletrack/notetrack/ui/NoteTrackSliderHandles.cpp
      tracks/playabletrack/notetrack/ui/NoteTrackSliderHandles.h
      tracks/playabletrack/notetrack/ui/NoteTrackVRulerControls.cpp
      tracks/playabletrack/notetrack/ui/NoteTrackVRulerControls.h
      tracks/playabletrack/notetrack/ui/NoteTrackVZoomHandle.cpp
      tracks/playabletrack/notetrack/ui/NoteTrackVZoomHandle.h
      tracks/playabletrack/notetrack/ui/NoteTrackView.cpp
      tracks/playabletrack/notetrack/ui/NoteTrackView.h
      tracks/playabletrack/notetrack/ui/StretchHandle.cpp
      tracks/playabletrack/notetrack/ui/StretchHandle.h
      tracks/playabletrack/ui/PlayableTrackButtonHandles.cpp
      tracks/playabletrack/ui/PlayableTrackButtonHandles.h
      tracks/playabletrack/ui/PlayableTrackControls.cpp
      tracks/playabletrack/ui/PlayableTrackControls.h
      tracks/playabletrack/wavetrack/ui/CutlineHandle.cpp
      tracks/playabletrack/wavetrack/ui/CutlineHandle.h
      tracks/playabletrack/wavetrack/ui/SampleHandle.cpp
      tracks/playabletrack/wavetrack/ui/SampleHandle.h
      tracks/playabletrack/wavetrack/ui/SpectrumVRulerControls.cpp
      tracks/playabletrack/wavetrack/ui/SpectrumVRulerControls.h
      tracks/playabletrack/wavetrack/ui/SpectrumVZoomHandle.cpp
      tracks/playabletrack/wavetrack/ui/SpectrumVZoomHandle.h
      tracks/playabletrack/wavetrack/ui/SpectrumView.cpp
      tracks/playabletrack/wavetrack/ui/SpectrumView.h
      tracks/playabletrack/wavetrack/ui/WaveTrackControls.cpp
      tracks/playabletrack/wavetrack/ui/WaveTrackControls.h
      tracks/playabletrack/wavetrack/ui/WaveTrackSliderHandles.cpp
      tracks/playabletrack/wavetrack/ui/WaveTrackSliderHandles.h
      tracks/playabletrack/wavetrack/ui/WaveTrackVRulerControls.cpp
      tracks/playabletrack/wavetrack/ui/WaveTrackVRulerControls.h
      tracks/playabletrack/wavetrack/ui/WaveTrackVZoomHandle.cpp
      tracks/playabletrack/wavetrack/ui/WaveTrackVZoomHandle.h
      tracks/playabletrack/wavetrack/ui/WaveTrackView.cpp
      tracks/playabletrack/wavetrack/ui/WaveTrackView.h
      tracks/playabletrack/wavetrack/ui/WaveTrackViewConstants.cpp
      tracks/playabletrack/wavetrack/ui/WaveTrackViewConstants.h
      tracks/playabletrack/wavetrack/ui/WaveformVRulerControls.cpp
      tracks/playabletrack/wavetrack/ui/WaveformVRulerControls.h
      tracks/playabletrack/wavetrack/ui/WaveformVZoomHandle.cpp
      tracks/playabletrack/wavetrack/ui/WaveformVZoomHandle.h
      tracks/playabletrack/wavetrack/ui/WaveformView.cpp
      tracks/playabletrack/wavetrack/ui/WaveformView.h
      tracks/timetrack/ui/TimeTrackControls.cpp
      tracks/timetrack/ui/TimeTrackControls.h
      tracks/timetrack/ui/TimeTrackVRulerControls.cpp
      tracks/timetrack/ui/TimeTrackVRulerControls.h
      tracks/timetrack/ui/TimeTrackView.cpp
      tracks/timetrack/ui/TimeTrackView.h

      # Tracks UI

      tracks/ui/BackgroundCell.cpp
      tracks/ui/BackgroundCell.h
      tracks/ui/ButtonHandle.cpp
      tracks/ui/ButtonHandle.h
      tracks/ui/CommonTrackControls.cpp
      tracks/ui/CommonTrackControls.h
      tracks/ui/CommonTrackPanelCell.cpp
      tracks/ui/CommonTrackPanelCell.h
      tracks/ui/CommonTrackView.cpp
      tracks/ui/CommonTrackView.h
      tracks/ui/EditCursorOverlay.cpp
      tracks/ui/EditCursorOverlay.h
      tracks/ui/EnvelopeHandle.cpp
      tracks/ui/EnvelopeHandle.h
      tracks/ui/PlayIndicatorOverlay.cpp
      tracks/ui/PlayIndicatorOverlay.h
      tracks/ui/ScrubUI.cpp
      tracks/ui/ScrubUI.h
      tracks/ui/Scrubbing.cpp
      tracks/ui/Scrubbing.h
      tracks/ui/SelectHandle.cpp
      tracks/ui/SelectHandle.h
      tracks/ui/SliderHandle.cpp
      tracks/ui/SliderHandle.h
      tracks/ui/TimeShiftHandle.cpp
      tracks/ui/TimeShiftHandle.h
      tracks/ui/TrackButtonHandles.cpp
      tracks/ui/TrackButtonHandles.h
      tracks/ui/TrackControls.cpp
      tracks/ui/TrackControls.h
      tracks/ui/TrackSelectHandle.cpp
      tracks/ui/TrackSelectHandle.h
      tracks/ui/TrackVRulerControls.cpp
      tracks/ui/TrackVRulerControls.h
      tracks/ui/TrackView.cpp
      tracks/ui/TrackView.h
      tracks/ui/ZoomHandle.cpp
      tracks/ui/ZoomHandle.h

      # Widgets

      widgets/AButton.cpp
      widgets/AButton.h
      widgets/ASlider.cpp
      widgets/ASlider.h
      widgets/AttachableScrollBar.cpp
      widgets/AttachableScrollBar.h
      widgets/AudacityMessageBox.cpp
      widgets/AudacityMessageBox.h
      widgets/BackedPanel.cpp
      widgets/BackedPanel.h
      widgets/ErrorDialog.cpp
      widgets/ErrorDialog.h
      widgets/ExpandingToolBar.cpp
      widgets/ExpandingToolBar.h
      widgets/FileHistory.cpp
      widgets/FileHistory.h
      widgets/Grabber.cpp
      widgets/Grabber.h
      widgets/Grid.cpp
      widgets/Grid.h
      widgets/HelpSystem.cpp
      widgets/HelpSystem.h
      widgets/HtmlWindow.cpp
      widgets/HtmlWindow.h
      widgets/ImageRoll.cpp
      widgets/ImageRoll.h
      widgets/KeyView.cpp
      widgets/KeyView.h
      widgets/Meter.cpp
      widgets/Meter.h
      widgets/MeterPanelBase.cpp
      widgets/MeterPanelBase.h
      widgets/MultiDialog.cpp
      widgets/MultiDialog.h
      widgets/NumericTextCtrl.cpp
      widgets/NumericTextCtrl.h
      widgets/Overlay.cpp
      widgets/Overlay.h
      widgets/OverlayPanel.cpp
      widgets/OverlayPanel.h
      widgets/PopupMenuTable.cpp
      widgets/PopupMenuTable.h
      widgets/ProgressDialog.cpp
      widgets/ProgressDialog.h
      widgets/Ruler.cpp
      widgets/Ruler.h
      widgets/Warning.cpp
      widgets/Warning.h
      widgets/WindowAccessible.cpp
      widgets/WindowAccessible.h
      widgets/numformatter.cpp
      widgets/numformatter.h
      widgets/valnum.cpp
      widgets/valnum.h
      widgets/wxPanelWrapper.cpp
      widgets/wxPanelWrapper.h

      # XML handling
      xml/XMLFileReader.cpp
      xml/XMLFileReader.h
      xml/XMLTagHandler.cpp
      xml/XMLTagHandler.h
      xml/XMLWriter.cpp
      xml/XMLWriter.h
      xml/audacityproject.dtd
)

#
#
#
list( APPEND HEADERS
   ../include/audacity/EffectInterface.h
   ../include/audacity/Types.h
   ../include/audacity/ConfigInterface.h
   ../include/audacity/ModuleInterface.h
   ../include/audacity/PluginInterface.h
   ../include/audacity/ComponentInterface.h
   ../include/audacity/EffectAutomationParameters.h
   ../include/audacity/ImporterInterface.h
)

#
#
#
list( APPEND INCLUDES
   PRIVATE
      ${CMAKE_CURRENT_BINARY_DIR}/private
      ${topdir}/lib-src/lib-widget-extra
      ${topdir}/include
      ${TARGET_ROOT}
)

#
#
#
list( APPEND RESOURCES
   $<$<PLATFORM_ID:Windows>:../win/audacity.rc>
   $<$<PLATFORM_ID:Windows>:../win/packages.config>
)

#
# Get those pesky CMake files out of the way (on Windows)
#
source_group(
    TREE
        "${_INTDIR}"
    PREFIX
        "CMake"
    FILES
        ${_INTDIR}/CMakeFiles/Audacity.dir/cmake_pch.cxx
        ${_INTDIR}/CMakeFiles/Audacity.dir/cmake_pch.hxx
)

#
#
#
list( APPEND DEFINES
   PRIVATE
      BUILDING_AUDACITY
      XML_STATIC
      wxDEBUG_LEVEL=0
      WXINTL_NO_GETTEXT_MACRO
      WXUSINGDLL
      $<$<PLATFORM_ID:Windows>:
         _CRT_SECURE_NO_WARNINGS
         __STDC_CONSTANT_MACROS
         __WXMSW__
         NDEBUG
         WIN32
         STRICT
      >
)

list( APPEND OPTIONS
   PRIVATE
      -std=c++11
      $<$<CXX_COMPILER_ID:Clang>:-stdlib=libc++>
      $<$<PLATFORM_ID:Windows>:/permissive->
#      $<$<CXX_COMPILER_ID:GNU>:-Wl,-rpath -Wl,${_RPATH}>
)

list( APPEND FEATURES
   PRIVATE
      cxx_std_11
)

list( APPEND LDFLAGS
   PRIVATE
      $<$<PLATFORM_ID:Windows>:/MANIFEST:NO>
)

#
#
#
list( APPEND LIBRARIES
   PRIVATE
      ${CMAKE_REQUIRED_LIBRARIES}
      wxwidgets
      FileDialog
      expat
      lame
      libsndfile
      libsoxr
      lib-widget-extra
      portaudio-v19
      $<$<BOOL:USE_LIBID3TAG>:libid3tag>
      $<$<BOOL:USE_LIBFLAC>:libflac>
      $<$<BOOL:USE_LIBFLAC>:libflac++>
      $<$<BOOL:USE_LIBMAD>:libmad>
      $<$<BOOL:USE_LIBVORBIS>:libogg>
      $<$<BOOL:USE_LIBVORBIS>:libvorbis>
      $<$<BOOL:USE_LIBTWOLAME>:twolame>
      $<$<BOOL:USE_LV2>:lv2>
      $<$<BOOL:USE_MIDI>:portmidi>
      $<$<BOOL:USE_MIDI>:portsmf>
      $<$<BOOL:USE_NYQUIST>:libnyquist>
      $<$<BOOL:USE_PORTMIXER>:portmixer>
      $<$<BOOL:USE_SBSMS>:sbsms>
      $<$<BOOL:USE_SOUNDTOUCH>:soundtouch>
      $<$<BOOL:USE_VAMP>:libvamp>
)

#
# If was have cmake 3.16 or higher, we can use precompiled headers
#
if( CMAKE_VERSION VERSION_GREATER_EQUAL "3.16" )
   list( APPEND PRECOMP
      AudacityHeaders.h
   )
endif()

# Handle Ladspa option
option( enable_ladspa "Enable LADSPA plug-in support" ON )
set( USE_LADSPA ${enable_ladspa} )

# Handle VST option
option( enable_vst "Enable VST2 plug-in support" ON )
set( USE_VST ${enable_vst} )

set( AUDACITY_NAME "Audacity" )
set( BUILDING_AUDACITY 1 )
set( INSTALL_PREFIX "" )
#set( DISABLE_DYNAMIC_LOADING_FFMPEG 1 )
#set( DISABLE_DYNAMIC_LOADING_LAME 1 )

if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   set_directory_properties(
      PROPERTIES
         # Make sure Audacity is the startup project
         VS_STARTUP_PROJECT "${TARGET}"
   )

   set( PACKAGES
      "${GETTEXT_NAME}_${GETTEXT_VERSION}"
      "${PYTHON_NAME}_${PYTHON_VERSION}"
   )

   set_target_properties(
      ${TARGET}
      PROPERTIES
         # Make sure we're a GUI application and not a console tool
         WIN32_EXECUTABLE ON

         # These two are for auto-restoring gettext and python
         VS_PACKAGE_REFERENCES "${PACKAGES}"
         DOTNET_TARGET_FRAMEWORK_VERSION "v4.5"
   )

   # And copy the NuGet packages config
   configure_file( ../win/packages.config packages.config COPYONLY )

   # Create the config file
   configure_file( audacity_config.h.in private/configwin.h )

   # Copy over the wxWidgets DLLs
   if( use_system_wxwidgets )
      set( wxlibs "$ENV{WXWIN}" )
   else()
      set( wxlibs "${CMAKE_BINARY_DIR}" )
   endif()

   file( TO_NATIVE_PATH ${_EXEDIR} exedir )
   add_custom_command(
      TARGET
         ${TARGET}
      COMMAND
         XCOPY "*.dll" ${exedir} /I /R /Y 
      WORKING_DIRECTORY
         "${wxlibs}/lib/vc_dll"
      POST_BUILD
   )
elseif( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   # Handle Audio Units option
   option( enable_audiounits "Enable Audio Unit plug-in support" ON )
   set( USE_AUDIO_UNITS ${enable_audiounits} )

   set_target_properties(
      ${TARGET}
      PROPERTIES
         # Build a bundle
         MACOSX_BUNDLE 1
   )

   # These sources must be compiled as Objective C++
   set_source_files_properties(
      KeyboardCapture.cpp
      effects/EffectUI.cpp
      menus/WindowMenus.cpp
      PROPERTIES
         COMPILE_FLAGS "-x objective-c++"
   )

   # Because of the "misidentified" Objective C++ sources listed above,
   # we can only use precompiled headers if also using Xcode.
   if( NOT "${CMAKE_GENERATOR}" MATCHES "Xcode" )
      set( PRECOMP )
   endif()

   # Add our required frameworks
   list(APPEND LIBRARIES
      PRIVATE
         "-framework AudioUnit"
         "-framework CoreAudioKit"
   )

   # Create the config file
   set( HAVE_VISIBILITY 1 )
   configure_file( audacity_config.h.in private/configmac.h )

   # Extract the version information
   execute_process(
      COMMAND
         gcc -E -dM ${_SRCDIR}/Audacity.h
      COMMAND
         awk "/#define *AUDACITY_(VERSION|RELEASE|REVISION|MODLEVEL) /{printf \"%s %s;\",$2,$3}"
      OUTPUT_VARIABLE
         output
   )

   foreach( line ${output} )
      string( REPLACE " " ";" line ${line})
      list( GET line 0 name )
      list( GET line 1 value )
      set( ${name} ${value} )
   endforeach()

   set( AUDACITY_DIST_VERSION ${AUDACITY_VERSION}.${AUDACITY_RELEASE}.${AUDACITY_REVISION} )
   set( AUDACITY_INFO_VERSION ${AUDACITY_VERSION}.${AUDACITY_RELEASE}.${AUDACITY_REVISION}.${AUDACITY_MODLEVEL} )

   # Copy the wxWidgets libraries into the bundle
   add_custom_command(
      TARGET
         ${TARGET}
      COMMAND
         sh -c "TARGET_BUILD_DIR=${_EXEDIR} EXECUTABLE_PATH=MacOS/Audacity FRAMEWORKS_FOLDER_PATH=Frameworks ${topdir}/mac/scripts/install_wxlibs.sh"
      WORKING_DIRECTORY
         ${CMAKE_BINARY_DIR}/lib
      POST_BUILD
   )
elseif( CMAKE_SYSTEM_NAME MATCHES "Linux" )
   # Add additional library requirements
   list( APPEND LIBRARIES
      PRIVATE
         PkgConfig::GLIB
         z
         pthread
   )

   # Create the config file
   set( HAVE_VISIBILITY 1 )
   configure_file( audacity_config.h.in private/configunix.h )

   # Create the script to copy required wxWidgets libraries
   file( WRITE "${_INTDIR}/copy_libs.sh"
      "for lib in \$(ldd ${_EXEDIR}/Audacity | awk '/libwx/{print \$1}')
       do
          echo \${lib}
          ldd ${WXWIN}/lib/\${lib} | awk '/libwx/{print \$1}'
       done | sort -u | xargs cp -n -H -t ${_LIBDIR}
      rm \${0}"
   )

   # And run it after the build
   add_custom_command(
      TARGET
         ${TARGET}
      COMMAND
         sh "${_INTDIR}/copy_libs.sh"
      WORKING_DIRECTORY
         ${WXWIN}/lib
      POST_BUILD
   )
endif()

find_package (Git)
if (GIT_FOUND)
   execute_process(
      COMMAND
         ${GIT_EXECUTABLE} show -s "--format=\"%H\";\"%cd\";"
      OUTPUT_VARIABLE
         output
   )
   list( GET output 0 long )
   list( GET output 1 time )
   list( APPEND DEFINES
      REV_LONG=${long}
      REV_TIME=${time}
   )

endif()

organize_source( "${TARGET_ROOT}/.." "include" "${HEADERS}" )
organize_source( "${TARGET_ROOT}/../win" "win" "${RESOURCES}" )
organize_source( "${TARGET_ROOT}" "src" "${SOURCES}" )

target_sources( ${TARGET} PRIVATE ${HEADERS} ${SOURCES} ${RESOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_features( ${TARGET} PRIVATE ${FEATURES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_options( "${TARGET}" PRIVATE ${LDFLAGS} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )
target_precompile_headers( ${TARGET} PRIVATE ${PRECOMP} )

