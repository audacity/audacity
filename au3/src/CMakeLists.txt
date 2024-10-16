#[[
Configure the executable, modules, and libraries
]]

set( TARGET Audacity )
set( TARGET_ROOT ${topdir}/src )

include (CheckFunctionExists)

message( STATUS "========== Configuring ${TARGET} ==========" )

# Allow modules to link against the Audacity executable
set( CMAKE_ENABLE_EXPORTS ON )

add_executable( ${TARGET} )
add_dependencies( ${TARGET} locale )
add_dependencies( ${TARGET} nyquist )
add_dependencies( ${TARGET} plug-ins )

def_vars()

# Add a target that will provide the git revision info
# whenever it changes.  (Must be done at build time, not
# configuration time.)
if( DEFINED AUDACITY_REV_LONG AND DEFINED AUDACITY_REV_TIME )
   file( WRITE "${_PRVDIR}/RevisionIdent.h"
      "#define REV_LONG \"${AUDACITY_REV_LONG}\"\n#define REV_TIME \"${AUDACITY_REV_TIME}\"\n"
   )
elseif( GIT_FOUND )
   add_custom_target(
      version
      COMMAND
         ${CMAKE_COMMAND} -D GIT="${GIT_EXECUTABLE}"
                          -D "_PRVDIR=${_PRVDIR}"
                          -P "${AUDACITY_MODULE_PATH}/Version.cmake"
      WORKING_DIRECTORY
         ${topdir}
   )

   add_dependencies( ${TARGET} version )
else()
   # No Git installed and no version data is available.
   # Generate an empty file and let AboutDialog do the rest
   # WRITE, unlike TOUCH, will create any directories needed
   # (see https://github.com/audacity/audacity/issues/2163)
   file( WRITE "${_PRVDIR}/RevisionIdent.h" "" )
endif()

# Handle VST option
cmd_option(
   ${_OPT}use_vst
   "Use VST2 plug-in support [on, off]"
   ON
)
set( USE_VST ${${_OPT}use_vst} CACHE INTERNAL "" )

#
#
#
set( SOURCES
   PRIVATE
      ActiveProject.cpp
      ActiveProject.h
      AboutDialog.cpp
      AboutDialog.h
      $<$<NOT:$<BOOL:${${_OPT}bundle_gplv3}>>:AboutDialogGPLv2Text.cpp>
      $<$<BOOL:${${_OPT}bundle_gplv3}>:AboutDialogGPLv3Text.cpp>
      AdornedRulerPanel.cpp
      AdornedRulerPanel.h
      AnalyzedWaveClip.cpp
      AnalyzedWaveClip.h
      AudacityApp.cpp
      AudacityApp.h
      $<$<BOOL:${wxIS_MAC}>:AudacityApp.mm>
      AudacityFileConfig.cpp
      AudacityFileConfig.h
      AudacityHeaders.cpp
      AudacityHeaders.h
      AudacityMirProject.cpp
      AudacityMirProject.h
      AudioPasteDialog.cpp
      AudioPasteDialog.h
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
      CellularPanel.cpp
      CellularPanel.h
      Clipboard.cpp
      Clipboard.h
      ClipMirAudioReader.cpp
      ClipMirAudioReader.h
      CommonCommandFlags.cpp
      CommonCommandFlags.h
      CrashReport.cpp
      CrashReport.h
      DefaultPlaybackPolicy.cpp
      DefaultPlaybackPolicy.h
      Diags.cpp
      Diags.h
      DropTarget.cpp
      EffectAndCommandPluginManager.cpp
      EffectAndCommandPluginManager.h
      ExportLabelTrack.cpp
      DropoutDetector.cpp
      EnvelopeEditor.cpp
      EnvelopeEditor.h
      FrameStatisticsDialog.cpp
      FrameStatisticsDialog.h
      FreqWindow.cpp
      FreqWindow.h
      HelpUtilities.cpp
      HelpUtilities.h
      HistoryWindow.cpp
      HistoryWindow.h
      HitTestResult.h
      IncompatiblePluginsDialog.cpp
      IncompatiblePluginsDialog.h
      JournalEvents.cpp
      JournalEvents.h
      JournalWindowPaths.cpp
      JournalWindowPaths.h
      KeyboardCapture.cpp
      KeyboardCapture.h
      LabelDialog.cpp
      LabelDialog.h
      LangChoice.cpp
      LangChoice.h
      Legacy.cpp
      Legacy.h
      DoEffect.cpp
      DoEffect.h
      ListNavigationEnabled.cpp
      ListNavigationEnabled.h
      ListNavigationPanel.cpp
      ListNavigationPanel.h
      MenuCreator.cpp
      MenuCreator.h
      MixerBoard.cpp
      MixerBoard.h
      MouseWheelHandler.cpp
      MovableControl.cpp
      MovableControl.h
      NoteTrackEditing.cpp
      PluginDataModel.cpp
      PluginDataModel.h
      PluginDataViewCtrl.cpp
      PluginDataViewCtrl.h
      PluginRegistrationDialog.cpp
      PluginRegistrationDialog.h
      PluginStartupRegistration.cpp
      PluginStartupRegistration.h
      Profiler.cpp
      Profiler.h
      ProjectAudioManager.cpp
      ProjectAudioManager.h
      ProjectFileManager.cpp
      ProjectFileManager.h
      ProjectManager.cpp
      ProjectManager.h
      ProjectSettings.cpp
      ProjectSettings.h
      ProjectTempoListener.cpp
      ProjectTimeRuler.cpp
      ProjectTimeRuler.h
      ProjectWindow.cpp
      ProjectWindow.h
      ProjectWindowBase.cpp
      ProjectWindowBase.h
      RefreshCode.h
      ProjectWindows.cpp
      ProjectWindows.h
      RealtimeEffectPanel.cpp
      RealtimeEffectPanel.h
      ScrubState.cpp
      ScrubState.h
      SelectUtilities.cpp
      SelectUtilities.h
      ShuttleGetDefinition.cpp
      ShuttleGetDefinition.h
      SoundActivatedRecord.cpp
      SoundActivatedRecord.h
      SpectralDataDialog.cpp
      SpectralDataManager.h
      SpectralDataManager.cpp
      SseMathFuncs.cpp
      SseMathFuncs.h
      TagsEditor.cpp
      TagsEditor.h
      ThemedWrappers.h
      TimeDialog.cpp
      TimeDialog.h
      TimeDisplayMode.cpp
      TimeDisplayMode.h
      TimeTrackEditing.cpp
      TimerRecordDialog.cpp
      TimerRecordDialog.h
      TimerRecordExportDialog.cpp
      TimerRecordExportDialog.h
      TrackArt.cpp
      TrackArt.h
      TrackArtist.cpp
      TrackArtist.h
      TrackInfo.cpp
      TrackInfo.h
      TrackPanel.cpp
      TrackPanel.h
      TrackPanelAx.cpp
      TrackPanelAx.h
      TrackPanelCell.cpp
      TrackPanelCell.h
      TrackPanelConstants.h
      TrackPanelDrawable.cpp
      TrackPanelDrawable.h
      TrackPanelDrawingContext.h
      TrackPanelMouseEvent.h
      TrackPanelResizeHandle.cpp
      TrackPanelResizeHandle.h
      TrackPanelResizerCell.cpp
      TrackPanelResizerCell.h
      TrackUtilities.cpp
      TrackUtilities.h
      UIHandle.cpp
      UIHandle.h
      TransportUtilities.cpp
      TransportUtilities.h
      VoiceKey.cpp
      VoiceKey.h
      WaveTrackLocation.cpp
      WaveTrackLocation.h

      $<$<BOOL:${_OPT}has_whats_new>:
         WhatsNewDialog.cpp
         WhatsNewDialog.h
      >

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
      commands/CommandDirectory.cpp
      commands/CommandDirectory.h
      commands/CommandDispatch.cpp
      commands/CommandDispatch.h
      commands/CommandHandler.cpp
      commands/CommandHandler.h
      commands/CommandManagerWindowClasses.cpp
      commands/CommandManagerWindowClasses.h
      commands/CommandMisc.h
      commands/CommandSignature.cpp
      commands/CommandSignature.h
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
      commands/wxCommandTargets.cpp
      commands/wxCommandTargets.h

      # Built-in Effects

      effects/Amplify.cpp
      effects/Amplify.h
      effects/AutoDuck.cpp
      effects/AutoDuck.h
      effects/BasicEffectUIServices.cpp
      effects/BasicEffectUIServices.h
      effects/BassTreble.cpp
      effects/BassTreble.h
      effects/ChangePitch.cpp
      effects/ChangePitch.h
      effects/ChangeSpeed.cpp
      effects/ChangeSpeed.h
      effects/ChangeTempo.cpp
      effects/ChangeTempo.h
      effects/ClickRemoval.cpp
      effects/ClickRemoval.h
      effects/ClipIndicatorPanel.cpp
      effects/ClipIndicatorPanel.h
      effects/CompressionMeterPanel.cpp
      effects/CompressionMeterPanel.h
      effects/Contrast.cpp
      effects/Contrast.h
      effects/Distortion.cpp
      effects/Distortion.h
      effects/DtmfGen.cpp
      effects/DtmfGen.h
      effects/DynamicRangeProcessorDummyOutputs.h
      effects/DynamicRangeProcessorEditor.cpp
      effects/DynamicRangeProcessorEditor.h
      effects/DynamicRangeProcessorEffectUtils.cpp
      effects/DynamicRangeProcessorEffectUtils.h
      effects/DynamicRangeProcessorHistoryPanel.cpp
      effects/DynamicRangeProcessorHistoryPanel.h
      effects/DynamicRangeProcessorPanelCommon.cpp
      effects/DynamicRangeProcessorPanelCommon.h
      effects/DynamicRangeProcessorTransferFunctionPanel.cpp
      effects/DynamicRangeProcessorTransferFunctionPanel.h
      effects/Echo.cpp
      effects/Echo.h
      effects/EffectEditor.cpp
      effects/EffectEditor.h
      effects/EffectPreview.cpp
      effects/EffectPreview.h
      effects/EffectPresetDialog.cpp
      effects/EffectPresetDialog.h
      effects/EffectUI.cpp
      $<$<BOOL:${wxIS_MAC}>:effects/EffectUI.mm>
      effects/EffectUI.h
      effects/EffectUIServices.cpp
      effects/EffectUIServices.h
      effects/Equalization.cpp
      effects/Equalization.h
      effects/Equalization48x.cpp
      effects/Equalization48x.h
      effects/EqualizationBandSliders.cpp
      effects/EqualizationBandSliders.h
      effects/EqualizationCurvesDialog.cpp
      effects/EqualizationCurvesDialog.h
      effects/EqualizationPanel.cpp
      effects/EqualizationPanel.h
      effects/EqualizationUI.cpp
      effects/EqualizationUI.h
      effects/FindClipping.cpp
      effects/FindClipping.h
      effects/LegacyCompressor.cpp
      effects/LegacyCompressor.h
      effects/Limiter.cpp
      effects/Limiter.h
      effects/LimiterEditor.cpp
      effects/LimiterEditor.h
      effects/Loudness.cpp
      effects/Loudness.h
      effects/Noise.cpp
      effects/Noise.h
      effects/NoiseReduction.cpp
      effects/NoiseReduction.h
      effects/Normalize.cpp
      effects/Normalize.h
      effects/Paulstretch.cpp
      effects/Paulstretch.h
      effects/Phaser.cpp
      effects/Phaser.h
      effects/Compressor.cpp
      effects/Compressor.h
      effects/CompressorEditor.cpp
      effects/CompressorEditor.h
      effects/RealtimeEffectStateUI.cpp
      effects/RealtimeEffectStateUI.h
      effects/RegisterBuiltinEffects.cpp
      effects/RegisterBuiltinEffects.h
      effects/Repeat.cpp
      effects/Repeat.h
      effects/Reverb.cpp
      effects/Reverb.h
      effects/ScienFilter.cpp
      effects/ScienFilter.h
      effects/ScoreAlignDialog.cpp
      effects/ScoreAlignDialog.h
      effects/Silence.cpp
      effects/Silence.h
      effects/StatefulEffectUIServices.cpp
      effects/StatefulEffectUIServices.h
      effects/StatelessPerTrackEffect.cpp
      effects/StatelessPerTrackEffect.h
      effects/TimeScale.cpp
      effects/TimeScale.h
      effects/ToneGen.cpp
      effects/ToneGen.h
      effects/TruncSilence.cpp
      effects/TruncSilence.h
      effects/Wahwah.cpp
      effects/Wahwah.h

      # VST Effects

      $<$<BOOL:${USE_VST}>:
         effects/VST/VSTControl.h
         effects/VST/VSTEditor.cpp
         effects/VST/VSTEditor.h
         effects/VST/VSTEffect.cpp
         effects/VST/VSTEffect.h
         effects/VST/VSTEffectOptionsDialog.cpp
         effects/VST/VSTEffectOptionsDialog.h
      >

      $<$<AND:$<BOOL:${USE_VST}>,$<BOOL:${wxIS_GTK}>>:
         effects/VST/VSTControlGTK.cpp
         effects/VST/VSTControlGTK.h
      >

      $<$<AND:$<BOOL:${USE_VST}>,$<BOOL:${wxIS_WIN}>>:
         effects/VST/VSTControlMSW.cpp
         effects/VST/VSTControlMSW.h
      >

      $<$<AND:$<BOOL:${USE_VST}>,$<BOOL:${wxIS_MAC}>>:
         effects/VST/VSTControlOSX.h
         effects/VST/VSTControlOSX.mm
      >

      $<$<BOOL:${${_OPT}has_vst3}>:
         effects/VST3/VST3Editor.cpp
         effects/VST3/VST3Editor.h
         effects/VST3/VST3Effect.h
         effects/VST3/VST3Effect.cpp
         effects/VST3/VST3OptionsDialog.h
         effects/VST3/VST3OptionsDialog.cpp
         effects/VST3/VST3ParametersWindow.h
         effects/VST3/VST3ParametersWindow.cpp
         effects/VST3/internal/PlugFrame.h
         effects/VST3/internal/PlugFrame.cpp
      >

      $<$<AND:$<BOOL:${${_OPT}has_vst3}>,$<BOOL:${wxIS_GTK}>>:
         effects/VST3/internal/x11/RunLoop.h
         effects/VST3/internal/x11/RunLoop.cpp
         effects/VST3/internal/x11/PlugFrame.h
         effects/VST3/internal/x11/PlugFrame.cpp
         effects/VST3/internal/x11/SocketWindow.h
         effects/VST3/internal/x11/SocketWindow.cpp
      >

      # Audio Unit Effects

      $<$<AND:$<BOOL:${USE_AUDIO_UNITS}>,$<BOOL:${wxIS_MAC}>>:
         effects/audiounits/AUControl.h
         effects/audiounits/AUControl.mm
         effects/audiounits/AudioUnitEditor.cpp
         effects/audiounits/AudioUnitEditor.h
         effects/audiounits/AudioUnitEffect.cpp
         effects/audiounits/AudioUnitEffect.h
         effects/audiounits/AudioUnitEffectOptionsDialog.cpp
         effects/audiounits/AudioUnitEffectOptionsDialog.h
      >

      # Ladspa Effects

      $<$<BOOL:${USE_LADSPA}>:
         effects/ladspa/LadspaEditor.cpp
         effects/ladspa/LadspaEditor.h
         effects/ladspa/LadspaEffect.cpp
         effects/ladspa/LadspaEffect.h
	 effects/ladspa/LadspaEffectOptionsDialog.cpp
	 effects/ladspa/LadspaEffectOptionsDialog.h
      >

      # LV2 Effects

      $<$<BOOL:${USE_LV2}>:
         effects/lv2/LV2Editor.cpp
         effects/lv2/LV2Editor.h
         effects/lv2/LV2Effect.cpp
         effects/lv2/LV2Effect.h
         effects/lv2/LV2EffectMeter.cpp
         effects/lv2/LV2EffectMeter.h
         effects/lv2/LV2PreferencesDialog.cpp
         effects/lv2/LV2PreferencesDialog.h
         effects/lv2/LV2UIFeaturesList.cpp
         effects/lv2/LV2UIFeaturesList.h
         effects/lv2/NativeWindow.h
      >

      # Nyquist Effects

      $<$<BOOL:${USE_NYQUIST}>:
         effects/nyquist/Nyquist.cpp
         effects/nyquist/Nyquist.h
      >

      # VAMP Effects

      $<$<BOOL:${USE_VAMP}>:
         effects/vamp/LoadVamp.cpp
         effects/vamp/LoadVamp.h
         effects/vamp/VampEffect.cpp
         effects/vamp/VampEffect.h
      >

      # Export

      export/ExportAudioDialog.cpp
      export/ExportAudioDialog.h
      export/ExportFilePanel.cpp
      export/ExportFilePanel.h
      export/ExportMixerPanel.cpp
      export/ExportMixerPanel.h
      export/ExportMixerDialog.cpp
      export/ExportMixerDialog.h
      export/ExportOptionsHandler.cpp
      export/ExportOptionsHandler.h

      # Import

      import/FormatClassifier.cpp
      import/FormatClassifier.h
      import/ImportStreamDialog.cpp
      import/ImportStreamDialog.h
      import/MultiFormatReader.cpp
      import/MultiFormatReader.h
      import/RawAudioGuess.cpp
      import/RawAudioGuess.h
      import/SpecPowerMeter.cpp
      import/SpecPowerMeter.h
      import/ImportRaw.cpp
      import/ImportRaw.h

      # Menus

      menus/ClipMenus.cpp
      menus/EditMenus.cpp
      menus/ExtraMenus.cpp
      menus/FileMenus.cpp
      menus/HelpMenus.cpp
      menus/LabelMenus.cpp
      menus/MenuHelper.cpp
      menus/MenuHelper.h
      menus/NavigationMenus.cpp
      menus/PluginMenus.cpp
      menus/SelectMenus.cpp
      menus/TimelineMenus.cpp
      menus/ToolbarMenus.cpp
      menus/TrackMenus.cpp
      menus/TransportMenus.cpp
      menus/ViewMenus.cpp

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
      prefs/GUISettings.cpp
      prefs/GUISettings.h
      prefs/ImportExportPrefs.cpp
      prefs/ImportExportPrefs.h
      prefs/KeyConfigPrefs.cpp
      prefs/KeyConfigPrefs.h
      prefs/MidiIOPrefs.cpp
      prefs/MidiIOPrefs.h
      prefs/ModulePrefs.cpp
      prefs/ModulePrefs.h
      prefs/PlaybackPrefs.cpp
      prefs/PlaybackPrefs.h
      prefs/PrefsDialog.cpp
      prefs/PrefsDialog.h
      prefs/QualityPrefs.cpp
      prefs/QualityPrefs.h
      prefs/RecordingPrefs.cpp
      prefs/RecordingPrefs.h
      prefs/SpectrumPrefs.cpp
      prefs/SpectrumPrefs.h
      prefs/TracksBehaviorsPrefs.cpp
      prefs/TracksBehaviorsPrefs.h
      prefs/TracksPrefs.cpp
      prefs/TracksPrefs.h
      prefs/WaveformPrefs.cpp
      prefs/WaveformPrefs.h
      prefs/WaveformScale.cpp
      prefs/WaveformScale.h

      # Toolbars

      toolbars/AudioSetupToolBar.cpp
      toolbars/AudioSetupToolBar.h
      toolbars/CutCopyPasteToolBar.cpp
      toolbars/CutCopyPasteToolBar.h
      toolbars/ControlToolBar.cpp
      toolbars/ControlToolBar.h
      toolbars/DeviceToolBar.cpp
      toolbars/DeviceToolBar.h
      toolbars/EditToolBar.cpp
      toolbars/EditToolBar.h
      toolbars/MeterToolBar.cpp
      toolbars/MeterToolBar.h
      toolbars/SelectionBar.cpp
      toolbars/SelectionBar.h
      toolbars/SnappingToolBar.cpp
      toolbars/SnappingToolBar.h
      toolbars/SpectralSelectionBar.cpp
      toolbars/SpectralSelectionBar.h
      toolbars/TimeSignatureToolBar.cpp
      toolbars/TimeSignatureToolBar.h
      toolbars/TimeToolBar.cpp
      toolbars/TimeToolBar.h
      toolbars/ToolBar.cpp
      toolbars/ToolBar.h
      toolbars/ToolBarButtons.cpp
      toolbars/ToolBarButtons.h
      toolbars/ToolDock.cpp
      toolbars/ToolDock.h
      toolbars/ToolManager.cpp
      toolbars/ToolManager.h
      toolbars/ToolsToolBar.cpp
      toolbars/ToolsToolBar.h

      # Tracks

      tracks/labeltrack/ui/LabelDefaultClickHandle.cpp
      tracks/labeltrack/ui/LabelDefaultClickHandle.h
      tracks/labeltrack/ui/LabelGlyphHandle.cpp
      tracks/labeltrack/ui/LabelGlyphHandle.h
      tracks/labeltrack/ui/LabelTextHandle.cpp
      tracks/labeltrack/ui/LabelTextHandle.h
      tracks/labeltrack/ui/LabelTrackControls.cpp
      tracks/labeltrack/ui/LabelTrackControls.h
      tracks/labeltrack/ui/LabelTrackShifter.cpp
      tracks/labeltrack/ui/LabelTrackVRulerControls.cpp
      tracks/labeltrack/ui/LabelTrackVRulerControls.h
      tracks/labeltrack/ui/LabelTrackView.cpp
      tracks/labeltrack/ui/LabelTrackView.h
      tracks/playabletrack/notetrack/ui/NoteTrackAffordanceControls.h
      tracks/playabletrack/notetrack/ui/NoteTrackAffordanceControls.cpp
      tracks/playabletrack/notetrack/ui/NoteTrackButtonHandle.cpp
      tracks/playabletrack/notetrack/ui/NoteTrackButtonHandle.h
      tracks/playabletrack/notetrack/ui/NoteTrackControls.cpp
      tracks/playabletrack/notetrack/ui/NoteTrackControls.h
      tracks/playabletrack/notetrack/ui/NoteTrackDisplayData.cpp
      tracks/playabletrack/notetrack/ui/NoteTrackDisplayData.h
      tracks/playabletrack/notetrack/ui/NoteTrackMenuItems.cpp
      tracks/playabletrack/notetrack/ui/NoteTrackShifter.cpp
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
      tracks/playabletrack/wavetrack/ui/ClipOverflowButtonHandle.cpp
      tracks/playabletrack/wavetrack/ui/ClipOverflowButtonHandle.h
      tracks/playabletrack/wavetrack/ui/ClipButtonId.h
      tracks/playabletrack/wavetrack/ui/ClipParameters.cpp
      tracks/playabletrack/wavetrack/ui/ClipParameters.h
      tracks/playabletrack/wavetrack/ui/ClipPitchAndSpeedButtonHandle.cpp
      tracks/playabletrack/wavetrack/ui/ClipPitchAndSpeedButtonHandle.h
      tracks/playabletrack/wavetrack/ui/CutlineHandle.cpp
      tracks/playabletrack/wavetrack/ui/CutlineHandle.h
      tracks/playabletrack/wavetrack/ui/HighlitClipButtonHandle.cpp
      tracks/playabletrack/wavetrack/ui/HighlitClipButtonHandle.h
      tracks/playabletrack/wavetrack/ui/LowlitClipButton.cpp
      tracks/playabletrack/wavetrack/ui/LowlitClipButton.h
      tracks/playabletrack/wavetrack/ui/PitchAndSpeedDialog.cpp
      tracks/playabletrack/wavetrack/ui/PitchAndSpeedDialog.h
      tracks/playabletrack/wavetrack/ui/SampleHandle.cpp
      tracks/playabletrack/wavetrack/ui/SampleHandle.h
      tracks/playabletrack/wavetrack/ui/ShuttleGuiScopedSizer.h
      tracks/playabletrack/wavetrack/ui/SpectrumCache.cpp
      tracks/playabletrack/wavetrack/ui/SpectrumCache.h
      tracks/playabletrack/wavetrack/ui/SpectrumVRulerControls.cpp
      tracks/playabletrack/wavetrack/ui/SpectrumVRulerControls.h
      tracks/playabletrack/wavetrack/ui/SpectrumVZoomHandle.cpp
      tracks/playabletrack/wavetrack/ui/SpectrumVZoomHandle.h
      tracks/playabletrack/wavetrack/ui/SpectrumView.cpp
      tracks/playabletrack/wavetrack/ui/SpectrumView.h
      tracks/playabletrack/wavetrack/ui/WaveChannelVRulerControls.cpp
      tracks/playabletrack/wavetrack/ui/WaveChannelVRulerControls.h
      tracks/playabletrack/wavetrack/ui/WaveChannelVZoomHandle.cpp
      tracks/playabletrack/wavetrack/ui/WaveChannelVZoomHandle.h
      tracks/playabletrack/wavetrack/ui/WaveChannelView.cpp
      tracks/playabletrack/wavetrack/ui/WaveChannelView.h
      tracks/playabletrack/wavetrack/ui/WaveClipAdjustBorderHandle.h
      tracks/playabletrack/wavetrack/ui/WaveClipAdjustBorderHandle.cpp
      tracks/playabletrack/wavetrack/ui/WaveClipUIUtilities.cpp
      tracks/playabletrack/wavetrack/ui/WaveClipUIUtilities.h
      tracks/playabletrack/wavetrack/ui/WaveformAppearance.cpp
      tracks/playabletrack/wavetrack/ui/WaveformAppearance.h
      tracks/playabletrack/wavetrack/ui/WaveTrackAffordanceControls.cpp
      tracks/playabletrack/wavetrack/ui/WaveTrackAffordanceControls.h
      tracks/playabletrack/wavetrack/ui/WaveTrackAffordanceHandle.cpp
      tracks/playabletrack/wavetrack/ui/WaveTrackAffordanceHandle.h
      tracks/playabletrack/wavetrack/ui/WaveTrackControls.cpp
      tracks/playabletrack/wavetrack/ui/WaveTrackControls.h
      tracks/playabletrack/wavetrack/ui/WaveTrackMenuItems.cpp
      tracks/playabletrack/wavetrack/ui/WaveTrackShifter.cpp
      tracks/playabletrack/wavetrack/ui/WaveTrackSliderHandles.cpp
      tracks/playabletrack/wavetrack/ui/WaveTrackSliderHandles.h
      tracks/playabletrack/wavetrack/ui/WaveformVRulerControls.cpp
      tracks/playabletrack/wavetrack/ui/WaveformVRulerControls.h
      tracks/playabletrack/wavetrack/ui/WaveformVZoomHandle.cpp
      tracks/playabletrack/wavetrack/ui/WaveformVZoomHandle.h
      tracks/playabletrack/wavetrack/ui/WaveformView.cpp
      tracks/playabletrack/wavetrack/ui/WaveformView.h
      tracks/timetrack/ui/TimeTrackControls.cpp
      tracks/timetrack/ui/TimeTrackControls.h
      tracks/timetrack/ui/TimeTrackMenuItems.cpp
      tracks/timetrack/ui/TimeTrackVRulerControls.cpp
      tracks/timetrack/ui/TimeTrackVRulerControls.h
      tracks/timetrack/ui/TimeTrackVZoomHandle.cpp
      tracks/timetrack/ui/TimeTrackVZoomHandle.h
      tracks/timetrack/ui/TimeTrackView.cpp
      tracks/timetrack/ui/TimeTrackView.h

      # Tracks UI

      tracks/ui/AffordanceHandle.cpp
      tracks/ui/AffordanceHandle.h
      tracks/ui/BackgroundCell.cpp
      tracks/ui/BackgroundCell.h
      tracks/ui/BrushHandle.cpp
      tracks/ui/BrushHandle.h
      tracks/ui/ButtonHandle.cpp
      tracks/ui/ButtonHandle.h
      tracks/ui/ChannelVRulerControls.cpp
      tracks/ui/ChannelVRulerControls.h
      tracks/ui/ChannelView.cpp
      tracks/ui/ChannelView.h
      tracks/ui/CommonChannelView.cpp
      tracks/ui/CommonChannelView.h
      tracks/ui/CommonTrackControls.cpp
      tracks/ui/CommonTrackControls.h
      tracks/ui/CommonTrackInfo.cpp
      tracks/ui/CommonTrackInfo.h
      tracks/ui/CommonTrackPanelCell.cpp
      tracks/ui/CommonTrackPanelCell.h
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
      tracks/ui/TextEditHelper.cpp
      tracks/ui/TextEditHelper.h
      tracks/ui/TimeShiftHandle.cpp
      tracks/ui/TimeShiftHandle.h
      tracks/ui/TrackButtonHandles.cpp
      tracks/ui/TrackButtonHandles.h
      tracks/ui/TrackControls.cpp
      tracks/ui/TrackControls.h
      tracks/ui/TrackSelectHandle.cpp
      tracks/ui/TrackSelectHandle.h
      tracks/ui/ZoomHandle.cpp
      tracks/ui/ZoomHandle.h

      # Widgets

      widgets/AButton.cpp
      widgets/AButton.h
      widgets/ASlider.cpp
      widgets/ASlider.h
      widgets/AttachableScrollBar.cpp
      widgets/AttachableScrollBar.h
      widgets/BackedPanel.cpp
      widgets/BackedPanel.h
      widgets/BasicMenu.cpp
      widgets/BasicMenu.h
      widgets/BeatsFormat.cpp
      widgets/BeatsFormat.h
      widgets/CustomUpdater.cpp
      widgets/CustomUpdater.h
      widgets/CustomUpdaterPosition.cpp
      widgets/CustomUpdaterPosition.h
      widgets/CustomUpdaterValue.cpp
      widgets/CustomUpdaterValue.h
      widgets/FileHistory.cpp
      widgets/FileHistory.h
      widgets/FreqGauge.cpp
      widgets/FreqGauge.h
      widgets/GeneratedUpdater.cpp
      widgets/GeneratedUpdater.h
      widgets/Grabber.cpp
      widgets/Grabber.h
      widgets/Grid.cpp
      widgets/Grid.h
      widgets/ImageRoll.cpp
      widgets/ImageRoll.h
      widgets/IntFormat.cpp
      widgets/IntFormat.h
      widgets/KeyView.cpp
      widgets/KeyView.h
      widgets/LinearDBFormat.cpp
      widgets/LinearDBFormat.h
      widgets/LinearUpdater.cpp
      widgets/LinearUpdater.h
      widgets/LogarithmicUpdater.cpp
      widgets/LogarithmicUpdater.h
      widgets/MeterPanel.cpp
      widgets/MeterPanel.h
      widgets/MeterPanelBase.cpp
      widgets/MeterPanelBase.h
      widgets/MissingPluginsErrorDialog.cpp
      widgets/MissingPluginsErrorDialog.h
      widgets/NumericTextCtrl.cpp
      widgets/NumericTextCtrl.h
      widgets/Overlay.cpp
      widgets/Overlay.h
      widgets/OverlayPanel.cpp
      widgets/OverlayPanel.h
      widgets/PopupMenuTable.cpp
      widgets/PopupMenuTable.h
      widgets/RealFormat.cpp
      widgets/RealFormat.h
      widgets/Ruler.cpp
      widgets/Ruler.h
      widgets/RulerFormat.cpp
      widgets/RulerFormat.h
      widgets/RulerPanel.cpp
      widgets/RulerPanel.h
      widgets/RulerUpdater.cpp
      widgets/RulerUpdater.h
      widgets/TimeFormat.cpp
      widgets/TimeFormat.h
      widgets/UnwritableLocationErrorDialog.cpp
      widgets/UnwritableLocationErrorDialog.h
      widgets/Warning.cpp
      widgets/Warning.h
      widgets/auStaticText.cpp
      widgets/auStaticText.h
      widgets/numformatter.cpp
      widgets/numformatter.h
      widgets/valnum.cpp
      widgets/valnum.h

      # Update version
      $<$<BOOL:${${_OPT}has_updates_check}>:
          update/VersionId.h
          update/VersionId.cpp
          update/VersionPatch.h
          update/UpdateDataParser.h
          update/UpdateDataParser.cpp
          update/UpdateManager.h
          update/UpdateManager.cpp
          update/UpdateNoticeDialog.h
          update/UpdateNoticeDialog.cpp
          update/UpdatePopupDialog.h
          update/UpdatePopupDialog.cpp
          prefs/ApplicationPrefs.h
          prefs/ApplicationPrefs.cpp
          update/NoUpdatesAvailableDialog.h
          update/NoUpdatesAvailableDialog.cpp
      >

      Experimental.cmake
)

#
#
#
set( HEADERS
   ../include/audacity/Types.h
)

#
#
#
set( INCLUDES
   PUBLIC
      ${_PRVDIR}
      ${topdir}/include
      ${TARGET_ROOT}
)

#
# Define resources
#
set( RESOURCES
   ${topdir}/resources/EffectsMenuDefaults.xml
)

#
#
#
set( DEFINES
   PRIVATE
      BUILDING_AUDACITY
      WXUSINGDLL
      CMAKE
      $<$<BOOL:${HAVE_LRINT}>:
         HAVE_LRINT
      >
      $<$<BOOL:${HAVE_LRINTF}>:
         HAVE_LRINTF
      >
      $<$<BOOL:${HAVE_MLOCK}>:
         HAVE_MLOCK
      >
      $<$<PLATFORM_ID:CYGWIN>:
         _GNU_SOURCE=1
      >
      $<$<PLATFORM_ID:Windows>:
         _CRT_SECURE_NO_WARNINGS
         __STDC_CONSTANT_MACROS
         STRICT
      >
      $<$<BOOL:${${_OPT}has_updates_check}>:
          HAVE_UPDATES_CHECK
      >
)

# If we have cmake 3.16 or higher, we can use precompiled headers, but
# only use them if ccache is not available and the user hasn't disabled
# it.
if( CMAKE_VERSION VERSION_GREATER_EQUAL "3.16" AND NOT CCACHE_PROGRAM )
   cmd_option(
      ${_OPT}use_pch
      "Use precompiled headers [yes, no]"
      YES
   )
endif()

audacity_append_common_compiler_options( OPTIONS "${${_OPT}use_pch}" )

set( LDFLAGS
   PRIVATE
      $<$<CXX_COMPILER_ID:MSVC>:/MANIFEST:NO>
      $<$<CXX_COMPILER_ID:GNU>:-Wl,--disable-new-dtags>
)

find_package( Threads QUIET )
#
#
#
set( LIBRARIES
   PUBLIC
      ${CMAKE_REQUIRED_LIBRARIES}
      ZLIB::ZLIB
      $<$<BOOL:${USE_VAMP}>:libvamp>
      $<$<PLATFORM_ID:Linux,FreeBSD,OpenBSD,NetBSD,CYGWIN>:PkgConfig::GLIB>
      $<$<PLATFORM_ID:Linux,FreeBSD,OpenBSD,NetBSD,CYGWIN>:PkgConfig::GTK>
      $<$<TARGET_EXISTS:Threads::Threads>:Threads::Threads>
)

if( ${_OPT}has_crashreports )
   add_dependencies(${TARGET} crashreporter)
   add_custom_command(
      TARGET
         ${TARGET}
      COMMAND
         ${CMAKE_COMMAND} -E copy $<TARGET_FILE:crashreporter> ${_DESTDIR}/${_EXEDIR}/$<TARGET_FILE_NAME:crashreporter>
      POST_BUILD
   )
   if(CMAKE_SYSTEM_NAME MATCHES "Darwin")
      add_custom_command(
         TARGET
            ${TARGET}
         COMMAND
            ${PYTHON}
            ${CMAKE_SOURCE_DIR}/scripts/build/macOS/fix_bundle.py
            ${_DESTDIR}/${_EXEDIR}/$<TARGET_FILE_NAME:crashreporter>
         POST_BUILD
      )
   endif()
endif()

set( BUILDING_AUDACITY YES )
set( PKGLIBDIR "${_PKGLIBDIR}" )
set( LIBDIR "${CMAKE_INSTALL_FULL_LIBDIR}" )
set( HAVE_GTK ${GTK_FOUND} )

# Do not define these for Windows or Mac until further testing
# can be done against ExportMP3.cpp.  If either are defined, the
# build will fail.
if( NOT CMAKE_SYSTEM_NAME MATCHES "Windows|Darwin" )
   if( "${${_OPT}use_lame}" STREQUAL "local" )
      set( DISABLE_DYNAMIC_LOADING_LAME YES )
   endif()
endif()

if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   # Define the Windows specific resources
   set( WIN_RESOURCES
      ../win/audacity.rc
   )

   set_target_properties(
      ${TARGET}
      PROPERTIES
         # Make sure we're a GUI application and not a console tool
         WIN32_EXECUTABLE ON
         # Add the resources
         RESOURCE "${WIN_RESOURCES}"
   )

   # Copy the VC runtime libraries as well
   add_custom_command(
      TARGET
         ${TARGET}
      COMMAND
         ${CMAKE_COMMAND} -E copy ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS} ${_DEST}
      POST_BUILD
   )

   # Copy resource files into build folder
   add_custom_command(
      TARGET
         ${TARGET}
      COMMAND
         ${CMAKE_COMMAND} -E copy ${RESOURCES} ${_DEST}
      POST_BUILD
   )
elseif( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   # Bug 2400 workaround
   #
   # Replaces the SDK version in the built executable with 10.13 to
   # prevent high CPU usage and slow drawing on Mojave or newer
   check_for_platform_version()
   if( PLATFORM_VERSION_SUPPORTED )
      list( APPEND LDFLAGS
         PRIVATE
            -Wl,-platform_version,macos,${MIN_MACOS_VERSION},${TARGET_MACOS_VERSION}
      )
   else()
      list( APPEND LDFLAGS
         PRIVATE
            -Wl,-sdk_version,10.13
      )
   endif()

   # Define Mac specific resources
   set( MAC_RESOURCES
      ../mac/Resources/Audacity.icns
      ../mac/Resources/AudacityAIFF.icns
      ../mac/Resources/AudacityAU.icns
      ../mac/Resources/AudacityMP3.icns
      ../mac/Resources/AudacityOGG.icns
      ../mac/Resources/AudacityProject.icns
      ../mac/Resources/AudacityWAV.icns
   )

   set_target_properties(
      ${TARGET}
      PROPERTIES
         # Build a bundle
         MACOSX_BUNDLE ON
         # Add the resources
         RESOURCE "${MAC_RESOURCES};${RESOURCES}"
   )
   set_target_property_all( ${TARGET} RUNTIME_OUTPUT_DIRECTORY "${_DESTDIR}" )

   # These sources must be compiled as Objective C++ and excluded
   # from precompiled header usage
   set_source_files_properties(
      AudacityApp.mm
      KeyboardCapture.cpp
      effects/EffectUI.mm
      effects/VST/VSTControlOSX.mm
      effects/audiounits/AUControl.mm
      widgets/FileDialog/mac/FileDialogPrivate.mm
      PROPERTIES
         COMPILE_FLAGS "-x objective-c++"
         SKIP_PRECOMPILE_HEADERS YES
   )
   if(${_OPT}has_vst3 AND ${wxIS_MAC})
      set_source_files_properties(
         effects/VST/VSTControlOSX.mm
         PROPERTIES
            SKIP_PRECOMPILE_HEADERS YES
      )
   endif()

   # Add our required frameworks
   list( APPEND LIBRARIES
      PRIVATE
         "-framework CoreAudio"
         "-framework CoreAudioKit"
   )

   # Define the Wrapper target
   if( CMAKE_CONFIGURATION_TYPES )
      set( WRAPPER_EXEDIR "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Wrapper" )
   else()
      set( WRAPPER_EXEDIR "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Wrapper" )
   endif()

   set( WRAPPER_ROOT "${TARGET_ROOT}/../mac" )
   set( WRAPPER_SOURCES "${WRAPPER_ROOT}/Wrapper.c" )

   add_executable( Wrapper "${WRAPPER_SOURCES}" )
   add_dependencies( ${TARGET} Wrapper )

   set_target_property_all( Wrapper RUNTIME_OUTPUT_DIRECTORY "${WRAPPER_EXEDIR}" )
   organize_source( "${WRAPPER_ROOT}" "mac" "${WRAPPER_SOURCES}" )

   add_custom_command(
      TARGET
         ${TARGET}
      COMMAND
         ${CMAKE_COMMAND} -E copy $<TARGET_FILE:Wrapper> "${_DESTDIR}/${_EXEDIR}/Wrapper"
      POST_BUILD
   )

   fix_bundle( ${TARGET} )

   # Because the wrapper is copied into the bundle, we need to
   # re-sing the bundle as the last post build step.
   # This is only required when using the Xcode generator with
   # the  Xcode >= 14.
   if (XCODE_VERSION VERSION_GREATER_EQUAL 14)
      add_custom_command(
         TARGET
            ${TARGET}
         COMMAND
            ${CMAKE_COMMAND} -E echo "Fixing Xcode 14 code signing issues"
         COMMAND
            codesign --force --timestamp --deep --sign - "${_DESTDIR}/Audacity.app"
         POST_BUILD
      )
   endif()
else()
   set_target_properties(
      ${TARGET}
      PROPERTIES
         # Add the resources
         RESOURCE "${RESOURCES}"
   )
   set_target_property_all( ${TARGET} RUNTIME_OUTPUT_DIRECTORY "${_DESTDIR}/${_EXEDIR}" )

   # Build the list of mimetypes
   #
   # (Don't use generator expressions since it will leave null/empty
   # entries in the list.)
   set( MIMETYPES
      application/x-audacity-project
      application/x-audacity-project+sqlite3
      audio/basic
      audio/x-aiff
      audio/x-wav
   )
   if( USE_FFMPEG )
      list( APPEND MIMETYPES
         audio/aac
         audio/ac3
         audio/mp4
         audio/x-ms-wma
         video/mpeg
      )
   endif()
   if( USE_LIBFLAC )
      list( APPEND MIMETYPES
         audio/flac
         audio/x-flac
      )
   endif()
   if( USE_LIBMPG123)
      list( APPEND MIMETYPES
         audio/mpeg
      )
   endif()
   if( USE_LIBOGG AND USE_LIBVORBIS )
      list( APPEND MIMETYPES
         application/ogg
         audio/x-vorbis+ogg
      )
   endif()

   # Create the desktop file
   configure_file( audacity.desktop.in ${_INTDIR}/audacity.desktop )
   
   if(${_OPT}has_url_schemes_support)
      configure_file( audacity-url-handler.desktop.in ${_INTDIR}/audacity-url-handler.desktop )
   endif()

   if( CMAKE_SYSTEM_NAME MATCHES "Linux" )
      add_executable(findlib ../linux/findlib.c)
      target_link_libraries(findlib ${CMAKE_DL_LIBS})
      set_target_property_all( findlib SKIP_BUILD_RPATH On )
      set_target_property_all( findlib RUNTIME_OUTPUT_DIRECTORY "${_DESTDIR}/${_EXEDIR}" )
   endif()
endif()

if(CRASH_REPORT_URL)
    list(APPEND DEFINES PRIVATE -DCRASH_REPORT_URL="${CRASH_REPORT_URL}")
endif()

if(${_OPT}has_whats_new)
   list(APPEND DEFINES PRIVATE HAS_WHATS_NEW )
endif()

set_target_property_all( ${TARGET} RUNTIME_OUTPUT_NAME ${AUDACITY_NAME} )

organize_source( "${TARGET_ROOT}/.." "include" "${HEADERS}" )
organize_source( "${TARGET_ROOT}/../resources" "resources" "${RESOURCES}" )
organize_source( "${TARGET_ROOT}" "src" "${SOURCES}" )
organize_source( "${TARGET_ROOT}/../mac/Resources" "mac" "${MAC_RESOURCES}" )
organize_source( "${TARGET_ROOT}/../win" "win" "${WIN_RESOURCES}" )

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

# Define AUDACITY_DLL_API
import_symbol_define( import_symbol AUDACITY_DLL )
export_symbol_define( export_symbol AUDACITY_DLL )
list( APPEND DEFINES PRIVATE "${export_symbol}" INTERFACE "${import_symbol}" )

# see AudioIO.cpp
check_function_exists(clock_gettime HAVE_CLOCK_GETTIME)
if(HAVE_CLOCK_GETTIME)
   list( APPEND DEFINES HAVE_CLOCK_GETTIME )
endif()

set( AUDACITY_LIBRARIES
# A sub-sequence of what is in libraries/CMakeLists.txt :
   lib-theme-resources-interface
   lib-graphics-interface
   lib-tags-interface
   lib-wx-init-interface
   lib-time-track-interface
   lib-stretching-sequence-interface
   lib-import-export-interface
   lib-export-ui-interface
   lib-wave-track-paint-interface
   lib-wave-track-fft-interface
   lib-music-information-retrieval-interface
   lib-preference-pages-interface
   lib-audacity-application-logic-interface
   lib-builtin-effects-interface
)

if (USE_VST)
   list( APPEND AUDACITY_LIBRARIES
      lib-vst-interface
   )
endif()

if (USE_LV2)
   list( APPEND AUDACITY_LIBRARIES
      lib-lv2-interface
   )
endif()

if (${_OPT}use_ladspa)
   list( APPEND AUDACITY_LIBRARIES
      lib-ladspa-interface
   )
endif()

if (${_OPT}has_vst3)
   list( APPEND AUDACITY_LIBRARIES
      lib-vst3-interface
   )
endif()

if (USE_AUDIO_UNITS AND wxIS_MAC)
   list(APPEND AUDACITY_LIBRARIES lib-audio-unit-interface)
endif()

if ( ${_OPT}has_networking )
   list( APPEND AUDACITY_LIBRARIES
      lib-network-manager-interface
   )
endif()

if( ${_OPT}has_url_schemes_support )
   list( APPEND AUDACITY_LIBRARIES
      lib-url-schemes-interface
   )
endif()

if( ${_OPT}has_audiocom_upload )
   list( APPEND AUDACITY_LIBRARIES
      lib-cloud-audiocom-interface
   )
endif()

if( ${_OPT}has_crashreports )
   if( ${_OPT}crashreport_backend STREQUAL "crashpad" )
      list(APPEND DEFINES
         PRIVATE
            CRASHREPORTER_NAME="$<TARGET_FILE_NAME:crashreporter>"
            CRASHPAD_HANDLER_NAME="crashpad_handler${CMAKE_EXECUTABLE_SUFFIX}"
      )
      list( APPEND AUDACITY_LIBRARIES
         lib-crashpad-configurer-interface
      )
   elseif( ${_OPT}crashreport_backend STREQUAL "breakpad" )
      list( APPEND AUDACITY_LIBRARIES
         lib-breakpad-configurer-interface
      )
   endif()
endif()

if( ${_OPT}use_nyquist )
   list( APPEND AUDACITY_LIBRARIES
      lib-nyquist-effects-interface
   )
endif()

propagate_interesting_dependencies( ${TARGET} "${AUDACITY_LIBRARIES}" )

target_sources( ${TARGET} PRIVATE ${HEADERS} ${SOURCES} ${RESOURCES} ${MAC_RESOURCES} ${WIN_RESOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_options( ${TARGET} PRIVATE ${LDFLAGS} )
target_link_libraries( ${TARGET} PUBLIC ${AUDACITY_LIBRARIES} )
target_link_libraries( ${TARGET} ${LIBRARIES} )

if( CMAKE_VERSION VERSION_GREATER_EQUAL "3.16" AND NOT CCACHE_PROGRAM )
   if( ${_OPT}use_pch )
      message( STATUS "Using precompiled headers" )
      target_precompile_headers( ${TARGET} PRIVATE
         $<$<PLATFORM_ID:Windows>:${CMAKE_BINARY_DIR}/src/private/configwin.h>
         $<$<PLATFORM_ID:Darwin>:${CMAKE_BINARY_DIR}/src/private/configmac.h>
         $<$<NOT:$<PLATFORM_ID:Windows,Darwin>>:${CMAKE_BINARY_DIR}/src/private/configunix.h>
         AudacityHeaders.h
      )
   else()
      message( STATUS "Not using precompiled headers" )
   endif()
endif()

if( "${CMAKE_GENERATOR}" MATCHES "Xcode|Visual Studio*" )
   install(
      DIRECTORY "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/"
      DESTINATION "."
      USE_SOURCE_PERMISSIONS
      PATTERN "*.pdb" EXCLUDE
      PATTERN "*.ilk" EXCLUDE
      PATTERN "*.dSYM" EXCLUDE
      PATTERN "image-compiler" EXCLUDE
      PATTERN "image-compiler.exe" EXCLUDE
      PATTERN "riff-test-util" EXCLUDE
      PATTERN "riff-test-util.exe" EXCLUDE
   )
else()
   if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
      install( TARGETS ${TARGET}
               DESTINATION "."
               RESOURCE DESTINATION "${_APPDIR}/Resources" )
   else()

      install( TARGETS ${TARGET}
               RUNTIME
               RESOURCE DESTINATION "${_PKGDATA}" )

      install( DIRECTORY "${_DEST}/${_LIBDIR}/"
               DESTINATION "${_LIBDIR}"
               USE_SOURCE_PERMISSIONS
               FILES_MATCHING PATTERN "*.so*" )

      install( FILES "${_INTDIR}/audacity.desktop"
               DESTINATION "${_DATADIR}/applications" )

      if(${_OPT}has_url_schemes_support)
         install( FILES "${_INTDIR}/audacity-url-handler.desktop"
                  DESTINATION "${_DATADIR}/applications" )
      endif()
               
      install( FILES "${topdir}/LICENSE.txt" "${topdir}/README.md"
               DESTINATION "${_DATADIR}/doc/${AUDACITY_NAME}" )
      install( FILES "${_SRCDIR}/audacity.xml"
               DESTINATION "${_DATADIR}/mime/packages" )

      if( ${_OPT}has_crashreports )
         if( ${_OPT}crashreport_backend STREQUAL "crashpad" )
            install( PROGRAMS  "${_DESTDIR}/${_EXEDIR}/crashpad_handler"
                     DESTINATION "${CMAKE_INSTALL_BINDIR}" )
         endif()
      endif()
   endif()
endif()

# collect dependency information for third party libraries
set( AUDACITY_NODE_ATTRIBUTES "shape=house" )
append_node_attributes( AUDACITY_NODE_ATTRIBUTES ${TARGET} )
list( APPEND GRAPH_EDGES "Audacity [${AUDACITY_NODE_ATTRIBUTES}]" )
foreach( LIBRARY ${LIBRARIES} ${AUDACITY_LIBRARIES} )
   if (NOT LIBRARY MATCHES "PUBLIC|PRIVATE|INTERFACE")
      canonicalize_node_name(LIBRARY "${LIBRARY}")
      list( APPEND GRAPH_EDGES "\"${TARGET}\" -> \"${LIBRARY}\"" )
   endif()
endforeach()

#propagate collected edgets to root CMakeLists.txt
set( GRAPH_EDGES "${GRAPH_EDGES}" PARENT_SCOPE )
