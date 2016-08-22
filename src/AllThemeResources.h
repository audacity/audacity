/**********************************************************************

  Audacity: A Digital Audio Editor

  AllThemeResources.h

  James Crook

  Audacity is free software.
  This file is licensed under the GPL license, see License.txt

********************************************************************//**

\file AllThemeResources.h

This file contains definitions of all images, cursors, colours, fonts
and grids used by Audacity.

This will be split up into separate include files to reduce the amount
of recompilation on a change.

Meantime, do NOT DELETE any of these declarations, even if they're
unused, as they're all offset by prior declarations.

To add an image, you give its size and name like so:

\code
   DEFINE_IMAGE( bmpPause, wxImage( 16, 16 ), wxT("Pause"));
\endcode

If you do this and run the program the image will be black to start
with, but you can go into ThemePrefs and load it (load components)
from there.  Audacity will look for a file called "Pause.png".

 - Now save into ImageCache.
 - From here on you can get the image by loading ImageCache.
 - To burn it into the program defaults, use the
 'Output Sourcery' button.

\see \ref Themability in DOxygen documentation for more details.

*//*******************************************************************/

// Note: No '#ifndef/#define' pair on this header file.
// we want to include it multiple times in Theme.cpp.

#include "Experimental.h"
#include "MacroMagic.h"
#define XPMS_RETIRED

   SET_THEME_FLAGS(  resFlagPaired  );
   DEFINE_IMAGE( bmpPause, wxImage( 16, 16 ), wxT("Pause"));
   DEFINE_IMAGE( bmpPauseDisabled, wxImage( 16, 16 ), wxT("PauseDisabled"));
   DEFINE_IMAGE( bmpPlay, wxImage( 16, 16 ), wxT("Play"));
   DEFINE_IMAGE( bmpPlayDisabled, wxImage( 16, 16 ), wxT("PlayDisabled"));
   DEFINE_IMAGE( bmpLoop, wxImage( 16, 16 ), wxT("Loop"));
   DEFINE_IMAGE( bmpLoopDisabled, wxImage( 16, 16 ), wxT("LoopDisabled"));
   DEFINE_IMAGE( bmpStop, wxImage( 16, 16 ), wxT("Stop"));
   DEFINE_IMAGE( bmpStopDisabled, wxImage( 16, 16 ), wxT("StopDisabled"));
   DEFINE_IMAGE( bmpRewind, wxImage( 16, 16 ), wxT("Rewind"));
   DEFINE_IMAGE( bmpRewindDisabled, wxImage( 16, 16 ), wxT("RewindDisabled"));
   DEFINE_IMAGE( bmpFFwd, wxImage( 16, 16 ), wxT("FFwd"));
   DEFINE_IMAGE( bmpFFwdDisabled, wxImage( 16, 16 ), wxT("FFwdDisabled"));
   DEFINE_IMAGE( bmpRecord, wxImage( 16, 16 ), wxT("Record"));
   DEFINE_IMAGE( bmpRecordDisabled, wxImage( 16, 16 ), wxT("RecordDisabled"));
   DEFINE_IMAGE( bmpCutPreview, wxImage( 16, 16 ), wxT("CutPreview"));
   DEFINE_IMAGE( bmpCutPreviewDisabled, wxImage( 16, 16 ), wxT("CutPreviewDisabled"));
   DEFINE_IMAGE( bmpAppendRecord, wxImage( 16, 16 ), wxT("AppendRecord"));
   DEFINE_IMAGE( bmpAppendRecordDisabled, wxImage( 16, 16 ), wxT("AppendRecordDisabled"));
   DEFINE_IMAGE( bmpScrubDisabled, wxImage( 18, 16 ), wxT("ScrubDisabled"));
   DEFINE_IMAGE( bmpScrub, wxImage( 18, 16 ), wxT("Scrub"));
   DEFINE_IMAGE( bmpSeekDisabled, wxImage( 26, 16 ), wxT("SeekDisabled"));
   DEFINE_IMAGE( bmpSeek, wxImage( 26, 16 ), wxT("Seek"));

   SET_THEME_FLAGS(  resFlagNewLine  );
   DEFINE_IMAGE( bmpUpButtonLarge, wxImage( 48, 48 ), wxT("UpButtonLarge"));
   DEFINE_IMAGE( bmpDownButtonLarge, wxImage( 48, 48 ), wxT("DownButtonLarge"));
   DEFINE_IMAGE( bmpHiliteButtonLarge, wxImage( 48, 48 ), wxT("HiliteButtonLarge"));
   DEFINE_IMAGE( bmpUpButtonSmall, wxImage( 27, 27 ), wxT("UpButtonSmall"));
   DEFINE_IMAGE( bmpDownButtonSmall, wxImage( 27, 27 ), wxT("DownButtonSmall"));
   DEFINE_IMAGE( bmpHiliteButtonSmall, wxImage( 27, 27 ), wxT("HiliteButtonSmall"));
   DEFINE_IMAGE( bmpVolumeSlider, wxImage( 100, 28 ), wxT("VolumeSlider"));
   DEFINE_IMAGE( bmpVolumeSliderThumb, wxImage( 10, 28 ), wxT("VolumeSliderThumb"));
   DEFINE_IMAGE( bmpSliderThumb, wxImage( 11, 14 ), wxT("SliderThumb"));
   DEFINE_IMAGE( bmpToggleScrubRuler, wxImage( 20, 20 ), wxT("ToggleScrubBar"));

   SET_THEME_FLAGS(  resFlagNone  );
   DEFINE_IMAGE( bmpMacUpButton, wxImage( 36, 36 ), wxT("MacUpButton"));
   DEFINE_IMAGE( bmpMacDownButton, wxImage( 36, 36 ), wxT("MacDownButton"));
   DEFINE_IMAGE( bmpMacHiliteButton, wxImage( 36, 36 ), wxT("MacHiliteButton"));
   DEFINE_IMAGE( bmpMacUpButtonSmall, wxImage( 27, 27 ), wxT("MacUpButtonSmall"));
   DEFINE_IMAGE( bmpMacDownButtonSmall, wxImage( 27, 27 ), wxT("MacDownButtonSmall"));
   DEFINE_IMAGE( bmpMacHiliteButtonSmall, wxImage( 27, 27 ), wxT("MacHiliteButtonSmall"));
   DEFINE_IMAGE( bmpMacSlider, wxImage( 100, 28 ), wxT("MacSlider"));
   DEFINE_IMAGE( bmpMacSliderThumb, wxImage( 17, 28 ), wxT("MacSliderThumb"));

   SET_THEME_FLAGS(  resFlagInternal  );
   DEFINE_IMAGE( bmpRecoloredUpLarge, wxImage( 48, 48 ), wxT("RecoloredUpLarge"));
   DEFINE_IMAGE( bmpRecoloredDownLarge, wxImage( 48, 48 ), wxT("RecoloredDownLarge"));
   DEFINE_IMAGE( bmpRecoloredHiliteLarge, wxImage( 48, 48 ), wxT("RecoloredHiliteLarge"));
   DEFINE_IMAGE( bmpRecoloredUpSmall, wxImage( 27, 27 ), wxT("RecoloredUpSmall"));
   DEFINE_IMAGE( bmpRecoloredDownSmall, wxImage( 27, 27 ), wxT("RecoloredDownSmall"));
   DEFINE_IMAGE( bmpRecoloredHiliteSmall, wxImage( 27, 27 ), wxT("RecoloredHiliteSmall"));

   SET_THEME_FLAGS(  resFlagCursor  );
   DEFINE_IMAGE( bmpIBeamCursor, wxImage( 32, 32 ), wxT("IBeamCursor"));
   DEFINE_IMAGE( bmpDrawCursor, wxImage( 32, 32 ), wxT("DrawCursor"));
   DEFINE_IMAGE( bmpEnvCursor, wxImage( 32, 32 ), wxT("EnvCursor"));
   DEFINE_IMAGE( bmpTimeCursor, wxImage( 32, 32 ), wxT("TimeCursor"));
   DEFINE_IMAGE( bmpZoomInCursor, wxImage( 32, 32 ), wxT("ZoomInCursor"));
   DEFINE_IMAGE( bmpZoomOutCursor, wxImage( 32, 32 ), wxT("ZoomOutCursor"));
   DEFINE_IMAGE( bmpLabelCursorLeft, wxImage( 32, 32 ), wxT("LabelCursorLeft"));
   DEFINE_IMAGE( bmpLabelCursorRight, wxImage( 32, 32 ), wxT("LabelCursorRight"));
   DEFINE_IMAGE( bmpDisabledCursor, wxImage( 32, 32 ), wxT("DisabledCursor"));
   DEFINE_IMAGE( bmpBottomFrequencyCursor, wxImage( 32, 32 ), wxT("BottomFrequencyCursor"));
   DEFINE_IMAGE( bmpTopFrequencyCursor, wxImage( 32, 32 ), wxT("TopFrequencyCursor"));
   DEFINE_IMAGE( bmpBandWidthCursor, wxImage(32, 32), wxT("BandWidthCursor"));

   SET_THEME_FLAGS(  resFlagNone  );
   DEFINE_IMAGE( bmpIBeam, wxImage( 27, 27 ), wxT("IBeam"));
   DEFINE_IMAGE( bmpZoom, wxImage( 27, 27 ), wxT("Zoom"));
   DEFINE_IMAGE( bmpEnvelope, wxImage( 27, 27 ), wxT("Envelope"));
   DEFINE_IMAGE( bmpTimeShift, wxImage( 27, 27 ), wxT("TimeShift"));
   DEFINE_IMAGE( bmpDraw, wxImage( 27, 27 ), wxT("Draw"));
   DEFINE_IMAGE( bmpMulti, wxImage( 27, 27 ), wxT("Multi"));
   DEFINE_IMAGE( bmpMic, wxImage( 25, 25 ), wxT("Mic"));
   DEFINE_IMAGE( bmpSpeaker, wxImage( 25, 25 ), wxT("Speaker"));

   DEFINE_IMAGE( bmpPinnedPlayHead, wxImage( 27, 27 ), wxT("PinnedPlayHead"));
   DEFINE_IMAGE( bmpUnpinnedPlayHead, wxImage( 27, 27 ), wxT("UnpinnedPlayHead"));
   DEFINE_IMAGE( bmpPinnedRecordHead, wxImage( 27, 27 ), wxT("PinnedRecordHead"));
   DEFINE_IMAGE( bmpUnpinnedRecordHead, wxImage( 27, 27 ), wxT("UnpinnedRecordHead"));

   SET_THEME_FLAGS(  resFlagPaired  );
   DEFINE_IMAGE( bmpZoomFit, wxImage( 27, 27 ), wxT("ZoomFit"));
   DEFINE_IMAGE( bmpZoomFitDisabled, wxImage( 27, 27 ), wxT("ZoomFitDisabled"));
   DEFINE_IMAGE( bmpZoomIn, wxImage( 27, 27 ), wxT("ZoomIn"));
   DEFINE_IMAGE( bmpZoomInDisabled, wxImage( 27, 27 ), wxT("ZoomInDisabled"));
   DEFINE_IMAGE( bmpZoomOut, wxImage( 27, 27 ), wxT("ZoomOut"));
   DEFINE_IMAGE( bmpZoomOutDisabled, wxImage( 27, 27 ), wxT("ZoomOutDisabled"));
   DEFINE_IMAGE( bmpZoomSel, wxImage( 27, 27 ), wxT("ZoomSel"));
   DEFINE_IMAGE( bmpZoomSelDisabled, wxImage( 27, 27 ), wxT("ZoomSelDisabled"));
   DEFINE_IMAGE( bmpZoomToggle, wxImage( 27, 27 ), wxT("ZoomToggle"));
   DEFINE_IMAGE( bmpZoomToggleDisabled, wxImage( 27, 27 ), wxT("ZoomToggleDisabled"));
   DEFINE_IMAGE( bmpCut, wxImage( 26, 24 ), wxT("Cut"));
   DEFINE_IMAGE( bmpCutDisabled, wxImage( 26, 24 ), wxT("CutDisabled"));
   DEFINE_IMAGE( bmpCopy, wxImage( 26, 24 ), wxT("Copy"));
   DEFINE_IMAGE( bmpCopyDisabled, wxImage( 26, 24 ), wxT("CopyDisabled"));
   DEFINE_IMAGE( bmpPaste, wxImage( 26, 24 ), wxT("Paste"));
   DEFINE_IMAGE( bmpPasteDisabled, wxImage( 26, 24 ), wxT("PasteDisabled"));
   DEFINE_IMAGE( bmpTrim, wxImage( 26, 24 ), wxT("Trim"));
   DEFINE_IMAGE( bmpTrimDisabled, wxImage( 26, 24 ), wxT("TrimDisabled"));
   DEFINE_IMAGE( bmpSilence, wxImage( 26, 24 ), wxT("Silence"));
   DEFINE_IMAGE( bmpSilenceDisabled, wxImage( 26, 24 ), wxT("SilenceDisabled"));
   DEFINE_IMAGE( bmpUndo, wxImage( 26, 24 ), wxT("Undo"));
   DEFINE_IMAGE( bmpUndoDisabled, wxImage( 26, 24 ), wxT("UndoDisabled"));
   DEFINE_IMAGE( bmpRedo, wxImage( 26, 24 ), wxT("Redo"));
   DEFINE_IMAGE( bmpRedoDisabled, wxImage( 26, 24 ), wxT("RedoDisabled"));

   DEFINE_IMAGE( bmpSyncLockTracksDown, wxImage( 20, 20 ), wxT("SyncLockTracksDown"));
   DEFINE_IMAGE( bmpSyncLockTracksUp, wxImage( 20, 20 ), wxT("SyncLockTracksUp"));
   DEFINE_IMAGE( bmpSyncLockTracksDisabled, wxImage( 20, 20 ), wxT("SyncLockTracksDisabled"));

   SET_THEME_FLAGS(  resFlagPaired | resFlagNewLine  );
   DEFINE_IMAGE( bmpTnStartOn, wxImage( 27, 27 ), wxT("TnStartOn"));
   DEFINE_IMAGE( bmpTnStartOnDisabled, wxImage( 27, 27 ), wxT("TnStartOnDisabled"));
   DEFINE_IMAGE( bmpTnStartOff, wxImage( 27, 27 ), wxT("TnStartOff"));
   DEFINE_IMAGE( bmpTnStartOffDisabled, wxImage( 27, 27 ), wxT("TnStartOffDisabled"));
   DEFINE_IMAGE( bmpTnEndOn, wxImage( 27, 27 ), wxT("TnEndOn"));
   DEFINE_IMAGE( bmpTnEndOnDisabled, wxImage( 27, 27 ), wxT("TnEndOnDisabled"));
   DEFINE_IMAGE( bmpTnEndOff, wxImage( 27, 27 ), wxT("TnEndOff"));
   DEFINE_IMAGE( bmpTnEndOffDisabled, wxImage( 27, 27 ), wxT("TnEndOffDisabled"));
   DEFINE_IMAGE( bmpTnCalibrate, wxImage( 27, 27 ), wxT("TnCalibrate"));
   DEFINE_IMAGE( bmpTnCalibrateDisabled, wxImage( 27, 27 ), wxT("TnCalibrateDisabled"));
   DEFINE_IMAGE( bmpTnAutomateSelection, wxImage( 27, 27 ), wxT("TnAutomateSelection"));
   DEFINE_IMAGE( bmpTnAutomateSelectionDisabled, wxImage( 27, 27 ), wxT("TnAutomateSelectionDisabled"));
   DEFINE_IMAGE( bmpTnMakeTag, wxImage( 27, 27 ), wxT("TnMakeTag"));
   DEFINE_IMAGE( bmpTnMakeTagDisabled, wxImage( 27, 27 ), wxT("TnMakeTagDisabled"));
   DEFINE_IMAGE( bmpTnSelectSound, wxImage( 24, 24 ), wxT("TnSelectSound"));
   DEFINE_IMAGE( bmpTnSelectSoundDisabled, wxImage( 24, 24 ), wxT("TnSelectSoundDisabled"));
   DEFINE_IMAGE( bmpTnSelectSilence, wxImage( 24, 24 ), wxT("TnSelectSilence"));
   DEFINE_IMAGE( bmpTnSelectSilenceDisabled, wxImage( 24, 24 ), wxT("TnSelectSilenceDisabled"));

   SET_THEME_FLAGS(  resFlagNone  );
   DEFINE_IMAGE( bmpLabelGlyph0, wxImage( 15, 23 ), wxT("LabelGlyph0"));
   DEFINE_IMAGE( bmpLabelGlyph1, wxImage( 15, 23 ), wxT("LabelGlyph1"));
   DEFINE_IMAGE( bmpLabelGlyph2, wxImage( 15, 23 ), wxT("LabelGlyph2"));
   DEFINE_IMAGE( bmpLabelGlyph3, wxImage( 15, 23 ), wxT("LabelGlyph3"));
   DEFINE_IMAGE( bmpLabelGlyph4, wxImage( 15, 23 ), wxT("LabelGlyph4"));
   DEFINE_IMAGE( bmpLabelGlyph5, wxImage( 15, 23 ), wxT("LabelGlyph5"));
   DEFINE_IMAGE( bmpLabelGlyph6, wxImage( 15, 23 ), wxT("LabelGlyph6"));
   DEFINE_IMAGE( bmpLabelGlyph7, wxImage( 15, 23 ), wxT("LabelGlyph7"));
   DEFINE_IMAGE( bmpLabelGlyph8, wxImage( 15, 23 ), wxT("LabelGlyph8"));
   DEFINE_IMAGE( bmpLabelGlyph9, wxImage( 15, 23 ), wxT("LabelGlyph9"));
   DEFINE_IMAGE( bmpLabelGlyph10, wxImage( 15, 23 ), wxT("LabelGlyph10"));
   DEFINE_IMAGE( bmpLabelGlyph11, wxImage( 15, 23 ), wxT("LabelGlyph11"));

   SET_THEME_FLAGS(  resFlagNewLine  );
   DEFINE_IMAGE( bmpPostfishHome, wxImage( 19, 17 ), wxT("PostfishHome"));
   DEFINE_IMAGE( bmpPostfishFastRewind, wxImage( 17, 17 ), wxT("PostfishFastRewind"));
   DEFINE_IMAGE( bmpPostfishRewind, wxImage( 18, 17 ), wxT("PostfishRewind"));
   DEFINE_IMAGE( bmpPostfishPlay, wxImage( 29, 17 ), wxT("PostfishPlay"));
   DEFINE_IMAGE( bmpPostfishForward, wxImage( 18, 17 ), wxT("PostfishForward"));
   DEFINE_IMAGE( bmpPostfishFastForward, wxImage( 17, 17 ), wxT("PostfishFastForward"));
   DEFINE_IMAGE( bmpPostfishEnd, wxImage( 19, 17 ), wxT("PostfishEnd"));
   DEFINE_IMAGE( bmpPostfishLoop, wxImage( 29, 17 ), wxT("PostfishLoop"));

   SET_THEME_FLAGS(  resFlagNone  );
   DEFINE_IMAGE( bmpDockDown, wxImage( 15, 55 ), wxT("DockDown"));
   DEFINE_IMAGE( bmpDockDownShort, wxImage( 15, 27 ), wxT("DockDownShort"));
   DEFINE_IMAGE( bmpDockOver, wxImage( 15, 55 ), wxT("DockOver"));
   DEFINE_IMAGE( bmpDockOverShort, wxImage( 15, 27 ), wxT("DockOverShort"));
   DEFINE_IMAGE( bmpDockUp, wxImage( 15, 55 ), wxT("DockUp"));
   DEFINE_IMAGE( bmpDockUpShort, wxImage( 15, 27 ), wxT("DockUpShort"));
   DEFINE_IMAGE( bmpToolBarToggle, wxImage( 43, 35 ), wxT("ToolBarToggle"));
   DEFINE_IMAGE( bmpToolBarTarget, wxImage( 17, 26 ), wxT("ToolBarTarget"));
   DEFINE_IMAGE( bmpToolBarGrabber, wxImage( 17, 8 ), wxT("ToolBarGrabber"));
   DEFINE_IMAGE( bmpArrow, wxImage( 9, 16 ), wxT("Arrow"));
   DEFINE_IMAGE( bmpUploadFile, wxImage( 16, 16 ), wxT("UploadFile"));
   DEFINE_IMAGE( bmpUploadFolder, wxImage( 16, 16 ), wxT("UploadFolder"));
   DEFINE_IMAGE( bmpUploadMp3, wxImage( 16, 16 ), wxT("UploadMp3"));
   DEFINE_IMAGE( bmpUploadUp, wxImage( 16, 16 ), wxT("UploadUp"));

   SET_THEME_FLAGS(  resFlagNewLine  );

#define LOGOWITHNAME_WIDTH 506
#define LOGOWITHNAME_HEIGHT 200

   DEFINE_IMAGE( bmpAudacityLogo, wxImage( 215, 190 ), wxT("AudacityLogo"));
   DEFINE_IMAGE( bmpAudacityLogo48x48, wxImage( 48, 48 ), wxT("AudacityLogo48x48"));

   DEFINE_IMAGE( bmpSyncLockSelTile, wxImage(20, 22), wxT("SyncLockSelTile"));
   DEFINE_IMAGE( bmpSyncLockIcon, wxImage(12, 12), wxT("SyncLockIcon"));

   SET_THEME_FLAGS(  resFlagNone  );
   DEFINE_IMAGE( bmpEditEffects, wxImage(21, 20), wxT("EditEffects"));

#if defined(EXPERIMENTAL_THEMING)
   DEFINE_COLOUR( clrBlank,      wxColour( 64,  64,  64), wxT("Blank"));
   DEFINE_COLOUR( clrUnselected, wxColour( 30,  30,  30), wxT("Unselected"));
   DEFINE_COLOUR( clrSelected,   wxColour( 93,  65,  93), wxT("Selected"));
   DEFINE_COLOUR( clrSample,     wxColour( 63,  77, 155), wxT("Sample"));
   DEFINE_COLOUR( clrSelSample,  wxColour( 50,  50, 200), wxT("SelSample"));
   DEFINE_COLOUR( clrDragSample, wxColour(  0, 100,   0), wxT("DragSample"));

   DEFINE_COLOUR( clrMuteSample, wxColour(136, 136, 144), wxT("MuteSample"));
   DEFINE_COLOUR( clrRms,        wxColour(107, 154, 247), wxT("Rms"));
   DEFINE_COLOUR( clrMuteRms,    wxColour(136, 136, 144), wxT("MuteRms"));
   DEFINE_COLOUR( clrShadow,     wxColour(148, 148, 148), wxT("Shadow"));
#else
   DEFINE_COLOUR( clrBlank,      wxColour(214, 214, 214), wxT("Blank"));
   DEFINE_COLOUR( clrUnselected, wxColour(192, 192, 192), wxT("Unselected"));
   DEFINE_COLOUR( clrSelected,   wxColour(148, 148, 170), wxT("Selected"));
   DEFINE_COLOUR( clrSample,     wxColour( 50,  50, 200), wxT("Sample"));
   DEFINE_COLOUR( clrSelSample,  wxColour( 50,  50, 200), wxT("SelSample"));
   DEFINE_COLOUR( clrDragSample, wxColour(  0,   0,   0), wxT("DragSample"));

   DEFINE_COLOUR( clrMuteSample, wxColour(136, 136, 144), wxT("MuteSample"));
   DEFINE_COLOUR( clrRms,        wxColour(100, 100, 220), wxT("Rms"));
   DEFINE_COLOUR( clrMuteRms,    wxColour(136, 136, 144), wxT("MuteRms"));
   DEFINE_COLOUR( clrShadow,     wxColour(148, 148, 148), wxT("Shadow"));
#endif

   DEFINE_COLOUR( clrAboutBoxBackground,  wxColour(255, 255, 255),  wxT("AboutBackground"));
#if defined(EXPERIMENTAL_THEMING)
   DEFINE_COLOUR( clrTrackPanelText,      wxColour(153, 153, 153),  wxT("TrackPanelText"));
#else
   DEFINE_COLOUR( clrTrackPanelText,      wxColour(  0,   0,   0),  wxT("TrackPanelText"));
#endif
   DEFINE_COLOUR( clrLabelTrackText,      wxColour(  0,   0,   0),  wxT("LabelTrackText"));

   DEFINE_COLOUR( clrMeterPeak,            wxColour(102, 102, 255),  wxT("MeterPeak"));
   DEFINE_COLOUR( clrMeterDisabledPen,     wxColour(192, 192, 192),  wxT("MeterDisabledPen"));
   DEFINE_COLOUR( clrMeterDisabledBrush,   wxColour(160, 160, 160),  wxT("MeterDisabledBrush"));

   DEFINE_COLOUR( clrMeterInputPen,        wxColour(204, 70, 70),     wxT("MeterInputPen") );
   DEFINE_COLOUR( clrMeterInputBrush,      wxColour(204, 70, 70),     wxT("MeterInputBrush") );
   DEFINE_COLOUR( clrMeterInputRMSBrush,   wxColour(255, 102, 102),   wxT("MeterInputRMSBrush") );
   DEFINE_COLOUR( clrMeterInputClipBrush,  wxColour(255, 53, 53),     wxT("MeterInputClipBrush") );
   DEFINE_COLOUR( clrMeterInputLightPen,   wxColour(255, 153, 153),   wxT("MeterInputLightPen") );
   DEFINE_COLOUR( clrMeterInputDarkPen,    wxColour(153, 61, 61),     wxT("MeterInputDarkPen") );

   DEFINE_COLOUR( clrMeterOutputPen,       wxColour(70, 204, 70),     wxT("MeterOutputPen") );
   DEFINE_COLOUR( clrMeterOutputBrush,     wxColour(70, 204, 70),     wxT("MeterOutputBrush") );
   DEFINE_COLOUR( clrMeterOutputRMSBrush,  wxColour(102, 255, 102),   wxT("MeterOutputRMSBrush") );
   DEFINE_COLOUR( clrMeterOutputClipBrush, wxColour(255, 53, 53),     wxT("MeterOutputClipBrush") );
   DEFINE_COLOUR( clrMeterOutputLightPen,  wxColour(153, 255, 153),   wxT("MeterOutputLightPen") );
   DEFINE_COLOUR( clrMeterOutputDarkPen,   wxColour(61, 164, 61),     wxT("MeterOutputDarkPen") );
#if defined(EXPERIMENTAL_THEMING)
   DEFINE_COLOUR( clrRulerBackground,      wxColour( 93,  65,  93),   wxT("RulerBackground") );
#else
   DEFINE_COLOUR( clrRulerBackground,      wxColour(148, 148, 170),   wxT("RulerBackground") );
#endif
   DEFINE_COLOUR( clrAxisLines,            wxColour(0, 0, 255),       wxT("AxisLines") );
   DEFINE_COLOUR( clrGraphLines,           wxColour(110, 110, 220),   wxT("GraphLines") );
   DEFINE_COLOUR( clrResponseLines,        wxColour(0, 255, 0),       wxT("ResponseLines") );
   DEFINE_COLOUR( clrHzPlot,               wxColour(140, 60, 190),    wxT("HzPlot") );
   DEFINE_COLOUR( clrWavelengthPlot,       wxColour(200, 50, 150),    wxT("WavelengthPlot") );

   DEFINE_COLOUR( clrEnvelope,             wxColour( 110, 110, 220),  wxT("Envelope") );

   DEFINE_COLOUR( clrMuteButtonActive,     wxColour( 160, 170, 210),  wxT("MuteButtonActive") );
   DEFINE_COLOUR( clrMuteButtonVetoed,     wxColour( 180, 180, 185),  wxT("MuteButtonVetoed") );

   DEFINE_COLOUR( clrCursorPen,            wxColour(     0,  0,  0),  wxT("CursorPen") );
   DEFINE_COLOUR( clrRecordingPen,         wxColour(   176,  0, 28),  wxT("RecordingPen") );
   DEFINE_COLOUR( clrPlaybackPen,          wxColour(    36, 96, 46),  wxT("PlaybackPen") );
   DEFINE_COLOUR( clrRecordingBrush,       wxColour(   190,129,129),  wxT("RecordingBrush") );
   DEFINE_COLOUR( clrPlaybackBrush,        wxColour(    28,171, 51),  wxT("PlaybackBrush") );

   DEFINE_COLOUR( clrRulerRecordingBrush,  wxColour(   196,196,196),  wxT("RulerRecordingBrush") );
   DEFINE_COLOUR( clrRulerRecordingPen,    wxColour(   128,128,128),  wxT("RulerRecordingPen") );
   DEFINE_COLOUR( clrRulerPlaybackBrush,   wxColour(   190,129,129),  wxT("RulerPlaybackBrush") );
   DEFINE_COLOUR( clrRulerPlaybackPen,     wxColour(   176,  0, 28),  wxT("RulerPlaybackPen") );

   DEFINE_COLOUR( clrTimeFont,             wxColour(     0,  0,180),  wxT("TimeFont") );
   DEFINE_COLOUR( clrTimeBack,             wxColour(   160,160,160),  wxT("TimeBack") );
   DEFINE_COLOUR( clrTimeFontFocus,        wxColour(     0,  0,  0),  wxT("TimeFontFocus") );
   DEFINE_COLOUR( clrTimeBackFocus,        wxColour(   242,242,255),  wxT("TimeBackFocus") );

   DEFINE_COLOUR( clrLabelTextNormalBrush, wxColour(   190,190,240),  wxT("LabelTextNormalBrush") );
   DEFINE_COLOUR( clrLabelTextEditBrush,   wxColour(   255,255,255),  wxT("LabelTextEditBrush") );
   DEFINE_COLOUR( clrLabelUnselectedBrush, wxColour(   192,192,192),  wxT("LabelUnselectedBrush") );
   DEFINE_COLOUR( clrLabelSelectedBrush,   wxColour(   148,148,170),  wxT("LabelSelectedBrush") );
   DEFINE_COLOUR( clrLabelUnselectedPen,   wxColour(   192,192,192),  wxT("LabelUnselectedPen") );
   DEFINE_COLOUR( clrLabelSelectedPen,     wxColour(   148,148,170),  wxT("LabelSelectedPen") );
   DEFINE_COLOUR( clrLabelSurroundPen,     wxColour(     0,  0,  0),  wxT("LabelSurroundPen") );

#ifdef EXPERIMENTAL_THEMING
   DEFINE_COLOUR( clrTrackFocus0,          wxColour( 200, 200, 200),  wxT("TrackFocus0") );
   DEFINE_COLOUR( clrTrackFocus1,          wxColour( 180, 180, 180),  wxT("TrackFocus1") );
   DEFINE_COLOUR( clrTrackFocus2,          wxColour( 160, 160, 160),  wxT("TrackFocus2") );
#else
   DEFINE_COLOUR( clrTrackFocus0,          wxColour( 255, 255, 128),  wxT("TrackFocus0") );
   DEFINE_COLOUR( clrTrackFocus1,          wxColour( 215, 215, 138),  wxT("TrackFocus1") );
   DEFINE_COLOUR( clrTrackFocus2,          wxColour( 185, 185, 142),  wxT("TrackFocus2") );
#endif

   DEFINE_COLOUR( clrSnapGuide,            wxColour( 255, 255,   0),  wxT("SnapGuide") );
   DEFINE_COLOUR( clrTrackInfo,            wxColour(  64,  64,  64),  wxT("TrackInfo") );
   DEFINE_COLOUR( clrTrackInfoSelected,    wxColour(  93,  65,  93),  wxT("TrackInfoSelected") );

   DEFINE_COLOUR( clrLight,                wxColour(  60,  60,  60),  wxT("Light") );
   DEFINE_COLOUR( clrMedium,               wxColour(  43,  43,  43),  wxT("Medium") );
   DEFINE_COLOUR( clrDark,                 wxColour(  20,  20,  20),  wxT("Dark") );

   DEFINE_COLOUR( clrLightSelected,        wxColour(  93,  65,  93),  wxT("LightSelected") );
   DEFINE_COLOUR( clrMediumSelected,       wxColour(  93,  43,  93),  wxT("MediumSelected") );
   DEFINE_COLOUR( clrDarkSelected,         wxColour(  93,  20,  93),  wxT("DarkSelected") );

   DEFINE_COLOUR( clrClipped,    wxColour(255,   0,   0), wxT("Clipped"));
   DEFINE_COLOUR( clrMuteClipped,wxColour(136, 136, 144), wxT("MuteClipped"));

   DEFINE_COLOUR( clrProgressDone,     wxColour(60, 240, 60, 128),   wxT("ProgressDone"));
   DEFINE_COLOUR( clrProgressNotYet,   wxColour(255, 255, 255,220), wxT("ProgressNotYet"));
   DEFINE_COLOUR( clrSyncLockSel,          wxColour(192, 192, 192),      wxT("SyncLockSel"));

   DEFINE_COLOUR( clrSelTranslucent,   wxColour(104, 104, 148, 127), wxT("SelTranslucent"));
   // This is for waveform drawing, selected outside of clips
   DEFINE_COLOUR( clrBlankSelected, wxColour(170, 170, 192), wxT("BlankSelected"));

