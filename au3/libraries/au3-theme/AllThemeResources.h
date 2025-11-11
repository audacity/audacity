/**********************************************************************

  Audacity: A Digital Audio Editor

  AllThemeResources.h

  James Crook

  Audacity is free software.
  License: GPL v2 or later - see LICENSE.txt

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

#include "MacroMagic.h"

SET_THEME_FLAGS(resFlagNewLine);

DEFINE_COLOUR(clrBlank,      QColor(64,  64,  64), wxT("Blank"));
DEFINE_COLOUR(clrUnselected, QColor(30,  30,  30), wxT("Unselected"));
DEFINE_COLOUR(clrSelected,   QColor(93,  65,  93), wxT("Selected"));
DEFINE_COLOUR(clrSample,     QColor(63,  77, 155), wxT("Sample"));
DEFINE_COLOUR(clrSample2,    QColor(160,  10,  10), wxT("Sample2"));
DEFINE_COLOUR(clrSample3,    QColor(35, 110,  35), wxT("Sample3"));
DEFINE_COLOUR(clrSample4,    QColor(0,   0,   0), wxT("Sample4"));
DEFINE_COLOUR(clrSelSample,  QColor(50,  50, 200), wxT("SelSample"));
DEFINE_COLOUR(clrDragSample, QColor(0, 100,   0), wxT("DragSample"));

DEFINE_COLOUR(clrMuteSample, QColor(136, 136, 144), wxT("MuteSample"));
DEFINE_COLOUR(clrRms,        QColor(107, 154, 247), wxT("Rms"));
DEFINE_COLOUR(clrRms2,       QColor(230,  80,  80), wxT("Rms2"));
DEFINE_COLOUR(clrRms3,       QColor(75, 200,  75), wxT("Rms3"));
DEFINE_COLOUR(clrRms4,       QColor(100, 100, 100), wxT("Rms4"));
DEFINE_COLOUR(clrMuteRms,    QColor(136, 136, 144), wxT("MuteRms"));

DEFINE_COLOUR(clrAboutBoxBackground,  QColor(255, 255, 255),  wxT("AboutBackground"));
DEFINE_COLOUR(clrTrackPanelText,      QColor(200, 200, 200),  wxT("TrackPanelText"));
DEFINE_COLOUR(clrLabelTrackText,      QColor(0,   0,   0),  wxT("LabelTrackText"));

DEFINE_COLOUR(clrMeterPeak,            QColor(102, 102, 255),  wxT("MeterPeak"));

DEFINE_COLOUR(clrMeterInputPen,        QColor(204, 70, 70),     wxT("MeterInputPen"));
DEFINE_COLOUR(clrMeterInputBrush,      QColor(204, 70, 70),     wxT("MeterInputBrush"));
DEFINE_COLOUR(clrMeterInputRMSBrush,   QColor(255, 102, 102),   wxT("MeterInputRMSBrush"));
DEFINE_COLOUR(clrMeterInputClipBrush,  QColor(255, 53, 53),     wxT("MeterInputClipBrush"));
DEFINE_COLOUR(clrMeterInputLightPen,   QColor(255, 153, 153),   wxT("MeterInputLightPen"));
DEFINE_COLOUR(clrMeterInputDarkPen,    QColor(153, 61, 61),     wxT("MeterInputDarkPen"));
DEFINE_COLOUR(clrMeterBackground,      QColor(153, 61, 61),     wxT("MeterBackground"));

DEFINE_COLOUR(clrMeterOutputPen,       QColor(70, 204, 70),     wxT("MeterOutputPen"));
DEFINE_COLOUR(clrMeterOutputBrush,     QColor(70, 204, 70),     wxT("MeterOutputBrush"));
DEFINE_COLOUR(clrMeterOutputRMSBrush,  QColor(102, 255, 102),   wxT("MeterOutputRMSBrush"));
DEFINE_COLOUR(clrMeterOutputClipBrush, QColor(255, 53, 53),     wxT("MeterOutputClipBrush"));
DEFINE_COLOUR(clrMeterOutputLightPen,  QColor(153, 255, 153),   wxT("MeterOutputLightPen"));
DEFINE_COLOUR(clrMeterOutputDarkPen,   QColor(61, 164, 61),     wxT("MeterOutputDarkPen"));
DEFINE_COLOUR(clrAxisLines,            QColor(0, 0, 255),       wxT("AxisLines"));
DEFINE_COLOUR(clrGraphLines,           QColor(110, 110, 220),   wxT("GraphLines"));
DEFINE_COLOUR(clrResponseLines,        QColor(24, 169, 153),    wxT("ResponseLines"));
DEFINE_COLOUR(clrHzPlot,               QColor(140, 60, 190),    wxT("HzPlot"));
DEFINE_COLOUR(clrWavelengthPlot,       QColor(200, 50, 150),    wxT("WavelengthPlot"));

DEFINE_COLOUR(clrEnvelope,             QColor(110, 110, 220),  wxT("EnvelopeColour"));
DEFINE_COLOUR(clrEnvelopeBackground,   QColor(110, 110, 220),  wxT("EnvelopeBackground"));

DEFINE_COLOUR(clrMuteButtonActive,     QColor(160, 170, 210),  wxT("MuteButtonActive"));
DEFINE_COLOUR(clrMuteButtonVetoed,     QColor(180, 180, 185),  wxT("MuteButtonVetoed"));

DEFINE_COLOUR(clrCursorPen,            QColor(0,  0,  0),  wxT("CursorPen"));
DEFINE_COLOUR(clrRecordingPen,         QColor(176,  0, 28),  wxT("RecordingPen"));
DEFINE_COLOUR(clrPlaybackPen,          QColor(36, 96, 46),  wxT("PlaybackPen"));
DEFINE_COLOUR(clrRecordingBrush,       QColor(190, 129, 129),  wxT("RecordingBrush"));
DEFINE_COLOUR(clrPlaybackBrush,        QColor(28, 171, 51),  wxT("PlaybackBrush"));

DEFINE_COLOUR(clrRulerRecordingBrush,  QColor(196, 196, 196),  wxT("RulerRecordingBrush"));

DEFINE_COLOUR(clrTimeFont,             QColor(0,  0, 180),  wxT("TimeFont"));
DEFINE_COLOUR(clrTimeBack,             QColor(160, 160, 160),  wxT("TimeBack"));
DEFINE_COLOUR(clrTimeFontFocus,        QColor(0,  0,  0),  wxT("TimeFontFocus"));
DEFINE_COLOUR(clrTimeBackFocus,        QColor(242, 242, 255),  wxT("TimeBackFocus"));

DEFINE_COLOUR(clrLabelTextNormalBrush, QColor(190, 190, 240),  wxT("LabelTextNormalBrush"));
DEFINE_COLOUR(clrLabelTextEditBrush,   QColor(255, 255, 255),  wxT("LabelTextEditBrush"));
DEFINE_COLOUR(clrLabelUnselectedBrush, QColor(192, 192, 192),  wxT("LabelUnselectedBrush"));
DEFINE_COLOUR(clrLabelSelectedBrush,   QColor(148, 148, 170),  wxT("LabelSelectedBrush"));
DEFINE_COLOUR(clrLabelSurroundPen,     QColor(0,  0,  0),  wxT("LabelSurroundPen"));

DEFINE_COLOUR(clrTrackFocus0,          QColor(200, 200, 200),  wxT("TrackFocus0"));
DEFINE_COLOUR(clrTrackFocus1,          QColor(180, 180, 180),  wxT("TrackFocus1"));
DEFINE_COLOUR(clrTrackFocus2,          QColor(160, 160, 160),  wxT("TrackFocus2"));

DEFINE_COLOUR(clrSnapGuide,            QColor(255, 255,   0),  wxT("SnapGuide"));
DEFINE_COLOUR(clrTrackInfo,            QColor(64,  64,  64),  wxT("TrackInfo"));
DEFINE_COLOUR(clrTrackInfoSelected,    QColor(93,  65,  93),  wxT("TrackInfoSelected"));

DEFINE_COLOUR(clrLight,                QColor(60,  60,  60),  wxT("Light"));
DEFINE_COLOUR(clrMedium,               QColor(43,  43,  43),  wxT("Medium"));
DEFINE_COLOUR(clrDark,                 QColor(20,  20,  20),  wxT("Dark"));

DEFINE_COLOUR(clrLightSelected,        QColor(93,  65,  93),  wxT("LightSelected"));
DEFINE_COLOUR(clrMediumSelected,       QColor(93,  43,  93),  wxT("MediumSelected"));
DEFINE_COLOUR(clrDarkSelected,         QColor(93,  20,  93),  wxT("DarkSelected"));

DEFINE_COLOUR(clrClipped,    QColor(255,   0,   0), wxT("Clipped"));
DEFINE_COLOUR(clrMuteClipped, QColor(136, 136, 144), wxT("MuteClipped"));

DEFINE_COLOUR(clrProgressDone,     QColor(60, 240, 60, 128),   wxT("ProgressDone"));
DEFINE_COLOUR(clrProgressNotYet,   QColor(255, 255, 255, 220), wxT("ProgressNotYet"));
DEFINE_COLOUR(clrSyncLockSel,          QColor(192, 192, 192),      wxT("SyncLockSel"));

// This is for waveform drawing, selected outside of clips
DEFINE_COLOUR(clrBlankSelected, QColor(170, 170, 192), wxT("BlankSelected"));

DEFINE_COLOUR(clrSliderLight,         QColor(1,   1,   1),  wxT("SliderLight"));
DEFINE_COLOUR(clrSliderMain,          QColor(43,  43,  43),  wxT("SliderMain"));
DEFINE_COLOUR(clrSliderDark,          QColor(1,   1,   1),  wxT("SliderDark"));
DEFINE_COLOUR(clrTrackBackground,     QColor(20,  20,  20),  wxT("TrackBackground"));

DEFINE_COLOUR(clrGraphLabels,         QColor(0,    0,   0),  wxT("GraphLabels"));
DEFINE_COLOUR(clrSpectroBackground,   QColor(255,  255,  20),  wxT("SpectroBackground"));
DEFINE_COLOUR(clrScrubRuler,          QColor(255,  255,  20),  wxT("ScrubRuler"));
DEFINE_COLOUR(clrRulerSelected,       QColor(255,  255,  20),  wxT("RulerSelected"));
DEFINE_COLOUR(clrTimeHours,           QColor(255,  255,  20),  wxT("TimeHours"));
DEFINE_COLOUR(clrMidiZebra,           QColor(255,  255,  20),  wxT("MidiZebra"));
DEFINE_COLOUR(clrMidiLines,           QColor(255,  255,  20),  wxT("MidiLines"));
DEFINE_COLOUR(clrTextNegativeNumbers, QColor(0,    0,  255), wxT("TextNegativeNumbers"));

DEFINE_COLOUR(clrSpectro1,            QColor(191,  191,  191),  wxT("Spectro1"));
DEFINE_COLOUR(clrSpectro2,            QColor(76,  153,  255),  wxT("Spectro2"));
DEFINE_COLOUR(clrSpectro3,            QColor(229,   25,  229),  wxT("Spectro3"));
DEFINE_COLOUR(clrSpectro4,            QColor(255,    0,    0),  wxT("Spectro4"));
DEFINE_COLOUR(clrSpectro5,            QColor(255,  255,  255),  wxT("Spectro5"));

DEFINE_COLOUR(clrSpectro1Sel,         QColor(143,  143,  143),  wxT("Spectro1Sel"));
DEFINE_COLOUR(clrSpectro2Sel,         QColor(57,  116,  191),  wxT("Spectro2Sel"));
DEFINE_COLOUR(clrSpectro3Sel,         QColor(172,   19,  172),  wxT("Spectro3Sel"));
DEFINE_COLOUR(clrSpectro4Sel,         QColor(191,    0,    0),  wxT("Spectro4Sel"));
DEFINE_COLOUR(clrSpectro5Sel,         QColor(191,  191,  191),  wxT("Spectro5Sel"));

DEFINE_COLOUR(clrClipAffordanceOutlinePen,    QColor(0,    0,    0),  wxT("ClipAffordanceOutlinePen"));
DEFINE_COLOUR(clrClipAffordanceInactiveBrush, QColor(219,  219,  219),  wxT("ClipAffordanceUnselectedBrush"));
DEFINE_COLOUR(clrClipAffordanceActiveBrush,   QColor(237,  237,  237),  wxT("ClipAffordanceSelectedBrush"));
DEFINE_COLOUR(clrClipAffordanceStroke,        QColor(255,  255,  255),  wxT("ClipAffordanceStroke"));
DEFINE_COLOUR(clrClipAffordanceButton,        QColor(255, 255, 255), wxT("ClipAffordanceButton"));

DEFINE_COLOUR(clrLabelTrackTextSelection, QColor(183, 202, 226), wxT("LabelTrackTextSelection"));

DEFINE_COLOUR(clrClipNameText,          QColor(0, 0, 0),       wxT("ClipNameText"));
DEFINE_COLOUR(clrClipNameTextSelection, QColor(183, 202, 226), wxT("ClipNameTextSelection"));

DEFINE_COLOUR(clrDropHintHighlight, QColor(71, 140, 247), wxT("DropHintHighlight"));
DEFINE_COLOUR(clrEffectListItemBackground, QColor(60, 60, 60), wxT("EffectListItemBackground"));
DEFINE_COLOUR(clrEffectListItemBorder, QColor(204, 204, 204), wxT("EffectListItemBorder"));

DEFINE_COLOUR(clrBeatSeparatorPen, QColor(0xBE, 0xBE, 0xBE), wxT("BeatSeparatorPen"));
DEFINE_COLOUR(clrBarSeparatorPen, QColor(0xBE, 0xBE, 0xBE), wxT("BarSeparatorPen"));
DEFINE_COLOUR(clrBeatFillStrongBrush, QColor(0xD6, 0xD6, 0xD6), wxT("BeatFillStrongBrush"));
DEFINE_COLOUR(clrBeatFillWeakBrush, QColor(0xDC, 0xDC, 0xDC), wxT("BeatFillWeakBrush"));
DEFINE_COLOUR(clrBeatFillStrongSelBrush, QColor(0xD6, 0xD6, 0xD6), wxT("BeatFillStrongSelBrush"));
DEFINE_COLOUR(clrBeatFillWeakSelBrush, QColor(0xDC, 0xDC, 0xDC), wxT("BeatFillWeakSelBrush"));

DEFINE_COLOUR(clrLoopEnabled,          QColor(93,  65,  93),   wxT("LoopEnabled"));
DEFINE_COLOUR(clrLoopDisabled,         QColor(93,  65,  93),   wxT("LoopNotEnabled"));
DEFINE_COLOUR(clrGrabber,              QColor(93,  65,  93),   wxT("Grabber"));

DEFINE_COLOUR(clrTimelineRulerBackground, QColor(0xE9, 0xE9, 0xEB), wxT("TimelineRulerBackground"));
