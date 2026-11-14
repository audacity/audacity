/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackArtist.cpp

  Dominic Mazzoni


*******************************************************************//*!

\class TrackArtist
\brief   This class handles the actual rendering of WaveTracks (both
  waveforms and spectra), NoteTracks, LabelTracks and TimeTracks.

  It's actually a little harder than it looks, because for
  waveforms at least it needs to cache the samples that are
  currently on-screen.

<b>How Audacity Redisplay Works \n
 Roger Dannenberg</b> \n
Oct 2010 \n

In my opinion, the bitmap should contain only the waveform, note, and
label images along with gray selection highlights. The track info
(sliders, buttons, title, etc.), track selection highlight, cursor, and
indicator should be drawn in the normal way, and clipping regions should
be used to avoid excessive copying of bitmaps (say, when sliders move),
or excessive redrawing of track info widgets (say, when scrolling occurs).
This is a fairly tricky code change since it requires careful specification
of what and where redraw should take place when any state changes. One
surprising finding is that NoteTrack display is slow compared to WaveTrack
display. Each note takes some time to gather attributes and select colors,
and while audio draws two amplitudes per horizontal pixels, large MIDI
scores can have more notes than horizontal pixels. This can make slider
changes very sluggish, but this can also be a problem with many
audio tracks.

*//*******************************************************************/

#include "TrackArtist.h"

#include "AllThemeResources.h"
#include "prefs/GUIPrefs.h"
#include "SyncLock.h"
#include "Theme.h"
#include "TrackPanelDrawingContext.h"

#include "Decibels.h"
#include "prefs/TracksPrefs.h"

namespace {
void ChangeLightness(wxPen& pen, int ialpha)
{
    pen.SetColour(pen.GetColour().ChangeLightness(ialpha));
}

void ChangeLightness(wxBrush& brush, int ialpha)
{
    brush.SetColour(brush.GetColour().ChangeLightness(ialpha));
}
}

TrackArtist::TrackArtist(TrackPanel* parent_)
    : parent(parent_)
{
    mdBrange = DecibelScaleCutoff.GetDefault();
    mSampleDisplay = 1;// Stem plots by default.

    SetColours(0);

    UpdatePrefs();
}

TrackArtist::~TrackArtist()
{
}

TrackArtist* TrackArtist::Get(TrackPanelDrawingContext& context)
{
    return static_cast< TrackArtist* >(context.pUserData);
}

void TrackArtist::SetColours(int iColorIndex)
{
    theTheme.SetBrushColour(blankBrush,      clrBlank);
    theTheme.SetBrushColour(unselectedBrush, clrUnselected);
    theTheme.SetBrushColour(selectedBrush,   clrSelected);
    theTheme.SetBrushColour(sampleBrush,     clrSample);
    theTheme.SetBrushColour(selsampleBrush,  clrSelSample);
    theTheme.SetBrushColour(dragsampleBrush, clrDragSample);
    theTheme.SetBrushColour(blankSelectedBrush, clrBlankSelected);
    theTheme.SetBrushColour(envelopeBackgroundBrush, clrEnvelopeBackground);
    theTheme.SetBrushColour(clipAffordanceBackgroundBrush, clrBlank);
    theTheme.SetBrushColour(clipAffordanceBackgroundSelBrush, clrBlankSelected);

    theTheme.SetPenColour(blankPen,        clrBlank);
    theTheme.SetPenColour(unselectedPen,   clrUnselected);
    theTheme.SetPenColour(selectedPen,     clrSelected);
    theTheme.SetPenColour(muteSamplePen,   clrMuteSample);
    theTheme.SetPenColour(odProgressDonePen, clrProgressDone);
    theTheme.SetPenColour(odProgressNotYetPen, clrProgressNotYet);
    theTheme.SetPenColour(clippedPen,      clrClipped);
    theTheme.SetPenColour(muteClippedPen,  clrMuteClipped);
    theTheme.SetPenColour(blankSelectedPen, clrBlankSelected);

    theTheme.SetPenColour(selsamplePen,    clrSelSample);
    theTheme.SetPenColour(muteRmsPen,      clrMuteRms);

    theTheme.SetPenColour(beatSepearatorPen[0], clrBeatSeparatorPen);
    theTheme.SetPenColour(beatSepearatorPen[1], clrBeatSeparatorPen);
    theTheme.SetPenColour(barSepearatorPen[0], clrBarSeparatorPen);
    theTheme.SetPenColour(barSepearatorPen[1], clrBarSeparatorPen);
    theTheme.SetBrushColour(beatStrongBrush[0], clrBeatFillStrongBrush);
    theTheme.SetBrushColour(beatStrongBrush[1], clrBeatFillStrongBrush);
    theTheme.SetBrushColour(beatWeakBrush[0], clrBeatFillWeakBrush);
    theTheme.SetBrushColour(beatWeakBrush[1], clrBeatFillWeakBrush);
    theTheme.SetBrushColour(beatStrongSelBrush[0], clrBeatFillStrongSelBrush);
    theTheme.SetBrushColour(beatStrongSelBrush[1], clrBeatFillStrongSelBrush);
    theTheme.SetBrushColour(beatWeakSelBrush[0], clrBeatFillWeakSelBrush);
    theTheme.SetBrushColour(beatWeakSelBrush[1], clrBeatFillWeakSelBrush);

    ChangeLightness(beatSepearatorPen[1], 90);
    ChangeLightness(barSepearatorPen[1], 90);
    ChangeLightness(beatStrongBrush[1], 90);
    ChangeLightness(beatWeakBrush[1], 90);
    ChangeLightness(beatStrongSelBrush[1], 90);
    ChangeLightness(beatWeakSelBrush[1], 90);
    ChangeLightness(clipAffordanceBackgroundBrush, 90);
    ChangeLightness(clipAffordanceBackgroundSelBrush, 90);

    switch (iColorIndex % 4) {
    default:
    case 0:
        theTheme.SetPenColour(samplePen,       clrSample);
        theTheme.SetPenColour(rmsPen,          clrRms);
        break;
    case 1:   // RED
        theTheme.SetPenColour(samplePen,       clrSample2);
        theTheme.SetPenColour(rmsPen,          clrRms2);
        break;
    case 2:   // GREEN
        theTheme.SetPenColour(samplePen,       clrSample3);
        theTheme.SetPenColour(rmsPen,          clrRms3);
        break;
    case 3:   //BLACK
        theTheme.SetPenColour(samplePen,       clrSample4);
        theTheme.SetPenColour(rmsPen,          clrRms4);
        break;
    }
}

void TrackArtist::UpdatePrefs()
{
    mdBrange = DecibelScaleCutoff.Read();
    mSampleDisplay = TracksPrefs::SampleViewChoice();

    SetColours(0);
}
