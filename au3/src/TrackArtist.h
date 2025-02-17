/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackArtist.h

  Dominic Mazzoni

  This singleton class handles the actual rendering of WaveTracks
  (both waveforms and spectra), NoteTracks, and LabelTracks.

  It's actually a little harder than it looks, because for
  waveforms at least it needs to cache the samples that are
  currently on-screen.

**********************************************************************/

#ifndef __AUDACITY_TRACKARTIST__
#define __AUDACITY_TRACKARTIST__

#include <wx/brush.h> // member variable
#include <wx/pen.h> // member variables

#include "Prefs.h"

class wxRect;

class PendingTracks;
class TrackList;
class TrackPanel;
class SelectedRegion;
class Track;
class TrackPanel;
struct TrackPanelDrawingContext;
class ZoomInfo;

class AUDACITY_DLL_API TrackArtist final : private PrefsListener
{
public:

    enum : unsigned {
        PassTracks,
        PassMargins,
        PassBorders,
        PassControls,
        PassZooming,
        PassBackground,
        PassFocus,
        PassSnapping,

        NPasses
    };

    TrackArtist(TrackPanel* parent_);
    ~TrackArtist();
    static TrackArtist* Get(TrackPanelDrawingContext&);

    void SetColours(int iColorIndex);

    void UpdatePrefs() override;

    TrackPanel* parent;

    // Preference values
    float mdBrange;           // "/GUI/EnvdBRange"
    int mSampleDisplay;

    wxBrush blankBrush;
    wxBrush unselectedBrush;
    wxBrush selectedBrush;
    wxBrush sampleBrush;
    wxBrush selsampleBrush;
    wxBrush dragsampleBrush;// for samples which are draggable.
    wxBrush muteSampleBrush;
    wxBrush blankSelectedBrush;
    wxBrush envelopeBackgroundBrush;
    wxBrush clipAffordanceBackgroundBrush;
    wxBrush clipAffordanceBackgroundSelBrush;
    wxPen blankPen;
    wxPen unselectedPen;
    wxPen selectedPen;
    wxPen samplePen;
    wxPen rmsPen;
    wxPen muteRmsPen;
    wxPen selsamplePen;
    wxPen muteSamplePen;
    wxPen odProgressNotYetPen;
    wxPen odProgressDonePen;
    wxPen clippedPen;
    wxPen muteClippedPen;
    wxPen blankSelectedPen;

    wxPen beatSepearatorPen[2];
    wxPen barSepearatorPen[2];
    wxBrush beatStrongBrush[2];
    wxBrush beatWeakBrush[2];
    wxBrush beatStrongSelBrush[2];
    wxBrush beatWeakSelBrush[2];

    const SelectedRegion* pSelectedRegion{};
    ZoomInfo* pZoomInfo{};
    const PendingTracks* pPendingTracks{};

    bool drawEnvelope{ false };
    bool bigPoints{ false };
    bool drawSliders{ false };
    bool onBrushTool{ false };
    bool hasSolo{ false };
};

#endif                          // define __AUDACITY_TRACKARTIST__
