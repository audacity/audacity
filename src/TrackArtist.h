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

#include "Audacity.h" // for USE_* macros
#include "Experimental.h"

#include "MemoryX.h"
#include <wx/brush.h>
#include <wx/pen.h>
#include "audacity/Types.h"

class wxRect;
class wxHashTable;

class Track;
class WaveDisplay;
class WaveTrack;
class WaveTrackCache;
class WaveClip;
class NoteTrack;
class LabelTrack;
class TimeTrack;
class TrackList;
class TrackPanel;
class Ruler;
class SelectedRegion;
class ZoomInfo;

struct TrackPanelDrawingContext;

#ifndef uchar
typedef unsigned char uchar;
#endif

namespace TrackArt {
   void DrawTracks(TrackPanelDrawingContext &context,
                   const TrackList *tracks,
                   const wxRegion & reg,
                   const wxRect &clip);

   void DrawTrack(TrackPanelDrawingContext &context,
                  const Track *t,
                  const wxRect & rect);

   void DrawVRuler(TrackPanelDrawingContext &context,
                   const Track *t, const wxRect & rect, bool bSelected );

   // Helper: draws the "sync-locked" watermark tiled to a rectangle
   void DrawSyncLockTiles(
      TrackPanelDrawingContext &context, const wxRect &rect );

   // Helper: draws background with selection rect
   void DrawBackgroundWithSelection(TrackPanelDrawingContext &contex,
         const wxRect &rect, const Track *track,
         const wxBrush &selBrush, const wxBrush &unselBrush,
         bool useSelection = true);

   //
   // Lower-level drawing functions
   //

   void DrawWaveform(TrackPanelDrawingContext &context,
                     const WaveTrack *track,
                     const wxRect & rect,
                     bool muted);

   void DrawSpectrum(TrackPanelDrawingContext &context,
                     const WaveTrack *track,
                     const wxRect & rect);
#ifdef USE_MIDI
   int GetBottom(NoteTrack *t, const wxRect &rect);
   void DrawNoteBackground(TrackPanelDrawingContext &context,
                           const NoteTrack *track,
                           const wxRect &rect, const wxRect &sel,
                           const wxBrush &wb, const wxPen &wp,
                           const wxBrush &bb, const wxPen &bp,
                           const wxPen &mp);
   void DrawNoteTrack(TrackPanelDrawingContext &context,
                      const NoteTrack *track,
                      const wxRect & rect,
                      bool muted);
#endif // USE_MIDI

   void DrawTimeTrack(TrackPanelDrawingContext &context,
                      const TimeTrack *track,
                      const wxRect & rect);

   void DrawTimeSlider(TrackPanelDrawingContext &context,
                       const wxRect & rect,
                       bool rightwards, bool highlight);

   void DrawClipWaveform(TrackPanelDrawingContext &context,
                         const WaveTrack *track, const WaveClip *clip,
                         const wxRect & rect,
                         bool dB, bool muted);

   void DrawClipSpectrum(TrackPanelDrawingContext &context,
                         WaveTrackCache &cache, const WaveClip *clip,
                         const wxRect & rect);

   // Waveform utility functions

   void DrawWaveformBackground(TrackPanelDrawingContext &context,
                               int leftOffset, const wxRect &rect,
                               const double env[],
                               float zoomMin, float zoomMax,
                               int zeroLevelYCoordinate,
                               bool dB, float dBRange,
                               double t0, double t1,
                               bool bIsSyncLockSelected,
                               bool highlightEnvelope);
   void DrawMinMaxRMS(TrackPanelDrawingContext &context,
                      const wxRect & rect, const double env[],
                      float zoomMin, float zoomMax,
                      bool dB, float dBRange,
                      const float *min, const float *max, const float *rms, const int *bl,
                      bool /* showProgress */, bool muted);
   void DrawIndividualSamples(TrackPanelDrawingContext &context,
                              int leftOffset, const wxRect & rect,
                              float zoomMin, float zoomMax,
                              bool dB, float dBRange,
                              const WaveClip *clip,
                              bool showPoints, bool muted,
                              bool highlight);

   void DrawNegativeOffsetTrackArrows( TrackPanelDrawingContext &context,
                                       const wxRect & rect );

   void DrawEnvelope(TrackPanelDrawingContext &context,
                     const wxRect & rect, const double env[],
                     float zoomMin, float zoomMax,
                     bool dB, float dBRange, bool highlight);
   void DrawEnvLine(TrackPanelDrawingContext &context,
                    const wxRect & rect, int x0, int y0, int cy, bool top);
}

class AUDACITY_DLL_API TrackArtist {

public:
   TrackArtist( TrackPanel *parent_ );
   ~TrackArtist();
   static TrackArtist *Get( TrackPanelDrawingContext & );

   void SetBackgroundBrushes(wxBrush unselectedBrushIn, wxBrush selectedBrushIn,
                             wxPen unselectedPenIn, wxPen selectedPenIn) {
     this->unselectedBrush = unselectedBrushIn;
     this->selectedBrush = selectedBrushIn;
     this->unselectedPen = unselectedPenIn;
     this->selectedPen = selectedPenIn;
   }

   void SetColours(int iColorIndex);

   void UpdatePrefs();

   void UpdateVRuler(const Track *t, const wxRect & rect);

   TrackPanel *parent;

   // Preference values
   float mdBrange;            // "/GUI/EnvdBRange"
   long mShowClipping;        // "/GUI/ShowClipping"
   int  mSampleDisplay;
   bool mbShowTrackNameInWaveform;  // "/GUI/ShowTrackNameInWaveform"

   wxBrush blankBrush;
   wxBrush unselectedBrush;
   wxBrush selectedBrush;
   wxBrush sampleBrush;
   wxBrush selsampleBrush;
   wxBrush dragsampleBrush;// for samples which are draggable.
   wxBrush muteSampleBrush;
   wxBrush blankSelectedBrush;
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
   wxPen shadowPen;
   wxPen clippedPen;
   wxPen muteClippedPen;
   wxPen blankSelectedPen;

   std::unique_ptr<Ruler> vruler;

#ifdef EXPERIMENTAL_FFT_Y_GRID
   bool fftYGridOld;
#endif //EXPERIMENTAL_FFT_Y_GRID

#ifdef EXPERIMENTAL_FIND_NOTES
   bool fftFindNotesOld;
   int findNotesMinAOld;
   int findNotesNOld;
   bool findNotesQuantizeOld;
#endif

   SelectedRegion *pSelectedRegion{};
   ZoomInfo *pZoomInfo{};

   int leftOffset{ 0 };
   bool drawEnvelope{ false };
   bool bigPoints{ false };
   bool drawSliders{ false };
   bool hasSolo{ false };
};

extern int GetWaveYPos(float value, float min, float max,
                       int height, bool dB, bool outer, float dBr,
                       bool clip);
extern float FromDB(float value, double dBRange);
extern float ValueOfPixel(int yy, int height, bool offset,
                          bool dB, double dBRange, float zoomMin, float zoomMax);

#endif                          // define __AUDACITY_TRACKARTIST__
