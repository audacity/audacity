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

#include "MemoryX.h"
#include <wx/brush.h>
#include <wx/pen.h>
#include "Experimental.h"
#include "audacity/Types.h"

class wxDC;
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
class Ruler;
class SelectedRegion;
class ZoomInfo;

#ifndef uchar
typedef unsigned char uchar;
#endif

class AUDACITY_DLL_API TrackArtist {

 public:
   TrackArtist();
   ~TrackArtist();

   void SetColours();
   void DrawTracks(TrackList *tracks, Track *start,
                   wxDC & dc, wxRegion & reg,
                   wxRect & rect, wxRect & clip,
                   const SelectedRegion &selectedRegion, const ZoomInfo &zoomInfo,
                   bool drawEnvelope, bool bigPoints, bool drawSliders);

   void DrawTrack(const Track *t,
                  wxDC & dc, const wxRect & rect,
                  const SelectedRegion &selectedRegion, const ZoomInfo &zoomInfo,
                  bool drawEnvelope, bool bigPoints, bool drawSliders,
                  bool hasSolo);

   void DrawVRuler(const Track *t, wxDC *dc, wxRect & rect);

   void UpdateVRuler(const Track *t, wxRect & rect);

   void SetInset(int left, int top, int right, int bottom);

   void UpdatePrefs();

   void SetBackgroundBrushes(wxBrush unselectedBrush, wxBrush selectedBrush,
                             wxPen unselectedPen, wxPen selectedPen) {
     this->unselectedBrush = unselectedBrush;
     this->selectedBrush = selectedBrush;
     this->unselectedPen = unselectedPen;
     this->selectedPen = selectedPen;
   }

   // Helper: draws the "sync-locked" watermark tiled to a rectangle
   static void DrawSyncLockTiles(wxDC *dc, wxRect rect);

   // Helper: draws background with selection rect
   static void DrawBackgroundWithSelection(wxDC *dc, const wxRect &rect,
         const Track *track, wxBrush &selBrush, wxBrush &unselBrush,
         const SelectedRegion &selectedRegion, const ZoomInfo &zoomInfo);

 private:

   //
   // Lower-level drawing functions
   //

   void DrawWaveform(const WaveTrack *track,
                     wxDC & dc, const wxRect & rect,
                     const SelectedRegion &selectedRegion, const ZoomInfo &zoomInfo,
                     bool drawEnvelope, bool bigPoints, bool drawSliders,
                     bool muted);

   void DrawSpectrum(const WaveTrack *track,
                     wxDC & dc, const wxRect & rect,
                     const SelectedRegion &selectedRegion, const ZoomInfo &zoomInfo);
#ifdef USE_MIDI
   int GetBottom(NoteTrack *t, const wxRect &rect);
   void DrawNoteBackground(const NoteTrack *track, wxDC &dc,
                           const wxRect &rect, const wxRect &sel,
                           const ZoomInfo &zoomInfo,
                           const wxBrush &wb, const wxPen &wp,
                           const wxBrush &bb, const wxPen &bp,
                           const wxPen &mp);
   void DrawNoteTrack(const NoteTrack *track,
                      wxDC & dc, const wxRect & rect,
                      const SelectedRegion &selectedRegion, const ZoomInfo &zoomInfo,
                      bool muted);
#endif // USE_MIDI

   void DrawLabelTrack(const LabelTrack *track,
                       wxDC & dc, const wxRect & rect,
                       const SelectedRegion &selectedRegion, const ZoomInfo &zoomInfo);

   void DrawTimeTrack(const TimeTrack *track,
                      wxDC & dc, const wxRect & rect, const ZoomInfo &zoomInfo);

   void DrawTimeSlider(wxDC & dc, const wxRect & rect,
                       bool rightwards);

   void DrawClipWaveform(const WaveTrack *track, const WaveClip *clip,
                         wxDC & dc, const wxRect & rect,
                         const SelectedRegion &selectedRegion, const ZoomInfo &zoomInfo,
                         bool drawEnvelope, bool bigPoints,
                         bool dB, bool muted);

   void DrawClipSpectrum(WaveTrackCache &cache, const WaveClip *clip,
                         wxDC & dc, const wxRect & rect,
                         const SelectedRegion &selectedRegion, const ZoomInfo &zoomInfo);

   // Waveform utility functions

   void DrawWaveformBackground(wxDC & dc, int leftOffset, const wxRect &rect,
                               const double env[],
                               float zoomMin, float zoomMax,
                               bool dB, float dBRange,
                               double t0, double t1, const ZoomInfo &zoomInfo,
                               bool drawEnvelope, bool bIsSyncLockSelected);
   void DrawMinMaxRMS(wxDC &dc, const wxRect & rect, const double env[],
                      float zoomMin, float zoomMax,
                      bool dB, float dBRange,
                      const float *min, const float *max, const float *rms, const int *bl,
                      bool /* showProgress */, bool muted
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
                      , const float gain
#endif
   );
   void DrawIndividualSamples(wxDC & dc, int leftOffset, const wxRect & rect,
                              float zoomMin, float zoomMax,
                              bool dB, float dBRange,
                              const WaveClip *clip,
                              const ZoomInfo &zoomInfo,
                              bool bigPoints, bool showPoints, bool muted);

   void DrawNegativeOffsetTrackArrows(wxDC & dc, const wxRect & rect);

   void DrawEnvelope(wxDC & dc, const wxRect & rect, const double env[],
                     float zoomMin, float zoomMax,
                     bool dB, float dBRange);
   void DrawEnvLine(wxDC & dc, const wxRect & rect, int x0, int y0, int cy, bool top);

   // Preference values
   float mdBrange;            // "/GUI/EnvdBRange"
   long mShowClipping;        // "/GUI/ShowClipping"
   bool mbShowTrackNameInWaveform;  // "/GUI/ShowTrackNameInWaveform"

   int mInsetLeft;
   int mInsetTop;
   int mInsetRight;
   int mInsetBottom;

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
};

extern int GetWaveYPos(float value, float min, float max,
                       int height, bool dB, bool outer, float dBr,
                       bool clip);
extern float FromDB(float value, double dBRange);
extern float ValueOfPixel(int yy, int height, bool offset,
                          bool dB, double dBRange, float zoomMin, float zoomMax);

#endif                          // define __AUDACITY_TRACKARTIST__
