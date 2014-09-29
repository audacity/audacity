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

#include <wx/brush.h>
#include <wx/pen.h>
#include "Experimental.h"
#include "Sequence.h"

class wxDC;
class wxRect;
class wxHashTable;

class Track;
class WaveTrack;
class WaveClip;
class NoteTrack;
class LabelTrack;
class TimeTrack;
class TrackList;
class Ruler;
struct ViewInfo;

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
                   wxRect & r, wxRect & clip, ViewInfo *viewInfo,
                   bool drawEnvelope, bool drawSamples, bool drawSliders);

   void DrawTrack(const Track *t,
                  wxDC & dc, const wxRect & r, const ViewInfo *viewInfo,
                  bool drawEnvelope, bool drawSamples, bool drawSliders,
                  bool hasSolo);

   void DrawVRuler(Track *t, wxDC *dc, wxRect & r);

   void UpdateVRuler(Track *t, wxRect & r);

   void SetInset(int left, int top, int right, int bottom);

   void UpdatePrefs();

   void InvalidateSpectrumCache(TrackList *tracks);
   void InvalidateSpectrumCache(WaveTrack *track);
   int GetSpectrumMinFreq(int deffreq);
   int GetSpectrumMaxFreq(int deffreq);
   int GetSpectrumLogMinFreq(int deffreq);
   int GetSpectrumLogMaxFreq(int deffreq);
   int GetSpectrumWindowSize();

#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
   int GetSpectrumFftSkipPoints();
#endif

   void SetSpectrumMinFreq(int freq);
   void SetSpectrumMaxFreq(int freq);
   void SetSpectrumLogMinFreq(int freq);
   void SetSpectrumLogMaxFreq(int freq);

   void SetBackgroundBrushes(wxBrush unselectedBrush, wxBrush selectedBrush,
                             wxPen unselectedPen, wxPen selectedPen) {
     this->unselectedBrush = unselectedBrush;
     this->selectedBrush = selectedBrush;
     this->unselectedPen = unselectedPen;
     this->selectedPen = selectedPen;
   }

   // Helper: draws the "sync-locked" watermark tiled to a rectangle
   static void DrawSyncLockTiles(wxDC *dc, wxRect r);

   // Helper: draws background with selection rect
   static void DrawBackgroundWithSelection(wxDC *dc, const wxRect &r,
         Track *track, wxBrush &selBrush, wxBrush &unselBrush,
         double sel0, double sel1, double h, double pps);

 private:

   //
   // Lower-level drawing functions
   //

   void DrawWaveform(WaveTrack *track,
                     wxDC & dc, const wxRect & r, const ViewInfo *viewInfo,
                     bool drawEnvelope, bool drawSamples, bool drawSliders,
                     bool dB, bool muted);

   void DrawSpectrum(WaveTrack *track,
                     wxDC & dc, const wxRect & r, const ViewInfo *viewInfo,
                     bool autocorrelation, bool logF);
#ifdef USE_MIDI
   int GetBottom(NoteTrack *t, const wxRect &r);
   void DrawNoteBackground(NoteTrack *track, wxDC &dc,
                           const wxRect &r, const wxRect &sel,
                           const ViewInfo *viewInfo,
                           const wxBrush &wb, const wxPen &wp,
                           const wxBrush &bb, const wxPen &bp,
                           const wxPen &mp);
   void DrawNoteTrack(NoteTrack *track,
                      wxDC & dc, const wxRect & r, const ViewInfo *viewInfo,
                      bool muted);
#endif // USE_MIDI

   void DrawLabelTrack(LabelTrack *track,
                       wxDC & dc, const wxRect & r, const ViewInfo *viewInfo);

   void DrawTimeTrack(TimeTrack *track,
                      wxDC & dc, const wxRect & r, const ViewInfo *viewInfo);

   void DrawTimeSlider(WaveTrack *track,
                       wxDC & dc, const wxRect & r, const ViewInfo *viewInfo,
                       bool rightwards);

   void DrawClipWaveform(WaveTrack *track, WaveClip *clip,
                         wxDC & dc, const wxRect & r, const ViewInfo *viewInfo,
                         bool drawEnvelope, bool drawSamples, bool drawSliders,
                         bool dB, bool muted);

   void DrawClipSpectrum(WaveTrack *track, WaveClip *clip,
                         wxDC & dc, const wxRect & r, const ViewInfo *viewInfo,
                         bool autocorrelation, bool logF);

   // Waveform utility functions

   void DrawWaveformBackground(wxDC & dc, const wxRect &r, const double env[],
                               float zoomMin, float zoomMax, bool dB,
                               const sampleCount where[],
                               sampleCount ssel0, sampleCount ssel1,
                               bool drawEnvelope, bool bIsSyncLockSelected);
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   void DrawMinMaxRMS(wxDC & dc, const wxRect & r, const double env[],
                      float zoomMin, float zoomMax, bool dB,
                      const float min[], const float max[], const float rms[],
                      const int bl[], bool showProgress, bool muted, const float gain);
#else
   void DrawMinMaxRMS(wxDC & dc, const wxRect & r, const double env[],
                      float zoomMin, float zoomMax, bool dB,
                      const float min[], const float max[], const float rms[],
                      const int bl[], bool showProgress, bool muted);
#endif
   void DrawIndividualSamples(wxDC & dc, const wxRect & r,
                              float zoomMin, float zoomMax, bool dB,
                              WaveClip *clip,
                              double t0, double pps, double h,
                              bool drawSamples, bool showPoints, bool muted);

   void DrawNegativeOffsetTrackArrows(wxDC & dc, const wxRect & r);

   void DrawEnvelope(wxDC & dc, const wxRect & r, const double env[],
                     float zoomMin, float zoomMax, bool dB);
   void DrawEnvLine(wxDC & dc, const wxRect & r, int x, int y, int cy, bool top);

   // Preference values
   float mdBrange;            // "/GUI/EnvdBRange"
   long mShowClipping;        // "/GUI/ShowClipping"
   int mLogMaxFreq;           // "/SpectrumLog/MaxFreq"
   int mLogMinFreq;           // "/SpectrumLog/MinFreq"
   int mMaxFreq;              // "/Spectrum/MaxFreq"
   int mMinFreq;              // "/Spectrum/MinFreq"
   int mWindowSize;           // "/Spectrum/FFTSize"
   bool mIsGrayscale;         // "/Spectrum/Grayscale"
   bool mbShowTrackNameInWaveform;  // "/GUI/ShowTrackNameInWaveform"

#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
   int mFftSkipPoints;        // "/Spectrum/FFTSkipPoints"
#endif //EXPERIMENTAL_FFT_SKIP_POINTS

#ifdef EXPERIMENTAL_FFT_Y_GRID
   bool mFftYGrid;            // "/Spectrum/FFTYGrid"
#endif //EXPERIMENTAL_FFT_Y_GRID

#ifdef EXPERIMENTAL_FIND_NOTES
   bool mFftFindNotes;        // "/Spectrum/FFTFindNotes"
   float mFindNotesMinA;      // "/Spectrum/FindNotesMinA"
   int mNumberOfMaxima;       // "/Spectrum/FindNotesN"
   bool mFindNotesQuantize;   // "/Spectrum/FindNotesQuantize")
#endif //EXPERIMENTAL_FIND_NOTES

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

   Ruler *vruler;

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

#endif                          // define __AUDACITY_TRACKARTIST__
