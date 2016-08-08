/**********************************************************************

  Audacity: A Digital Audio Editor

  MixerBoard.h

  Vaughan Johnson, January 2007

**********************************************************************/

#include "Experimental.h"

#ifndef __AUDACITY_MIXER_BOARD__
#define __AUDACITY_MIXER_BOARD__

#include <wx/frame.h>
#include <wx/bmpbuttn.h>
#include <wx/hashmap.h>
#include <wx/image.h>
#include <wx/scrolwin.h>
#include <wx/statbmp.h>
#include <wx/stattext.h>

#include "widgets/AButton.h"
#include "widgets/ASlider.h"
#include "widgets/wxPanelWrapper.h"

// containment hierarchy:
//    MixerBoardFrame -> MixerBoard -> MixerBoardScrolledWindow -> MixerTrackCluster(s)


// MixerTrackSlider is a subclass just to override OnMouseEvent,
// so we can know when adjustment ends, so we can PushState only then.
class MixerTrackSlider final : public ASlider
{
public:
   MixerTrackSlider(wxWindow * parent,
                     wxWindowID id,
                     const wxString &name,
                     const wxPoint & pos,
                     const wxSize & size,
                     int style = FRAC_SLIDER,
                     bool popup = true,
                     bool canUseShift = true,
                     float stepValue = STEP_CONTINUOUS,
                     int orientation = wxHORIZONTAL);
   virtual ~MixerTrackSlider() {}

   void OnMouseEvent(wxMouseEvent & event);

   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent& event);

protected:
   bool mIsPan;

public:
    DECLARE_EVENT_TABLE()
};


class AudacityProject;
class Meter;
class MixerBoard;
#ifdef EXPERIMENTAL_MIDI_OUT
class Track;
class NoteTrack;
#endif
class WaveTrack;

class MixerTrackCluster final : public wxPanelWrapper
{
public:
   MixerTrackCluster(wxWindow* parent,
                     MixerBoard* grandParent, AudacityProject* project,
                     WaveTrack* pLeftTrack, WaveTrack* pRightTrack = NULL,
                     const wxPoint& pos = wxDefaultPosition,
                     const wxSize& size = wxDefaultSize);
   virtual ~MixerTrackCluster() {}

   void UpdatePrefs();

   void HandleResize(); // For wxSizeEvents, update gain slider and meter.

   void HandleSliderGain(const bool bWantPushState = false);
   void HandleSliderPan(const bool bWantPushState = false);

   void ResetMeter(const bool bResetClipping);

   // These are used by TrackPanel for synchronizing control states.
   void UpdateForStateChange(); // Update the controls that can be affected by state change.
   void UpdateName();
   void UpdateMute();
   void UpdateSolo();
   void UpdatePan();
   void UpdateGain();
   void UpdateMeter(const double t0, const double t1);

private:
   wxColour GetTrackColor();

   // event handlers
   void HandleSelect(bool bShiftDown, bool bControlDown);

   void OnKeyEvent(wxKeyEvent& event);
   void OnMouseEvent(wxMouseEvent& event);
   void OnPaint(wxPaintEvent& evt);

   void OnButton_MusicalInstrument(wxCommandEvent& event);
   void OnSlider_Gain(wxCommandEvent& event);
   void OnSlider_Pan(wxCommandEvent& event);
   void OnButton_Mute(wxCommandEvent& event);
   void OnButton_Solo(wxCommandEvent& event);
   //v void OnSliderScroll_Gain(wxScrollEvent& event);


public:
#ifdef EXPERIMENTAL_MIDI_OUT
   // mTrack is redundant, but simplifies code that operates on either
   // mLeftTrack or mNoteTrack.
   Track* mTrack; // either mLeftTrack or mNoteTrack, whichever is not NULL
#endif
   WaveTrack* mLeftTrack; // NULL if Note Track
   WaveTrack* mRightTrack; // NULL if mono

   //vvv Vaughan, 2010-11-05:
   //    I suggest that when this is no longer experimental, rather than all these #ifdef's,
   //    this be done by factoring, i.e., add two subclasses to MixerTrackCluster,
   //    MixerNoteTrackCluster and MixerWaveTrackCluster, such that all the common
   //    code is in the parent, and these #ifdef's are only around
   //    MixerNoteTrackCluster rather than sprinkled throughout MixerTrackCluster.
#ifdef EXPERIMENTAL_MIDI_OUT
   NoteTrack* mNoteTrack; // NULL if Wave Track
#endif

private:
   MixerBoard* mMixerBoard;
   AudacityProject* mProject;

   // controls
   wxStaticText* mStaticText_TrackName;
   wxBitmapButton* mBitmapButton_MusicalInstrument;
   AButton* mToggleButton_Mute;
   AButton* mToggleButton_Solo;
   MixerTrackSlider* mSlider_Pan;
   MixerTrackSlider* mSlider_Gain;
   Meter* mMeter;

public:
   DECLARE_EVENT_TABLE()
};

WX_DEFINE_ARRAY(MixerTrackCluster*, MixerTrackClusterArray);


class MusicalInstrument
{
public:
   MusicalInstrument(std::unique_ptr<wxBitmap> &&pBitmap, const wxString & strXPMfilename);
   virtual ~MusicalInstrument();

   std::unique_ptr<wxBitmap> mBitmap;
   wxArrayString  mKeywords;
};

using MusicalInstrumentArray = std::vector<movable_ptr<MusicalInstrument>>;



// wxScrolledWindow ignores mouse clicks in client area,
// but they don't get passed to Mixerboard.
// We need to catch them to deselect all track clusters.
class MixerBoardScrolledWindow final : public wxScrolledWindow
{
public:
   MixerBoardScrolledWindow(AudacityProject* project,
                              MixerBoard* parent, wxWindowID id = -1,
                              const wxPoint& pos = wxDefaultPosition,
                              const wxSize& size = wxDefaultSize,
                              long style = wxHSCROLL | wxVSCROLL);
   virtual ~MixerBoardScrolledWindow();

private:
   void OnMouseEvent(wxMouseEvent& event);

private:
   MixerBoard* mMixerBoard;
   AudacityProject* mProject;

public:
   DECLARE_EVENT_TABLE()
};


class MixerBoardFrame;
class TrackList;

class MixerBoard final : public wxWindow
{
   friend class MixerBoardFrame;

public:
   MixerBoard(AudacityProject* pProject,
               wxFrame* parent,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& size = wxDefaultSize);
   virtual ~MixerBoard();

   void UpdatePrefs();

   // Add clusters for any tracks we're not yet showing.
   // Update pointers for tracks we're aleady showing.
   void UpdateTrackClusters();

   int GetTrackClustersWidth();
#ifdef EXPERIMENTAL_MIDI_OUT
   void MoveTrackCluster(const Track* pTrack, bool bUp); // Up in TrackPanel is left in MixerBoard.
   void RemoveTrackCluster(const Track* pTrack);


   wxBitmap* GetMusicalInstrumentBitmap(const wxString & name);
#else
   void MoveTrackCluster(const WaveTrack* pTrack, bool bUp); // Up in TrackPanel is left in MixerBoard.
   void RemoveTrackCluster(const WaveTrack* pTrack);


   wxBitmap* GetMusicalInstrumentBitmap(const WaveTrack* pLeftTrack);
#endif

   bool HasSolo();

#ifdef EXPERIMENTAL_MIDI_OUT
   void RefreshTrackCluster(const Track* pTrack, bool bEraseBackground = true);
#else
   void RefreshTrackCluster(const WaveTrack* pTrack, bool bEraseBackground = true);
#endif
   void RefreshTrackClusters(bool bEraseBackground = true);
   void ResizeTrackClusters();

   void ResetMeters(const bool bResetClipping);

#ifdef EXPERIMENTAL_MIDI_OUT
   void UpdateName(const Track* pTrack);
   void UpdateMute(const Track* pTrack = NULL); // NULL means update for all tracks.
   void UpdateSolo(const Track* pTrack = NULL); // NULL means update for all tracks.
   void UpdatePan(const Track* pTrack);
   void UpdateGain(const Track* pTrack);
#else
   void UpdateName(const WaveTrack* pTrack);
   void UpdateMute(const WaveTrack* pTrack = NULL); // NULL means update for all tracks.
   void UpdateSolo(const WaveTrack* pTrack = NULL); // NULL means update for all tracks.
   void UpdatePan(const WaveTrack* pTrack);
   void UpdateGain(const WaveTrack* pTrack);
#endif

   void UpdateMeters(const double t1, const bool bLoopedPlay);

   void UpdateWidth();

private:
   void CreateMuteSoloImages();
#ifdef EXPERIMENTAL_MIDI_OUT
   int FindMixerTrackCluster(const Track* pTrack,
                              MixerTrackCluster** hMixerTrackCluster) const;
#else
   int FindMixerTrackCluster(const WaveTrack* pLeftTrack,
                              MixerTrackCluster** hMixerTrackCluster) const;
#endif
   void LoadMusicalInstruments();

   // event handlers
   void OnSize(wxSizeEvent &evt);
   void OnTimer(wxCommandEvent &event);


public:
   // mute & solo button images: Create once and store on MixerBoard for use in all MixerTrackClusters.
   std::unique_ptr<wxImage> mImageMuteUp, mImageMuteOver, mImageMuteDown,
      mImageMuteDownWhileSolo, // the one actually alternate image
      mImageMuteDisabled, mImageSoloUp, mImageSoloOver, mImageSoloDown, mImageSoloDisabled;

   int mMuteSoloWidth;

private:
   // Track clusters are maintained in the same order as the WaveTracks.
   MixerTrackClusterArray     mMixerTrackClusters;

   MusicalInstrumentArray     mMusicalInstruments;
   AudacityProject*           mProject;
   MixerBoardScrolledWindow*  mScrolledWindow; // Holds the MixerTrackClusters and handles scrolling.
   double                     mPrevT1;
   TrackList*                 mTracks;

public:
   DECLARE_EVENT_TABLE()
};


class MixerBoardFrame final : public wxFrame
{
public:
   MixerBoardFrame(AudacityProject* parent);
   virtual ~MixerBoardFrame();

private:
   // event handlers
   void OnCloseWindow(wxCloseEvent &WXUNUSED(event));
   void OnMaximize(wxMaximizeEvent &event);
   void OnSize(wxSizeEvent &evt);
   void OnKeyEvent(wxKeyEvent &evt);

public:
   MixerBoard* mMixerBoard;

public:
   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_MIXER_BOARD__


