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
#include <wx/panel.h>
#include <wx/scrolwin.h>
#include <wx/statbmp.h>
#include <wx/stattext.h>

#include "widgets/AButton.h"
#include "widgets/ASlider.h"
#include "widgets/Meter.h"

// containment hierarchy: 
//    MixerBoardFrame -> MixerBoard -> MixerBoardScrolledWindow -> MixerTrackCluster(s)


// MixerTrackSlider is a subclass just to override OnMouseEvent, 
// so we can know when adjustment ends, so we can PushState only then.
class MixerTrackSlider : public ASlider
{
public:
   MixerTrackSlider(wxWindow * parent,
                     wxWindowID id,
                     wxString name,
                     const wxPoint & pos, 
                     const wxSize & size,
                     int style = FRAC_SLIDER,
                     bool popup = true,
                     bool canUseShift = true,
                     float stepValue = STEP_CONTINUOUS, 
                     int orientation = wxHORIZONTAL);
   virtual ~MixerTrackSlider() {};

   void OnMouseEvent(wxMouseEvent & event);

protected:
   bool mIsPan;

public:
    DECLARE_EVENT_TABLE()
};


class AudacityProject;
class MixerBoard;
class Track;
class WaveTrack;
#ifdef EXPERIMENTAL_MIDI_OUT
   class NoteTrack;
#endif

class MixerTrackCluster : public wxPanel 
{ 
public:
   MixerTrackCluster(wxWindow* parent, 
                     MixerBoard* grandParent, AudacityProject* project, 
                     WaveTrack* pLeftTrack, WaveTrack* pRightTrack = NULL, 
                     const wxPoint& pos = wxDefaultPosition, 
                     const wxSize& size = wxDefaultSize);
   virtual ~MixerTrackCluster() {};

   void HandleResize(); // For wxSizeEvents, update gain slider and meter.

   void HandleSliderGain(const bool bWantPushState = false);
   void HandleSliderPan(const bool bWantPushState = false);

   void ResetMeter();

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
   void HandleSelect(const bool bShiftDown);

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
   // mTrack is redundant, but simplifies code that operates on either 
   // mLeftTrack or mNoteTrack.
   Track* mTrack; // either mLeftTrack or mNoteTrack, whichever is not NULL
   WaveTrack* mLeftTrack; // NULL if Note Track
   WaveTrack* mRightTrack; // NULL if mono
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
   MusicalInstrument(wxBitmap* pBitmap, const wxString strXPMfilename);
   virtual ~MusicalInstrument();

   wxBitmap*      mBitmap;
   wxArrayString  mKeywords;
};
WX_DECLARE_OBJARRAY(MusicalInstrument, MusicalInstrumentArray);



// wxScrolledWindow ignores mouse clicks in client area, 
// but they don't get passed to Mixerboard.
// We need to catch them to deselect all track clusters.
class MixerBoardScrolledWindow : public wxScrolledWindow 
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

class MixerBoard : public wxWindow 
{ 
   friend class MixerBoardFrame;

public:
   MixerBoard(AudacityProject* pProject, 
               wxFrame* parent, 
               const wxPoint& pos = wxDefaultPosition, 
               const wxSize& size = wxDefaultSize);
   virtual ~MixerBoard();

   // Add clusters for any tracks we're not yet showing.
   // Update pointers for tracks we're aleady showing. 
   void UpdateTrackClusters(); 

   int GetTrackClustersWidth();
   void MoveTrackCluster(const Track* pTrack, bool bUp); // Up in TrackPanel is left in MixerBoard.
   void RemoveTrackCluster(const Track* pTrack);


   wxBitmap* GetMusicalInstrumentBitmap(const wxString name);

   bool HasSolo();

   void RefreshTrackCluster(const Track* pTrack, bool bEraseBackground = true);
   void RefreshTrackClusters(bool bEraseBackground = true);
   void ResizeTrackClusters();

   void ResetMeters();

   void UpdateName(const Track* pTrack);
   void UpdateMute(const Track* pTrack = NULL); // NULL means update for all tracks.
   void UpdateSolo(const Track* pTrack = NULL); // NULL means update for all tracks.
   void UpdatePan(const Track* pTrack);
   void UpdateGain(const Track* pTrack);
   
   void UpdateMeters(const double t1, const bool bLoopedPlay);

   void UpdateWidth();

private:
   void CreateMuteSoloImages();
   int FindMixerTrackCluster(const Track* pTrack, 
                              MixerTrackCluster** hMixerTrackCluster) const;
   void LoadMusicalInstruments();

   // event handlers
   void OnSize(wxSizeEvent &evt);


public:
   // mute & solo button images: Create once and store on MixerBoard for use in all MixerTrackClusters.
   wxImage* mImageMuteUp;
   wxImage* mImageMuteOver;
   wxImage* mImageMuteDown;
   wxImage* mImageMuteDownWhileSolo; // the one actually alternate image
   wxImage* mImageMuteDisabled;
   wxImage* mImageSoloUp;
   wxImage* mImageSoloOver;
   wxImage* mImageSoloDown;
   wxImage* mImageSoloDisabled;

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


class MixerBoardFrame : public wxFrame 
{ 
public:
   MixerBoardFrame(AudacityProject* parent);
   virtual ~MixerBoardFrame();

private:
   // event handlers
   void OnCloseWindow(wxCloseEvent &WXUNUSED(event));
   void OnMaximize(wxMaximizeEvent &event);
   void OnSize(wxSizeEvent &evt);

public:
   MixerBoard* mMixerBoard;

public:
   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_MIXER_BOARD__


