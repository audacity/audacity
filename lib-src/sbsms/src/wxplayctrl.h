#ifndef PLAYCTRL_H
#define PLAYCTRL_H

#include "wx/wx.h"
#include "btndown.h"
#include "play.h"
class wxPlayerFrame;

class wxStaticText_NoFlicker : public wxStaticText
{
 public:
  wxStaticText_NoFlicker(wxWindow* parent,
			 wxWindowID id,
			 const wxString& label,
			 const wxPoint& pos = wxDefaultPosition,
			 const wxSize& size = wxDefaultSize,
			 long style = 0,
			 const wxString& name= wxStaticTextNameStr )
    :wxStaticText(parent,id,label,pos,size,style,name){}
  void OnEraseBackground(wxEraseEvent& ) {}
  DECLARE_EVENT_TABLE()
};

class PlayCtrl : public wxPanel 
{
 public:
  PlayCtrl(wxWindow *parent, sbsmsplayer *player);
  ~PlayCtrl();

  void OnPlayPause( wxCommandEvent& WXUNUSED(event) );
  void OnTrack( wxScrollEvent &event);
  void OnRelease( wxScrollEvent &event);
  void OnTimer( wxTimerEvent& WXUNUSED(event) );
  void OnClickTimeDisplay( wxMouseEvent& event );
  wxBitmap	bmPlay,		bmPlayDown;
  wxBitmap	bmPause,	bmPauseDown;

  wxPlayerFrame *parent;

  wxBitmapButton *btnPlayPause;  
  wxSlider *slVolume;
  wxSlider *slSeek;
  wxSlider *slRate;
  wxSlider *slPitch;
  wxStaticText_NoFlicker *stVolume;
  wxStaticText_NoFlicker *stRate;
  wxStaticText_NoFlicker *stPitch;
  wxStaticText_NoFlicker *stCurtime;

#ifdef __WXMSW__
  CBtnDownEvt *pPlayEvt;
  CBtnDownEvt *pPauseEvt;
#endif

  DECLARE_EVENT_TABLE()

protected:
  wxString getTime(real time);
  wxString getTimeLeft(real time, real duration);

  bool bWasPlaying;
  bool bPlay;
  sbsmsplayer *player;
  int nTimeDisplayMode;
  void PlayBtnToPauseBtn();
  void PauseBtnToPlayBtn();
  
  void UpdateTime();
  void StartTimer();
  void KillTimer();
  wxTimer *pSecTimer;
};

#endif
