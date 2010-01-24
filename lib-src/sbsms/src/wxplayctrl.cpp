#include "wxplayctrl.h"
#include "wxplayer.h"
#include "wxplayerframe.h"

#include "../resource/Play.xpm"
#include "../resource/PlayDown.xpm"
#include "../resource/Pause.xpm"
#include "../resource/PauseDown.xpm"

#define PLAYPAUSE 10000
#define TIMER 10001
#define SEEK 10002
#define RATE 10003
#define PITCH 10004
#define VOLUME 10005

#define RATE_MAX 200
#define RATE_MIN 25
#define RATE_DEFAULT 100
#define PITCH_MIN -12
#define PITCH_DEFAULT 0
#define PITCH_MAX 12

#define SEEK_MAX 512

BEGIN_EVENT_TABLE(PlayCtrl, wxPanel)
  EVT_BUTTON(PLAYPAUSE,PlayCtrl::OnPlayPause)
  EVT_TIMER(TIMER,PlayCtrl::OnTimer)
  EVT_SCROLL_THUMBTRACK(PlayCtrl::OnTrack)
  EVT_SCROLL_THUMBRELEASE(PlayCtrl::OnRelease)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(wxStaticText_NoFlicker,wxStaticText)
EVT_ERASE_BACKGROUND(wxStaticText_NoFlicker::OnEraseBackground)
END_EVENT_TABLE()

PlayCtrl :: PlayCtrl(wxWindow *parent, sbsmsplayer *player)
  : wxPanel( parent, -1, wxDefaultPosition, wxDefaultSize, wxCLIP_CHILDREN| wxTAB_TRAVERSAL )
{
  this->parent = (wxPlayerFrame*)parent;
  this->player = player;
  bmPlay = wxBitmap(Play_xpm);
  bmPlayDown = wxBitmap(PlayDown_xpm);
  bmPause = wxBitmap(Pause_xpm);
  bmPauseDown = wxBitmap(PlayDown_xpm);

  btnPlayPause = new wxBitmapButton(this, PLAYPAUSE, bmPlay,wxDefaultPosition,wxDefaultSize,0);

#ifndef __WXMSW__
  btnPlayPause->SetBitmapFocus( bmPlayDown );
#else
  pPlayEvt = new CBtnDownEvt( btnPlayPause, &bmPlayDown, &bmPlay );
  pPauseEvt = new CBtnDownEvt( btnPlayPause, &bmPauseDown, &bmPause );
  btnPlayPause->PushEventHandler( pPlayEvt );
#endif
  
  slVolume = new wxSlider( this,VOLUME,90,0,100);
  slSeek = new wxSlider( this,SEEK,0,0,SEEK_MAX);
  slSeek->Connect(wxEVT_RIGHT_DOWN,wxMouseEventHandler(PlayCtrl::OnClickTimeDisplay),NULL,this);

  slRate = new wxSlider( this,RATE,RATE_DEFAULT,RATE_MIN,RATE_MAX);
  slPitch = new wxSlider( this,PITCH,PITCH_DEFAULT,PITCH_MIN,PITCH_MAX);

  stVolume = new wxStaticText_NoFlicker( this, -1, wxT("Volume: 90\%"), wxDefaultPosition, wxDefaultSize, 0 );
  stRate = new wxStaticText_NoFlicker( this, -1, wxT("Rate: 100\%"), wxDefaultPosition, wxDefaultSize, 0 );
  stPitch = new wxStaticText_NoFlicker( this, -1, wxT("Pitch: 0   "), wxDefaultPosition, wxDefaultSize, 0 );

  wxGridSizer *hsButtons = new wxGridSizer(1);
  hsButtons->Add( btnPlayPause ,0,wxALIGN_CENTRE_HORIZONTAL | wxLEFT ,1);
  
  stCurtime = new wxStaticText_NoFlicker( this, -1, wxT(""), wxDefaultPosition, wxDefaultSize, 0 );
  nTimeDisplayMode = 0;

  wxBoxSizer *hsPlay = new wxBoxSizer( wxHORIZONTAL );
  wxBoxSizer *vsPlay = new wxBoxSizer( wxVERTICAL );
  hsPlay->Add( hsButtons, 1, wxEXPAND|wxALL, 2 );
  hsPlay->AddSpacer(10);
  hsPlay->Add( slSeek, 0, wxALIGN_CENTER );
  hsPlay->AddSpacer(10);
  hsPlay->Add( stCurtime );

  wxFlexGridSizer *gsControls = new wxFlexGridSizer(3,2,10,10);
  gsControls->Add(stVolume);
  gsControls->Add(slVolume);
  gsControls->Add(stRate);
  gsControls->Add(slRate);
  gsControls->Add(stPitch);
  gsControls->Add(slPitch);

  vsPlay->AddSpacer(10);
  vsPlay->Add(gsControls);
  vsPlay->AddSpacer(10);
  vsPlay->Add(hsPlay);
  vsPlay->AddSpacer(10);
  
  SetSizer( vsPlay);

  pSecTimer = NULL;
  UpdateTime();
  StartTimer();
  bPlay = false;
  bWasPlaying = false;
  vsPlay->Fit(this);
  Layout();
}

PlayCtrl::~PlayCtrl()
{
  KillTimer();

#ifdef __WXMSW__
  btnPlayPause->PopEventHandler();
  delete pPlayEvt;
  delete pPauseEvt;
#endif
}

void PlayCtrl::OnTrack(wxScrollEvent &event)
{
  if(event.GetId() == SEEK) {
    if(player->isPlaying()) {
      bWasPlaying = true;
      KillTimer();
      player->pause();
    }
  } else if(event.GetId() == RATE) {
    int intval = slVolume->GetValue();
    stVolume->SetLabel( wxString::Format( wxT("volume: %d"),intval) + wxT("\%") );
  } else if(event.GetId() == RATE) {
    int intval = slRate->GetValue();
    stRate->SetLabel( wxString::Format( wxT("Rate: %d"),intval) + wxT("\%") );
  } else if(event.GetId() == PITCH) {
    int intval = slPitch->GetValue();
    stPitch->SetLabel( wxString::Format( wxT("Pitch: %s%d"),intval>0?wxT("+"):wxT(""),intval) );
  }
}

void PlayCtrl::OnRelease(wxScrollEvent &event)
{
  if(event.GetId() == SEEK) {
    real fPos = (real)slSeek->GetValue()/SEEK_MAX;
    player->setPos(fPos);
    if(bWasPlaying) {
      player->play();
      StartTimer();
      bWasPlaying = false;
    }
  } else if(event.GetId() == VOLUME) {
    int intval = slVolume->GetValue();
    real val = (real)intval*.01f;
    player->setVolume(val);
    stVolume->SetLabel( wxString::Format( wxT("Volume: %d"),intval) + wxT("\%") );
  } else if(event.GetId() == RATE) {
    int intval = slRate->GetValue();
    real val = (real)intval*.01f;
    player->setRate(val);
    stRate->SetLabel( wxString::Format( wxT("Rate: %d"),intval) + wxT("\%") );
  } else if(event.GetId() == PITCH) {
    int intval = slPitch->GetValue();
    real val = (real)intval;
    real ratio = pow(2.0,-val/12.0);
    player->setRatio(ratio);
    stPitch->SetLabel( wxString::Format( wxT("Pitch: %s%d"),intval>0?wxT("+"):wxT(""),intval) );
  }
}

void PlayCtrl::OnTimer( wxTimerEvent& WXUNUSED(event) )
{
  if( player->isPlaying() ) {
    UpdateTime();
  } else if( player->isDonePlaying() ){
    parent->OnPause();
    player->setPos(0.0f);
    UpdateTime();
    PauseBtnToPlayBtn();
    bPlay = false;
  }
}

void PlayCtrl::StartTimer()
{
  if ( pSecTimer == NULL ) {
    pSecTimer = new wxTimer(this, TIMER );
    pSecTimer->Start( 100, false );
  }
}

void PlayCtrl::KillTimer()
{
  if(pSecTimer) {
    pSecTimer->Stop();
    delete pSecTimer;
    pSecTimer = NULL;
  }
}

void PlayCtrl::PlayBtnToPauseBtn()
{
#ifndef __WXMSW__
  btnPlayPause->SetBitmapLabel( bmPause );
  btnPlayPause->SetBitmapFocus( bmPauseDown );
#else
  btnPlayPause->SetBitmapLabel( bmPause );
  btnPlayPause->PopEventHandler();
  btnPlayPause->PushEventHandler( pPauseEvt );
#endif
  btnPlayPause->Refresh();
}

void PlayCtrl::PauseBtnToPlayBtn()
{
#ifndef __WXMSW__
  btnPlayPause->SetBitmapLabel( bmPlay );
  btnPlayPause->SetBitmapFocus( bmPlayDown );
#else
  btnPlayPause->SetBitmapLabel( bmPlay );
  btnPlayPause->PopEventHandler();
  btnPlayPause->PushEventHandler( pPlayEvt );
#endif
  btnPlayPause->Refresh();
}

void PlayCtrl::OnPlayPause( wxCommandEvent& WXUNUSED(event) )	
{	
  if(bPlay) {
    if(player->pause()) {
      parent->OnPause();
      bPlay = false;
      PauseBtnToPlayBtn();
      KillTimer();
    }
  } else {
    if(player->play()) {
      parent->OnPlay();
      bPlay = true;
      PlayBtnToPauseBtn();
      StartTimer();
    }
  }
}

void PlayCtrl::OnClickTimeDisplay(wxMouseEvent & WXUNUSED(event))
{
  ++nTimeDisplayMode %= 2;
}

wxString PlayCtrl::getTime(real time)
{
  int h = (int)(time/3600.0f);
  time -= h*3600.0f;
  int m = (int)(time/60.0f);
  time -= m*60.0f;
  int s = (int)time;
  time -= s;
  int cs = (int)(time*100.0f);
  return wxString::Format( wxT("%.2d:%.2d:%.2d"),m,s,cs);
}

wxString PlayCtrl::getTimeLeft(real time, real duration)
{
  return getTime(duration-time);
}

void PlayCtrl::UpdateTime()
{
  real duration = player->getDuration(); 
  real time = player->getTime();
  real fPos = duration?SEEK_MAX*time/duration : 0;
	    
#ifdef __WXGTK__
  if ( fPos < 2.0f )
    fPos = 2.0f;
#endif
  
  slSeek->SetValue( (int)fPos );
  if(nTimeDisplayMode == 0) {
    stCurtime->SetLabel(wxT(" ") + getTime(time));
  } else {
    stCurtime->SetLabel(wxT("-") + getTimeLeft(time,duration));
  }
}
