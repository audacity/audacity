#ifndef WXPLAYERFRAME_H
#define WXPLAYERFRAME_H

#include "wx/wx.h"
#include "play.h"
#include "wxplayctrl.h"

class wxPlayerFrame : public wxFrame 
{
 public:
  wxPlayerFrame(sbsmsplayer *player);
  ~wxPlayerFrame();

  void OnPause();
  void OnPlay();
  void OnOpen(wxCommandEvent& WXUNUSED(event));
  void OnSave(wxCommandEvent& WXUNUSED(event));
  void OnExit(wxCommandEvent& WXUNUSED(event));

  DECLARE_EVENT_TABLE()
 protected:
  wxString openFileName;
  sbsmsplayer *player;
  PlayCtrl *playCtrl;
};

#endif
