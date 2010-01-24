#ifndef MUSIK_BTNDOWN_EVT
#define MUSIK_BTNDOWN_EVT

#include "wx/wx.h"

class CBtnDownEvt : public wxEvtHandler
{
 public:
  CBtnDownEvt( wxBitmapButton *parent, wxBitmap *down, wxBitmap *up ){ pParent = parent; pBtnDown = down; pBtnUp = up; };
  ~CBtnDownEvt(){};
  
  void OnLeftDown	( wxMouseEvent& event );
  void OnLeftUp	( wxMouseEvent& event );
  void OnMouseMove( wxMouseEvent& event );
  void OnEraseBackground(wxEraseEvent& ) {}
  DECLARE_EVENT_TABLE()
    private:
  wxBitmapButton	*pParent;
  wxBitmap		*pBtnDown;
  wxBitmap		*pBtnUp;
};

#endif
