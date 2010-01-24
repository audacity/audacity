#include "BtnDown.h"

//-------------------//
//--- CBtnDownEvt ---//
//-------------------//
BEGIN_EVENT_TABLE(CBtnDownEvt, wxEvtHandler)
  EVT_LEFT_DOWN	(CBtnDownEvt::OnLeftDown) 
  EVT_LEFT_UP		(CBtnDownEvt::OnLeftUp)
  EVT_MOTION		(CBtnDownEvt::OnMouseMove)
END_EVENT_TABLE()

void CBtnDownEvt::OnLeftDown( wxMouseEvent& event )
{
  pParent->SetBitmapLabel( *pBtnDown );
  pParent->Refresh();
  event.Skip();
}

void CBtnDownEvt::OnLeftUp( wxMouseEvent& event )
{
  pParent->SetBitmapLabel( *pBtnUp );
  pParent->Refresh();
  event.Skip();
}

void CBtnDownEvt::OnMouseMove( wxMouseEvent& event )
{
  if ( event.LeftIsDown() )
    pParent->SetBitmapLabel( *pBtnUp );
  event.Skip();
}
