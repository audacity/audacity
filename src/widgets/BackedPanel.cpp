//
//  BackedPanel.cpp
//  Audacity
//
//  Created by Paul Licameli on 5/7/16.
//
//


#include "BackedPanel.h"

BackedPanel::BackedPanel(wxWindow * parent, wxWindowID id,
            const wxPoint & pos,
            const wxSize & size,
            long style)
: wxPanelWrapper(parent, id, pos, size, style)
{

}

BackedPanel::~BackedPanel()
{
}

void BackedPanel::RequestRefresh()
{
   if (mWaitRefresh)
      return;

   mWaitRefresh = true;
   Refresh();
}

void BackedPanel::OnSize(wxSizeEvent& event)
{
   event.Skip();
   RequestRefresh();
}

void BackedPanel::OnPaint(wxPaintEvent& event)
{
   HandlePaintEvent(event);
   mWaitRefresh = false;
}

BEGIN_EVENT_TABLE(BackedPanel, wxPanelWrapper)
   EVT_SIZE(BackedPanel::OnSize)
   EVT_PAINT(BackedPanel::OnPaint)
END_EVENT_TABLE()

