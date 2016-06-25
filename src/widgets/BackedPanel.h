//
//  BackedPanel.h
//  Audacity
//
//  Created by Paul Licameli on 5/7/16.
//
//

#ifndef __AUDACITY_BACKED_PANEL__
#define __AUDACITY_BACKED_PANEL__

#include "../MemoryX.h"
#include <wx/dcmemory.h>
#include "wxPanelWrapper.h"

class AUDACITY_DLL_API BackedPanel /* not final */ : public wxPanelWrapper {
public:
   BackedPanel(wxWindow * parent, wxWindowID id,
               const wxPoint & pos,
               const wxSize & size,
               long style);

   ~BackedPanel();

   wxDC &GetBackingDC();

   wxDC &GetBackingDCForRepaint();

   void ResizeBacking();

   void RepairBitmap(wxDC &dc, wxCoord x, wxCoord y, wxCoord width, wxCoord height);

   void DisplayBitmap(wxDC &dc);

   void OnSize(wxSizeEvent & event);

private:
   std::unique_ptr<wxBitmap> mBacking;
   wxMemoryDC mBackingDC;
   bool mResizeBacking {};
   
   DECLARE_EVENT_TABLE()
};


#endif
