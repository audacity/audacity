/**********************************************************************

  Sneedacity: A Digital Audio Editor

  AttachableScrollBar.h

  James Crook

  A scroll bar that can be attached to multiple items and so control
  their scrolling.

  Sneedacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

**********************************************************************/

#ifndef __SNEEDACITY_ATTACHABLE_SCROLL_BAR__
#define __SNEEDACITY_ATTACHABLE_SCROLL_BAR__

#include <wx/scrolbar.h> // to inherit

class ViewInfo;

class SNEEDACITY_DLL_API AttachableScrollBar final :
   public wxScrollBar
{
public:
   AttachableScrollBar(
   wxWindow* parent,
   wxWindowID id,
   const wxPoint& pos = wxDefaultPosition,
   const wxSize& size = wxDefaultSize,
   long style = wxSB_HORIZONTAL);
public:
   ~AttachableScrollBar(void);
   void OnScroll(wxScrollEvent & event);
   void SetViewInfo( ViewInfo * view );

   void SetScrollBarFromViewInfo();
   void SetViewInfoFromScrollBar();

   ViewInfo * mpViewInfo;
   DECLARE_EVENT_TABLE()
};

#endif // __SNEEDACITY_ATTACHABLE_SCROLL_BAR__
