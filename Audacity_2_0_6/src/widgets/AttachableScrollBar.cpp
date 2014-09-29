/**********************************************************************

  Audacity: A Digital Audio Editor

  AttachableScrollBar.cpp

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

*****************************************************************//**

\class AttachableScrollBar
\brief An AttachableScrollBar is a scroll bar that can be attached
to multiple items and so control their scrolling.  Uses floats
internally, not ints, allowing for (external) control of zooming.

  Limitations:

  - Currently horizontal only.
  - Attached item repainting not yet supported.  Instead for now place the
    attached windows in an InvisiblePanel, and rely on refreshing of
    the invisible panel to refresh each of the objects.
  - Currently closely tied to the mViewInfo class.  This will change
    to make the class much more generic.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/wxprec.h>
#include "AttachableScrollBar.h"
#include "../ViewInfo.h"


BEGIN_EVENT_TABLE(AttachableScrollBar, wxScrollBar)
   EVT_SCROLL(AttachableScrollBar::OnScroll)
END_EVENT_TABLE()

AttachableScrollBar::AttachableScrollBar(
   wxWindow* parent,
   wxWindowID id,
   const wxPoint& pos,
   const wxSize& size,
   long style) :
   wxScrollBar( parent, id, pos, size, style )
{
   mpViewInfo = NULL;
}

AttachableScrollBar::~AttachableScrollBar(void)
{
}

// Essentially a float to int conversion.
void AttachableScrollBar::SetScrollBarFromViewInfo()
{
   ViewInfo & mViewInfo = *mpViewInfo;

   mViewInfo.sbarTotal = (int) (mViewInfo.total * mViewInfo.zoom);
   mViewInfo.sbarScreen = (int) (mViewInfo.screen * mViewInfo.zoom);
   mViewInfo.sbarH = (int) (mViewInfo.h * mViewInfo.zoom);

   SetScrollbar(mViewInfo.sbarH, mViewInfo.sbarScreen,
                        mViewInfo.sbarTotal, mViewInfo.sbarScreen, TRUE);

   mViewInfo.lastZoom = mViewInfo.zoom;
}

// Essentially an int to float conversion.
void AttachableScrollBar::SetViewInfoFromScrollBar()
{
   ViewInfo & mViewInfo = *mpViewInfo;

   int hlast = mViewInfo.sbarH;

   mViewInfo.sbarH = GetThumbPosition();

   if (mViewInfo.sbarH != hlast) {
      mViewInfo.h = mViewInfo.sbarH / mViewInfo.zoom;

      if (mViewInfo.h > mViewInfo.total - mViewInfo.screen)
         mViewInfo.h = mViewInfo.total - mViewInfo.screen;
      if (mViewInfo.h < 0.0)
         mViewInfo.h = 0.0;
   }
}

// Used to associated a ViewInfo structure with a scrollbar.
void AttachableScrollBar::SetViewInfo( ViewInfo * view )
{
   mpViewInfo = view;
   SetScrollBarFromViewInfo();
}

void AttachableScrollBar::OnScroll(wxScrollEvent & event)
{
   if( mpViewInfo == NULL )
   {
      event.Skip();
      return;
   }
   SetViewInfoFromScrollBar();
   event.Skip(); // This is so that the parent control can refresh whatever it needs to...
}
