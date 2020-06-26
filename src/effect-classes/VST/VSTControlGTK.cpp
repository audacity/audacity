/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTControlGTK.cpp

  Leland Lucius

**********************************************************************/

#include "VSTControlGTK.h"

#include <wx/dynlib.h>
#include <wx/sizer.h>

#if 0
static int trappedErrorCode = 0;
static int X11TrapHandler(Display *, XErrorEvent *err)
{
    return 0;
}
#endif

VSTControl::VSTControl()
:  VSTControlBase()
{
   mXdisp = 0;
   mXwin = 0;
}

VSTControl::~VSTControl()
{
   if (mXwin)
   {
      mLink->callDispatcher(effEditClose, 0, (intptr_t)mXdisp, (void *)mXwin, 0.0);
      mXdisp = 0;
      mXwin = 0;
   }
}

bool VSTControl::Create(wxWindow *parent, VSTEffectLink *link)
{
   if (!VSTControlBase::Create(parent, link))
   {
      return false;
   }

   VstRect *rect;

   // Some effects like to have us get their rect before opening them.
   mLink->callDispatcher(effEditGetRect, 0, 0, &rect, 0.0);

   // Make sure the parent has a window
   if (!gtk_widget_get_realized(GTK_WIDGET(m_wxwindow)))
   {
      gtk_widget_realize(GTK_WIDGET(m_wxwindow));
   }

   GdkWindow *gwin = gtk_widget_get_window(GTK_WIDGET(m_wxwindow));
   mXdisp = GDK_WINDOW_XDISPLAY(gwin);
   mXwin = GDK_WINDOW_XID(gwin);

   mLink->callDispatcher(effEditOpen, 0, (intptr_t)mXdisp, (void *)mXwin, 0.0);

   // Get the final bounds of the effect GUI
   mLink->callDispatcher(effEditGetRect, 0, 0, &rect, 0.0);

   // Add the effect host window to the layout
   SetMinSize(wxSize(rect->right - rect->left, rect->bottom - rect->top));

   // Must get the size again since SetPeer() could cause it to change
   SetInitialSize(GetMinSize());

   return true;
}
