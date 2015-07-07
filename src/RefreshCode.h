/**********************************************************************

Audacity: A Digital Audio Editor

RefreshCode.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_REFRESH_CODE__
#define __AUDACITY_REFRESH_CODE__

namespace RefreshCode
{

   // Bit flags for composing a result that directs the
   // framework whether to continue, and what to redraw
   enum {
      RefreshNone = 0,

      Cancelled = 0x1,
      RefreshCell = 0x2,        // Refresh the cell passed to Click()
      RefreshLatestCell = 0x4,  // Refresh the cell passed to latest call
      RefreshAll = 0x8,
      FixScrollbars = 0x10,
      Resize = 0x20,
      UpdateSelection = 0x40,
      UpdateVRuler = 0x80,      // of the clicked track
      EnsureVisible = 0x100,    // for the clicked track
      DrawOverlays = 0x200,

      DestroyedCell = 0x8000,     // true if the CLICKED cell was destroyed
   };

}



#endif
