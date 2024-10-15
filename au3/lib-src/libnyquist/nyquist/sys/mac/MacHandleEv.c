#include <MacTypes.h>
#include <Quickdraw.h>
#include <Windows.h>
#include <Controls.h>
#include <ToolUtils.h>
#include "macint.h"

extern WindowPtr gCommandWin, gGraphicsWin;
extern Boolean gCommandWinResized;
extern Rect dragRect, sizeRect;

//=============================================================================
// Hanlde Mouse Down Events
//=============================================================================

void DoMouseDown (EventRecord *theEvent) {
    WindowPtr whichWindow;
    short int thePart = FindWindow (theEvent->where, &whichWindow);

    switch (thePart) {
        case inSysWindow:
            SystemClick (theEvent, whichWindow);
            break;
        case inDrag:
            DragWindow (whichWindow, theEvent->where, &dragRect);
            break;
        case inMenuBar: {
            long choice;
            AdjustMenus ();
            choice = MenuSelect (theEvent->where);
            if (choice) DoMenu (choice);
            break;
        }
        case inGoAway:
            if ((whichWindow == gGraphicsWin)
                    && (TrackGoAway (whichWindow, theEvent->where)))
                HideGrafWin ();
            break;
        case inContent:
            if ((FrontWindow () == gCommandWin) && (whichWindow == gCommandWin))
                DoContent (theEvent);
            else SelectWindow (whichWindow);
            break;
        case inGrow:
        case inZoomIn:
        case inZoomOut: {
            long newSize;
            GrafPtr oldPort;
            if (thePart == inGrow) newSize = GrowWindow (whichWindow, theEvent->where, &sizeRect);
            if (((thePart == inGrow) && newSize)
                || ((thePart != inGrow) && TrackBox (whichWindow, theEvent->where, thePart))) {
                GetPort (&oldPort);
                SetPort (whichWindow);
                EraseRect (&whichWindow->portRect);
                if (thePart == inGrow) SizeWindow (whichWindow, LoWord (newSize), HiWord (newSize), -1);
                else ZoomWindow (whichWindow, thePart, 0);
                gCommandWinResized = true;
                InvalRect (&whichWindow->portRect);
                SetPort (oldPort);
            }
            break;
        }
    }
}
