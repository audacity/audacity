/* macaboutbox.c - Display the "about box" of the application. */
/* Written by Brian Kendig. */
/* The functions here are only used by macint.c. */

//#include <THINK.h>
#include <Dialogs.h>
#include <Fonts.h>
#include <Menus.h>
#include <Quickdraw.h>
#include <Resources.h>
#include <ToolUtils.h>
#include <Traps.h>
#include <Windows.h>
#include "macint.h"
#define NIL ((void *) 0)

static DialogPtr aboutBox;
extern Boolean hasColorQD;

static enum {
    theOKButton		= 1,
    theOKOutline	= 2,
    theIcon			= 3,
    theName			= 4,
    theAboutText	= 5,
    theCopyright	= 6
} ;

pascal void DrawOKOutline (WindowPtr dialogWindow, short theItem) {
    PenState	oldPen;
    short		iType;
    Handle		iHandle;
    Rect		iRect;

    GetPenState (&oldPen);
    PenNormal ();
    PenSize (3,3);

    GetDialogItem (aboutBox, theOKButton, &iType, &iHandle, &iRect);
    InsetRect (&iRect, -4, -4);
    FrameRoundRect (&iRect, 16, 16);

    SetPenState (&oldPen);
}

pascal void DrawIcon (WindowPtr dialogWindow, short theItem) {
    short		iType;
    Handle		iHandle;
    Rect		iRect;

    GetDialogItem (aboutBox, theIcon, &iType, &iHandle, &iRect);
    PlotIcon (&iRect, GetResource ('ICN#', 128));
}

pascal void DrawName (WindowPtr dialogWindow, short theItem) {
    short	iType;
    Handle	iHandle;
    Rect	iRect;
    Str255	string;

    TextFont (kFontIDHelvetica);
    TextSize (24);
    TextFace (0);
    GetDialogItem (aboutBox, theName, &iType, &iHandle, &iRect);
    GetIndString (string, STRINGS_RES, 1);
    TETextBox (string+1, string[0], &iRect, teFlushLeft);
}

pascal void DrawAboutText (WindowPtr dialogWindow, short theItem) {
    short		iType;
    Handle		iHandle;
    Rect		iRect;
    Str255		string;

    TextFont (kFontIDMonaco);
    TextSize (9);
    TextFace (0);
    GetDialogItem (aboutBox, theAboutText, &iType, &iHandle, &iRect);
    GetIndString (string, STRINGS_RES, 2);
    TETextBox (string+1, string[0], &iRect, teFlushLeft);
}

pascal void DrawCopyright (WindowPtr dialogWindow, short theItem) {
    short		iType;
    Handle		iHandle;
    Rect		iRect;
    Str255		string;

    TextFont (systemFont);
    TextSize (12);
    TextFace (0);
    GetDialogItem (aboutBox, theCopyright, &iType, &iHandle, &iRect);
    GetIndString (string, STRINGS_RES, 3);
    TETextBox (string+1, string[0], &iRect, teFlushLeft);
}

void DoAboutBox (void) {
    short itemType, itemHit = 0;
    Handle itemHandle;
    Rect aboutRect;
    short width, hight;
    PicHandle aboutPict;

    aboutPict = GetPicture(ABOUT_PICT);
    aboutRect = (*aboutPict)->picFrame;
    width = aboutRect.right - aboutRect.left;
    hight = aboutRect.bottom - aboutRect.top;
    
    aboutBox = GetNewDialog (ABOUT_BOX, NIL, (WindowPtr) -1);
    SizeWindow(aboutBox, width, hight, false);
    
    ShowWindow (aboutBox);
    SetPort(aboutBox);
    DrawPicture(aboutPict, &(*aboutPict)->picFrame);
    
    //itemHit = 0;
    //while (itemHit != ok) ModalDialog (NIL, &itemHit);
    while (!Button());
    
    DisposeDialog (aboutBox);
    
    FlushEvents(everyEvent, 0); // dmazzoni
}
