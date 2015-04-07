/* macint.c - macintosh interface routines for xlisp 2.1e */
/* Written by Brian Kendig. */
/* The functions here are only called by macstuff.c. */

#include <Events.h>
#include <Gestalt.h>
#include <Memory.h>
#include <Menus.h>
#include <Events.h>
#include <Quickdraw.h>
#include <StandardFile.h>
#include <TextEdit.h>
#include <ToolUtils.h>
#include <Traps.h>
#include <Windows.h>
#include <Controls.h>
/* #include <ControlDefinitions.h> */
#include <SIOUX.h>
#include <AppleEvents.h>
#include "macint.h"
/* #define FALSE 0
#define TRUE 1 */
#define NIL ((void *) 0)


#include "MacCommandWin.h"
#include "macaboutbox.h"
#include "MacDrag.h"
#include "MacHandleEv.h"
#include "macstuff.h"
#include "stdio.h"
#define TEXTREC		(*hTERec)  /* the command window text record */
#define TEXTCHAR(i)	((*(TEXTREC->hText))[i])

// Struct for apple event handling
typedef struct AEventList {
    AEEventClass evclass;
    AEEventID evid;
    void *handler;
    long refcon;
} AEventList, *AEventListPtr;

//===========================================================================
// GLOBALS DEFINED HERE USE MacGlobals.h FOR ACCESS
//===========================================================================

// Menu handles
MenuHandle appleMenu, fileMenu, editMenu, controlMenu;

/* command and graphics windows */
WindowPtr		gCommandWin, gGraphicsWin;
WindowRecord	commandWinRec, bwGraphicsWinRec;
CWindowRecord	colorGraphicsWinRec;
Boolean			gGraphicsShown, gCommandWinResized = false;

// Screen size stuff
Rect dragRect, sizeRect;
int screenWidth, screenHeight;					/* screen dimensions */
int sHorizontal, sVertical, sWidth, sHeight;	/* command win, split screen */
int gHorizontal, gVertical, gWidth, gHeight;	/* graphics win, split screen */

// The Text handle
TEHandle hTERec;

/* output is buffered */
//Handle hOutputBuffer = NULL;
int outputBufferLength = 0;

// Allocate space for UPPs
ControlActionUPP uppScrollProc;	
TEClickLoopUPP uppScrollClickLoop;
//AEEventHandlerUPP uppAEOpenFiles, uppAEQuit;

// Text related globals
CharsHandle	pastedTextH = NULL;					/* a handle to pasted text */
int			pastedLength = 0;					/* how many chars there are in the paste buffer */
int			flashTime = 0, cursorBeforeFlash;	/* for flashing cursor when parens match */
char		recentChar;							/* the last character typed */
RgnHandle		gMouseRgn;						// holds current mouse regin

/* miscellaneous stuff */
Boolean		gInBackground;						/* are we in background or not */
int wneImplemented;
unsigned long startupTicks;
Boolean hasColorQD;

short howManyFiles = 0, whichFile = 0;  /* keep track of files opened from Finder */


// Prototypes
static pascal OSErr AEQuit (AppleEvent *theAppleEvent, AppleEvent *theReply, long Refcon);
static pascal OSErr AEOpenFiles (AppleEvent *theAppleEvent, AppleEvent *theReply, long Refcon);
pascal Boolean ScrollClickLoop (void);
pascal void ScrollProc (ControlHandle control, short thePart);
Rect SetTERect (void);
void FlushOutput (void);

void ShowGrafWin (void) {
    /* make the graphics window visible */
    ShowWindow (gGraphicsWin);
    SelectWindow (gGraphicsWin);
    SetMenuItemText (controlMenu, SHOW_GRAPHICS, "\pHide Graphics");
    //AdjustCursor ();
    gGraphicsShown = true;
}

void HideGrafWin (void) {
    /* hide the graphics window */
    HideWindow (gGraphicsWin);
    SetMenuItemText (controlMenu, SHOW_GRAPHICS, "\pShow Graphics");
    gGraphicsShown = false;
}


static void UpdateGraphWindow ()
{
    BeginUpdate (gGraphicsWin);
    EndUpdate (gGraphicsWin);
}
void InitMac (void) {
//	{  /* set up memory properly */
//		int i;
        // fix this later. -EAD
        //if (DefltStack < STACKMIN) SetApplLimit (CurStackBase - STACKMIN); 
//		MaxApplZone ();
//		for (i = 0; i < MASTERS; i++) MoreMasters ();
//	}
    AEventListPtr theAppleEvent;
    AEventList theEventList[] = {
    { kCoreEventClass, kAEOpenDocuments, AEOpenFiles, 0 },
    { kCoreEventClass, kAEQuitApplication, AEQuit, 0 },
    { 0, 0, nil, 0 }
    };
    int i;

    /* do all the necessary initialization mumbo-jumbo */
    if (StackSpace() < STACKMIN) 
        SetApplLimit(GetApplLimit() - STACKMIN);
    MaxApplZone();
    /* printf("New StackSpace %lx GetApplLimit %lx\n",
        StackSpace(), GetApplLimit()); */
    for (i = 0; i < MASTERS; i++) MoreMasters ();
    /* getchar(); */
        
    /* initialize the toolbox */
    InitGraf (&qd.thePort);
    InitFonts ();
    FlushEvents (everyEvent, 0);
    InitWindows ();
    InitMenus ();
    TEInit ();
    InitDialogs (NIL);
    InitCursor ();

    // Setup Callbacks
    uppScrollClickLoop = NewTEClickLoopProc(ScrollClickLoop);
    uppScrollProc = NewControlActionProc(ScrollProc);
    
    // Handlers for core apple events		
    for (theAppleEvent = theEventList; theAppleEvent->handler; theAppleEvent++)
        if (AEInstallEventHandler(theAppleEvent->evclass, theAppleEvent->evid, NewAEEventHandlerProc((ProcPtr)theAppleEvent->handler),
                    theAppleEvent->refcon, 0) != noErr);
    
    // Set up the SIOUX window
    SIOUXSettings.initializeTB = FALSE;			//Toolbox is alread inited
    SIOUXSettings.setupmenus = FALSE;			//keep the csound menus
    SIOUXSettings.autocloseonquit = TRUE;		//close sioux without asking for save
    SIOUXSettings.showstatusline = FALSE;		//no status line
    SIOUXSettings.asktosaveonclose = FALSE;		//don't ask to save
    SIOUXSettings.toppixel = 40;
    SIOUXSettings.leftpixel = 5;
    
    /* see if we have WaitNextEvent and Color Quickdraw */
    wneImplemented = (NGetTrapAddress (_WaitNextEvent, ToolTrap) != NGetTrapAddress (_Unimplemented, ToolTrap));
    if (NGetTrapAddress ((short) Gestalt, ToolTrap) != NGetTrapAddress (_Unimplemented, ToolTrap)) {
        long returnCode;
        OSErr err = Gestalt (gestaltQuickdrawVersion, &returnCode);
        hasColorQD = ((err == noErr) && (returnCode >= gestalt8BitQD));
    } else hasColorQD = false;

    {  /* set up menus */
        Handle theMenuBar = GetNewMBar (MBAR_RES);
        SetMenuBar (theMenuBar);
        appleMenu = (MenuHandle)GetMenuHandle (APPLE_MENU_RES);
        fileMenu = (MenuHandle)GetMenuHandle (FILE_MENU_RES);
        editMenu = (MenuHandle)GetMenuHandle (EDIT_MENU_RES);
        controlMenu = (MenuHandle)GetMenuHandle (CONTROL_MENU_RES);
        AppendResMenu (appleMenu, 'DRVR');
        DrawMenuBar ();
    }

    /* get the size of the main screen */
    screenWidth  = qd.screenBits.bounds.right  - qd.screenBits.bounds.left;
    screenHeight = qd.screenBits.bounds.bottom - qd.screenBits.bounds.top;

    /* compute the size of the graphics window in split-screen mode */
    gHorizontal = SCREEN_MARGIN;
    gVertical = MBAR_HEIGHT + TITLEBAR_HEIGHT - 1;
    gWidth = screenWidth - (SCREEN_MARGIN * 2);
    gHeight = GRAFWIN_HEIGHT;

    /* compute the size of the command window in split-screen mode */
    sHorizontal = SCREEN_MARGIN;
    sVertical = MBAR_HEIGHT + TITLEBAR_HEIGHT - 1 + SCREEN_MARGIN + GRAFWIN_HEIGHT;
    sWidth = screenWidth - (SCREEN_MARGIN * 2);
    sHeight = screenHeight - MBAR_HEIGHT - TITLEBAR_HEIGHT - (SCREEN_MARGIN * 2) - GRAFWIN_HEIGHT - 1;
    
    /* set up size and drag rects */
    dragRect = (*GetGrayRgn ())->rgnBBox;
//	dragRect.left += DRAG_THRESHOLD;
//	dragRect.right -= DRAG_THRESHOLD;
//	dragRect.bottom -= DRAG_THRESHOLD;
    sizeRect.top = MIN_WIN_HEIGHT;
    sizeRect.left = MIN_WIN_WIDTH;
    sizeRect.bottom = qd.screenBits.bounds.bottom - qd.screenBits.bounds.top;
    sizeRect.right = qd.screenBits.bounds.right - qd.screenBits.bounds.left;
    
    /* create the command window */
    gCommandWin = GetNewWindow (CWINRES, &commandWinRec, (WindowPtr) -1L);
    SetPort (gCommandWin);

    /* create the graphics window */
    if (hasColorQD) gGraphicsWin = GetNewCWindow (GWINRES, &colorGraphicsWinRec, (WindowPtr) -1L);
    else gGraphicsWin = GetNewWindow (GWINRES, &bwGraphicsWinRec, (WindowPtr) -1L);

    startupTicks = TickCount ();  /* take note of what time we're starting up */
    
    // Create mouse regin
    gMouseRgn = NewRgn();
    
    // Initalize some command window stuff
    InitalizeCmdWindow();
    
    // Turn on text outlineing
    TEFeatureFlag(teFOutlineHilite, teBitSet, hTERec);
    
    HideGrafWin ();

    {  /* see if the user launched the app by opening text files from the Finder */
        short doWhat;\
// call to CountAppFiles was commented out, but that left doWhat uninitialized
// RBD added this ifdef, I wonder where CountAppFiles came from?
#ifdef CountAppFilesDefined
        CountAppFiles (&doWhat, &howManyFiles);
        if (doWhat != appOpen) howManyFiles = 0;
#else
        howManyFiles = 0;
#endif
    }

    UpdateCmdWindow ();

}



static void DoAppleMenu (int theItem) {
    switch (theItem) {
        case ABOUT_ITEM:
            DoAboutBox ();
            break;
        default: {
            Str255 name;
            GetMenuItemText (appleMenu, theItem, name);
            OpenDeskAcc (name);
            break;
        }
    }
}
/* this should really be in a header for MacFileUtils.c */
void GetFullPath(FSSpec *theSpec, StringPtr theName);


static void DoFileMenu (int theItem) {
    extern xlload (char *, int, int);
    extern xlabort(char *);
    extern xlisp_wrapup (void);
    StandardFileReply theFile;

    SFTypeList fileTypes;
    Point pt = { 100, 100 };

    fileTypes[0] = 'TEXT';
    switch (theItem) {
        case LOAD:
        case LOAD_NOISILY:
            StopPasting ();
            StandardGetFile(NIL, 1, fileTypes, &theFile);
            if (theFile.sfGood) {
                Str255 theFullPath;
				short wdRefNum;
				
				OSErr err;
				HiliteMenu (0);
				
                err = OpenWD(theFile.sfFile.vRefNum, theFile.sfFile.parID, 'Nyqu', &wdRefNum);
                err = SetVol(NIL, wdRefNum);
				SetSelection (TEXTREC->teLength, TEXTREC->teLength);  /* send cursor to end */

                GetFullPath(&theFile.sfFile, theFullPath);
				P2CStr(theFullPath);

				if ((xlload((char *) theFullPath, 1, (theItem == LOAD_NOISILY))) == 0) {
					xlabort("load error");
				}
				macputs ("> ");
				PrepareForInput ();
            }
            break;
        case QUIT:
            xlisp_wrapup ();
    }
}

static void DoEditMenu (int theItem) {
    if (SystemEdit (theItem-1) == false)
        switch (theItem) {
        case CUT: case COPY:
            if (ZeroScrap () == noErr) {
                TECopy (hTERec);  /* after copying, export the TE scrap */
                if (TEToScrap () != noErr) ZeroScrap ();
            }
            if (theItem == CUT) DeleteRange ();
            break;
        case PASTE: {
            long scrapOffset;
            if (pastedTextH) DisposeHandle (pastedTextH);
            pastedTextH = (CharsHandle) NewHandle (0);
            pastedLength = GetScrap (pastedTextH, 'TEXT', &scrapOffset);
            if (pastedLength < 0) pastedLength = 0;  /* error */
            else {
                SetHandleSize (pastedTextH, pastedLength + 1);
                HLock (pastedTextH);
                ((char *)(*pastedTextH))[pastedLength] = '\0';
                HUnlock (pastedTextH);
                }
            }  /* and fall through ... */
        case CLEAR:
            DeleteRange ();
            break;
        }
}

static void DoControlMenu (int theItem) {
    extern xlbreak (char *, char *);
    extern char *s_unbound;
    extern xlcontinue (void);
    extern xlcleanup (void);
    extern xlabort (char *);
    extern xltoplevel (void);

    scrflush ();
    HiliteMenu (0);
    switch (theItem) {
        case BREAK:			StopPasting ();	xlbreak ("user break", s_unbound);	PrepareForInput ();	break;
        case CONTINUE:		StopPasting ();	xlcontinue ();						PrepareForInput ();	break;
        case CLEAN_UP:		StopPasting ();	xlcleanup ();						PrepareForInput ();	break;
        case CANCEL_INPUT:	StopPasting ();	xlabort ("input canceled");			PrepareForInput ();	break;
        case TOP_LEVEL:		StopPasting ();	xltoplevel ();						PrepareForInput ();	break;
        case SHOW_GRAPHICS:
            if (gGraphicsShown) HideGrafWin ();
            else ShowGrafWin ();
            break;
        case SPLIT_SCREEN:
            MoveWindow (gCommandWin, sHorizontal, sVertical, -1);
            SizeWindow (gCommandWin, sWidth, sHeight, -1);
            InvalRect (&gCommandWin->portRect);
            SetTERect ();
            SetScrollRect ();
            ShowGrafWin ();
            MoveWindow (gGraphicsWin, gHorizontal, gVertical, -1);
            SizeWindow (gGraphicsWin, gWidth, gHeight, -1);
            break;
    }
}

void DoMenu (long choice) {
    int theMenu = HiWord (choice), theItem = LoWord (choice);

    HiliteMenu (theMenu);
    switch (theMenu) {
        case APPLE_MENU_RES:	DoAppleMenu (theItem);		break;
        case FILE_MENU_RES:		DoFileMenu (theItem);		break;
        case EDIT_MENU_RES:		DoEditMenu (theItem);		break;
        case CONTROL_MENU_RES:	DoControlMenu (theItem);	break;
    }
    HiliteMenu (0);
}

void AdjustMenus (void) {
    /* turn the stuff in the Edit menu on and off as necessary */
    long temp;
    DisableItem (editMenu, UNDO);
    if (TEXTREC->selStart != TEXTREC->selEnd) {
        EnableItem (editMenu, CUT);
        EnableItem (editMenu, COPY);
        EnableItem (editMenu, CLEAR);
    } else {
        DisableItem (editMenu, CUT);
        DisableItem (editMenu, COPY);
        DisableItem (editMenu, CLEAR);
    }
    if (GetScrap (NIL, 'TEXT', &temp) > 0) EnableItem (editMenu, PASTE);
    else DisableItem (editMenu, PASTE);
}

RgnHandle rgn = nil;

void DoContent (EventRecord *theEvent) {
    /* handle a click in a window's content region */
    ControlHandle theScrollBar;
    GrafPtr oldPort;
    int scrollValue;
    Point mouse = theEvent->where;
    int thePart;
//	RgnHandle rgn = nil;
    
    GetPort (&oldPort);
    SetPort (gCommandWin);
    GlobalToLocal (&mouse);
    
    // Get Selected text
    rgn = NewRgn();
    TEGetHiliteRgn(rgn, hTERec);

    if (thePart = FindControl (mouse, gCommandWin, &theScrollBar)) {
        switch (thePart) {
            case kControlUpButtonPart:
            case kControlDownButtonPart:
            case kControlPageUpPart:
            case kControlPageDownPart:
                scrollValue = TrackControl (theScrollBar, mouse, uppScrollProc);
                break;
            case kControlIndicatorPart:
                scrollValue = GetControlValue (theScrollBar);
                thePart = TrackControl (theScrollBar, mouse, NIL);
                if (thePart) {
                    scrollValue -= GetControlValue (theScrollBar);
                    if (scrollValue) TEScroll (0, scrollValue * LINEHEIGHT, hTERec);
                }
                break;
        }
    } else if (PtInRgn(mouse, rgn)) {
        if (!DragText(theEvent)) {
            TEClick(mouse, false, hTERec);
        }
    } else if (PtInRect (mouse, &(TEXTREC->viewRect))) {
        TEClick (mouse, (theEvent->modifiers & shiftKey) != 0, hTERec);
    }
    SetPort (oldPort);
    DisposeRgn(rgn);
}


void DoEvent (void) {
    EventRecord theEvent;
    
    if ((flashTime) && (--flashTime == 0)) SetSelection (cursorBeforeFlash, cursorBeforeFlash);
    if (outputBufferLength) FlushOutput ();
    if (FrontWindow () == gCommandWin) TEIdle (hTERec);
    recentChar = '\0';
    
    if (WaitNextEvent (everyEvent, &theEvent, 0, gMouseRgn)) {

        AdjustCursor (theEvent.where, gMouseRgn);	
    
        switch (theEvent.what) {
            case kHighLevelEvent:
                AEProcessAppleEvent(&theEvent);			
                break;
            case mouseDown:
                DoMouseDown (&theEvent);
                break;
            case keyDown:
            case autoKey:
                DoKeyPress (&theEvent);
                break;
            case activateEvt: {
                WindowPtr whichWindow = (WindowPtr)theEvent.message;
                SetPort (whichWindow);
                if (whichWindow == gCommandWin) {
                    if ((theEvent.modifiers & activeFlag) == 1) {
                        ActivateCmdWindow();
                    } else {
                        DeactivateCmdWindow();
                    }
                }
                break;
            }
            case updateEvt: {
                if ((WindowPtr)theEvent.message == gCommandWin) UpdateCmdWindow ();
                if ((WindowPtr)theEvent.message == gGraphicsWin) UpdateGraphWindow ();
                break;
            }
            case osEvt:
                if (((theEvent.message >> 24) & 0xff) == suspendResumeMessage) {
                    if (theEvent.message & resumeFlag) {
                        gInBackground = false;
                        if (FrontWindow () == gCommandWin) {
                            ActivateCmdWindow();
                        }
                    } else {
                        gInBackground = true;
                        if (FrontWindow () == gCommandWin) {
                            SetPort (gCommandWin);
                            DeactivateCmdWindow();
                        }
                    }
                }
                break;
    
        }
    }
    AdjustCursor (theEvent.where, gMouseRgn);
}

void MacWrapUp (void) {
    /* take everything down in preparation for quitting */
    CleanupCmdWindow();
    CloseWindow (gGraphicsWin);
}
