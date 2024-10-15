//=============================================================================
// All command window updates, input, etc happen here -EAD
//=============================================================================
#include <Controls.h>
/* #include <ControlDefinitions.h> */
#include <Events.h>
#include <Fonts.h>
#include <MacWindows.h>
#include "MacGlobals.h"
#include "macint.h"
#include <ctype.h>
#define NIL ((void *) 0)

//=============================================================================
// local variables
//=============================================================================

ControlHandle vScroll;
int 		cursorPos;							/* the cursor's position on the line */
short		linesInView;						/* how many lines are in the window */
int			cmdStart;							/* where (in text record) the current command starts */
TextStyle 	textStyle[2];  						/* styles: bold for user input, plain for output */
/* output is buffered */
Handle hOutputBuffer = NULL;
enum currentStyle { plainStyle, boldStyle } currentStyle;

static void GoStartOfLine (void);
static void GoEndOfLine (void);
static void GoBackOneWord (void);
static void GoForwardOneWord (void);

//=============================================================================
// static void DoScrollBar (ControlHandle control, short change)
//=============================================================================
/* keep track of the user as he fiddles with the scroll bar */
/* This routine is called while the user has the mouse down. */
/* It makes sure the thumb isn't dragged out-of-bounds. */
//=============================================================================

static void DoScrollBar (ControlHandle control, short change) {
    short value = GetControlValue (control), max = GetControlMaximum (control);
    long newval = value + change;  /* this is a long in case we try to go past MAX_INT */
    if (newval < 0) newval = 0; else if (newval > max) newval = max;
    SetControlValue (control, (short) newval);
    if (newval != value) TEScroll (0, (short) (value - newval) * LINEHEIGHT, hTERec);
}

//=============================================================================
// pascal Boolean ScrollClickLoop (void) 
//=============================================================================
//
//=============================================================================

pascal Boolean ScrollClickLoop (void) {
    Rect tempRect;
    Point mouse;
    GrafPtr oldPort;
    RgnHandle oldClip;
    short amount = 0;

    if (FrontWindow () != gCommandWin) return false;

    GetPort (&oldPort);
    SetPort (gCommandWin);
    GetClip (oldClip = NewRgn ());
    SetRect (&tempRect, INT_MIN, INT_MIN, INT_MAX, INT_MAX);
    ClipRect (&tempRect);

    GetMouse (&mouse);
    if (mouse.v < TEXTREC->viewRect.top)  			DoScrollBar (vScroll, -1);
    else if (mouse.v > TEXTREC->viewRect.bottom)	DoScrollBar (vScroll, 1);

    SetClip (oldClip);
    DisposeRgn (oldClip);
    SetPort (oldPort);
    return true;
}

//=============================================================================
// static pascal void ScrollProc (ControlHandle control, short thePart)
//=============================================================================
// for clicks in the scroll bar or arrows; update the window properly
//=============================================================================

pascal void ScrollProc (ControlHandle control, short thePart) {
    short amount;
    WindowPtr window;

    if (!thePart) return;
    window = (*control)->contrlOwner;
    switch (thePart) {
        case kControlUpButtonPart:	amount = -1;				break;
        case kControlDownButtonPart:	amount = 1;					break;
        case kControlPageUpPart:		amount = -linesInView;		break;
        case kControlPageDownPart:	amount = linesInView;		break;
    }
    DoScrollBar (control, amount);
}

//=============================================================================
// Rect SetTERect (void)
//=============================================================================
// set the dimensions of the text record in its window 
//=============================================================================

Rect SetTERect (void) {
    Rect teRect = gCommandWin->portRect;
    teRect.right -= SCROLLER_WIDTH;
    InsetRect (&teRect, TEXT_MARGIN, TEXT_MARGIN);
    linesInView = (teRect.bottom - teRect.top) / LINEHEIGHT;
    teRect.bottom = teRect.top + linesInView * LINEHEIGHT;  /* round off */
    return teRect;
}

//=============================================================================
// static void AdjustCursor (EventRecord *theEvent)
//=============================================================================
// make the pointer an I-beam iff it's in the text window
//=============================================================================

void AdjustCursor (Point theLoc, RgnHandle theRgn) 
{
    RgnHandle	arrowRgn, iBeamRgn, hiliteRgn, tempRgn;
    Rect		theRect;
    Point		thePoint;
   
    if (gInBackground)
        return;

    arrowRgn = NewRgn();
    SetRectRgn(arrowRgn, -32767, -32767, 32767, 32767);

//	GlobalToLocal ((theLoc); ???
    
    if (gCommandWin == FrontWindow () ) {
        SetPort(gCommandWin);
        iBeamRgn = NewRgn();
        hiliteRgn = NewRgn();
        
        theRect = TEXTREC->viewRect;
        LocalToGlobal((Point *)&(theRect.top));
        LocalToGlobal((Point *)&(theRect.bottom));
        RectRgn(iBeamRgn, &theRect);
        
        TEGetHiliteRgn(hiliteRgn, hTERec);
        thePoint.h = thePoint.v = 0;
        LocalToGlobal(&thePoint);
        OffsetRgn(hiliteRgn, thePoint.h, thePoint.v);

        DiffRgn(arrowRgn, hiliteRgn, arrowRgn);
        DiffRgn(arrowRgn, iBeamRgn, arrowRgn);

        DiffRgn(iBeamRgn, hiliteRgn, iBeamRgn);
        
        if (PtInRgn(theLoc, iBeamRgn)) {
            SetCursor(*GetCursor(iBeamCursor));
            CopyRgn(iBeamRgn, theRgn);
        } else if (PtInRgn(theLoc, hiliteRgn)) {
            SetCursor(&qd.arrow);
            CopyRgn(hiliteRgn, theRgn);
        } else {
            SetCursor(&qd.arrow);
            CopyRgn(arrowRgn, theRgn);
        }

        DisposeRgn(iBeamRgn);
        DisposeRgn(hiliteRgn);

    } else  {
        SetCursor(&qd.arrow);
        CopyRgn(arrowRgn, theRgn);
    }

    DisposeRgn(arrowRgn);
}



//=============================================================================
// static void SetScrollRect (void)
//=============================================================================
// Set Scroll bar rec size
//=============================================================================

void SetScrollRect (void) {
    /* set the dimensions of the scroll bar in its window */

// This change fixes the double flash on window resize -EAD

//	MoveControl (vScroll, commandWin->portRect.right - SCROLLER_WIDTH, -1);
//	SizeControl (vScroll, SCROLLER_WIDTH + 1,
//			(commandWin->portRect.bottom - commandWin->portRect.top) - (SCROLLER_WIDTH - 2));
    (*vScroll)->contrlRect.left = gCommandWin->portRect.right - SCROLLER_WIDTH;
    (*vScroll)->contrlRect.top = -1;
    (*vScroll)->contrlRect.right =  gCommandWin->portRect.right + 1;
    (*vScroll)->contrlRect.bottom = gCommandWin->portRect.bottom - (SCROLLER_WIDTH - 1);
    
}

//=============================================================================
// static void AdjustScrollBar (void)
//=============================================================================
// Set the thumb on scrollbar
//=============================================================================

static void AdjustScrollBar (void) {
    /* adjust the scroll bar to match the position of the text view */
    short oldval = GetControlValue (vScroll), oldmax = GetControlMaximum (vScroll);
    short value, max;
    short test;
    
    max = TEXTREC->nLines - linesInView;
    if ((TEXTREC->teLength > 0) && (*(*TEXTREC->hText + TEXTREC->teLength - 1) == '\r')) max++;
    if (max < 0) max = 0;
    if (max != oldmax) SetControlMaximum (vScroll, max);
    value = (short)((TEXTREC->viewRect.top - TEXTREC->destRect.top) / LINEHEIGHT);
//	value = roundup ((TEXTREC->viewRect.top - TEXTREC->destRect.top) / LINEHEIGHT);
    if (value < 0) value = 0; else if (value > max) value = max;
    if (value != oldval) SetControlValue (vScroll, value);
}

static short roundup (float x) {  /* a kludge to round up a float to an int */
    if (((int) x) != ((int) (x += 0.5))) x += 0.5;
    return (int) x;
}

//=============================================================================
// void DoKeyPress (EventRecord *theEvent)
//=============================================================================
// Hanlde Keyboard Input
//=============================================================================

void DoKeyPress (EventRecord *theEvent) {
    short whatKey = theEvent->message & charCodeMask;
    if (theEvent->modifiers & cmdKey) {
        long choice;
        AdjustMenus ();
        if (choice = MenuKey (theEvent->message)) DoMenu (choice);
        else if (((whatKey == 'w') || (whatKey == 'W')) && (FrontWindow () == gGraphicsWin))
            HideGrafWin ();
        else if (whatKey == LEFTARROW)	GoStartOfLine ();
        else if (whatKey == RIGHTARROW)	GoEndOfLine ();
        else if (whatKey == UPARROW)	DoScrollBar (vScroll, - linesInView);
        else if (whatKey == DOWNARROW)	DoScrollBar (vScroll, linesInView);
    }
    else if (theEvent->modifiers & optionKey) {
        if (whatKey == LEFTARROW)		GoBackOneWord ();
        else if (whatKey == RIGHTARROW)	GoForwardOneWord ();
    }
    else switch (whatKey) {
        case PAGEUP:	DoScrollBar (vScroll, -linesInView);	break;
        case PAGEDN:	DoScrollBar (vScroll, linesInView);		break;
        case HOMEKEY:	DoScrollBar (vScroll, INT_MIN);			break;
        case ENDKEY:	DoScrollBar (vScroll, INT_MAX);			break;
        case FNKEY:												break;
        case HELPKEY:											break;
        default: recentChar = theEvent->message & charCodeMask;
    }
}

//=============================================================================
// static void DrawOnlyGrowIcon (WindowPtr window) 
//=============================================================================
// draw growbox on command window with no scoll bars
//=============================================================================

static void DrawOnlyGrowIcon (WindowPtr window) 
{  
    RgnHandle saveRgn;
    Rect growRect;

    growRect = window->portRect;
    growRect.top = growRect.bottom - SCROLLER_WIDTH;
    growRect.left = growRect.right - SCROLLER_WIDTH;
    GetClip (saveRgn = NewRgn ());
    ClipRect (&growRect);
    DrawGrowIcon (window);
    SetClip (saveRgn);
    DisposeRgn (saveRgn);
}

//=============================================================================
// void SetSelection (short start, short end)
//=============================================================================
// set text selection in the command window
//=============================================================================

void SetSelection (short start, short end) {
    TEXTREC->clikStuff = 255;  /* to make sure the caret appears at the start of a line when it should */
    /* see tech note "TextEdit EOL Ambiguity" for more information */
    TESetSelect (start, end, hTERec);
}

//=============================================================================
// static void CancelFlash (void)
//=============================================================================
// cancel the matching-paren flashing
//=============================================================================

static void CancelFlash (void) {
    if (flashTime) {
        flashTime = 0;
        SetSelection (cursorBeforeFlash, cursorBeforeFlash);
    }
}

//=============================================================================
// static void StopPasting (void)
//=============================================================================
// clean up after finishing a paste
//=============================================================================

void StopPasting (void) {
    pastedLength = 0;
    if (pastedTextH) {
        DisposeHandle (pastedTextH);
        pastedTextH = NULL;
    }
}

//=============================================================================
// static void DoStyle (int whatStyle)
//=============================================================================
// set the text to a certain style
//=============================================================================

static void DoStyle (int whatStyle) {
    TESetStyle (doFace, &(textStyle[whatStyle]), false, hTERec);
}

//=============================================================================
// static void FlushOutput (void)
//=============================================================================
// clear out the output buffer, dumping its contents to the window
//=============================================================================

void FlushOutput (void) {
    short totalLines, scrollAmount, max;

    if (outputBufferLength == 0) return;
    CancelFlash ();
    DoStyle (plainStyle);
    HLock (hOutputBuffer);
    TEInsert (*hOutputBuffer, outputBufferLength, hTERec);
    HUnlock (hOutputBuffer);
    outputBufferLength = 0;

    if (TEXTREC->teLength > SCROLLBACK_THRESHHOLD) {  
        /* make sure TE record isn't too long */
#ifdef ORIGINALCODE
        /* I replaced this because Nyquist was crashing after the
           buffer got filled. The replacement below is simpler and
           eliminates the crashes, although it probably could cause
           problems by clearing the selection.
         */
        int i = 1, newLength;
        TEPtr textPtr;
        while ((TEXTREC->teLength - TEXTREC->lineStarts[i]) > 
               (SCROLLBACK_THRESHHOLD - DELETE_BLOCK)) i++;
        i = TEXTREC->lineStarts[i];
        newLength = TEXTREC->teLength - i;
        textPtr = (TEPtr)(*(TEXTREC->hText));
        BlockMoveData ((Ptr)((long)textPtr + i), textPtr, newLength);
        SetHandleSize (TEXTREC->hText, newLength);
        TEXTREC->destRect.top += LINEHEIGHT;
        TECalText (hTERec);
        TEUpdate (&(TEXTREC->viewRect), hTERec);
#else
        /* find the line start after DELETE_BLOCK */
        int i = 1;
        while (TEXTREC->lineStarts[i] < DELETE_BLOCK) i++;
        TESetSelect(0, TEXTREC->lineStarts[i], hTERec);
        TEDelete(hTERec);
        /* after deletion, put cursor back at end of buffer */
        TESetSelect(TEXTREC->teLength, TEXTREC->teLength, hTERec);
#endif
    }
    TESelView (hTERec);
    AdjustScrollBar ();
}

//=============================================================================
// void PrepareForInput (void)
//=============================================================================
// get ready to take input
//=============================================================================

void PrepareForInput (void) {
    FlushOutput ();
    cmdStart = TEXTREC->selStart;
}

//=============================================================================
// static void DeleteRange (void)
//=============================================================================
// delete the selected range of text, updating cmdStart as necessary
//=============================================================================

void DeleteRange (void) {
    if (TEXTREC->selEnd <= cmdStart) return;
    if (TEXTREC->selStart < cmdStart) SetSelection (cmdStart, TEXTREC->selEnd);
    TEDelete (hTERec);
}

//=============================================================================
// static void CopyThisLineToEnd (void)
//=============================================================================
// copy the line the caret is on to the end
//=============================================================================

static void CopyThisLineToEnd (void) {
    char *buffer;
    short b, i, caretOffset;

    /* first find out exactly where it starts */
    i = TEXTREC->nLines-1;  /* first find which line */
    while (TEXTREC->selStart < TEXTREC->lineStarts[i]) i--;
    while ((i > 0) && ((*(TEXTREC->hText))[TEXTREC->lineStarts[i]-1] != '\r'))
        i--;  /* for wrapped lines */
    i = TEXTREC->lineStarts[i];  /* now zero in on the exact character where it begins */
    while ((TEXTCHAR(i) >= '0') && (TEXTCHAR(i) <= '9')) i++;  /* skip error level */
    if ((TEXTCHAR(i) == '>') && (TEXTCHAR(i+1) == ' ')) i+=2;  /* get rid of leading prompt */

    caretOffset = TEXTREC->selStart - i;  /* how many characters in is the caret? */

    /* now put the line into the buffer */
    b = 0;
    while ((TEXTCHAR(i+b) != '\r') && (i+b < TEXTREC->teLength)) b++;  /* find the end of the line */
    buffer = (char *) NewPtr (b);
    BlockMoveData (*TEXTREC->hText + i, buffer, b);
    buffer[b] = '\0';

    /* delete whatever's already on the last line */
    SetSelection (cmdStart, TEXTREC->teLength);
    TEDelete (hTERec);

    DoStyle (boldStyle);
    TEInsert (buffer, b, hTERec);
    DisposePtr (buffer);

    if (caretOffset < 0) caretOffset = b;
    SetSelection (cmdStart + caretOffset, cmdStart + caretOffset);
}

//=============================================================================
// Next four functions possition cursor in text
//=============================================================================

static void GoStartOfLine (void) {
    short whichLine = TEXTREC->nLines - 1;  /* look for the caret; start at the end and go up */
    while (TEXTREC->lineStarts[whichLine] > TEXTREC->selStart) whichLine--;
    SetSelection (TEXTREC->lineStarts[whichLine], TEXTREC->lineStarts[whichLine]);
    AdjustScrollBar ();
}

static void GoEndOfLine (void) {
    short whichLine = TEXTREC->nLines - 1;  /* look for the caret; start at the end and go up */
    while (TEXTREC->lineStarts[whichLine] > TEXTREC->selStart) whichLine--;
    if (whichLine == TEXTREC->nLines - 1)
        SetSelection (TEXTREC->teLength, TEXTREC->teLength);
    else SetSelection (TEXTREC->lineStarts[whichLine+1] - 1, TEXTREC->lineStarts[whichLine+1] - 1);
    AdjustScrollBar ();
}

static void GoBackOneWord (void) {
    short i = TEXTREC->selStart;
    while ((i > 0) && !isalnum (TEXTCHAR(i-1)))	i--;
    while ((i > 0) && isalnum (TEXTCHAR(i-1)))	i--;
    SetSelection (i, i);
}

static void GoForwardOneWord (void) {
    short i = TEXTREC->selStart;
    while ((i < TEXTREC->teLength) && !isalnum (TEXTCHAR(i)))	i++;
    while ((i < TEXTREC->teLength) && isalnum (TEXTCHAR(i)))	i++;
    SetSelection (i, i);
}


//=============================================================================
// static void EditFreely (void)
//=============================================================================
// Enter text into the command windows
//=============================================================================

static void EditFreely (void) {
    Boolean done;
    do {
        done = false;
        DoEvent ();
        if (pastedLength > 0) {  /* if there is still text to paste, paste it */
            int i = 0;
            CancelFlash ();
            if (TEXTREC->selStart < cmdStart) StopPasting ();
            else {
                while ((i < pastedLength) && (((char *)(*pastedTextH))[i] != '\r')) i++;
                DoStyle (boldStyle);
                TEInsert (*pastedTextH, i, hTERec);
                AdjustScrollBar ();
                if (i < pastedLength) {  /* we were stopped by a carriage return, so eat it */
                    i++;
                    done = true;
                }
                pastedLength -= i;
                if (pastedLength > 0) {
                    BlockMoveData ((Ptr)((long)(*pastedTextH) + i), *pastedTextH, pastedLength);
                    SetHandleSize (pastedTextH, pastedLength);
                } else StopPasting ();
            }
        }
        else if (recentChar) {  /* if the last event got us a character, process it */
            int i;
            Boolean wasOnLastLine;
            CancelFlash ();

            if ((TEXTREC->selEnd <= cmdStart) && (TEXTREC->selStart != TEXTREC->selEnd)) continue;
            if (TEXTREC->selStart < cmdStart) SetSelection (cmdStart, TEXTREC->selEnd);
            wasOnLastLine = (TEXTREC->selStart >= cmdStart);

            if ((recentChar & 0xfc) == 0x1c) {  /* was this an arrow key? */
                TEXTREC->clikStuff = 255;  /* to make sure the caret appears where it should */
                TEKey (recentChar, hTERec);
                AdjustScrollBar ();
                continue;
            }
            if (!wasOnLastLine) CopyThisLineToEnd ();
            switch (recentChar) {
            case FWDDEL:
                if (TEXTREC->selStart != TEXTREC->selEnd) DeleteRange ();
                else if ((TEXTREC->selStart >= cmdStart) && (TEXTREC->selStart < TEXTREC->teLength)) {
                    TEDeactivate (hTERec);
                    SetSelection (TEXTREC->selStart, TEXTREC->selStart + 1);
                    TEDelete (hTERec);
                    if (FrontWindow () == gCommandWin) TEActivate (hTERec);
                }
                break;
            case CLRKEY:
                if (TEXTREC->selStart != TEXTREC->selEnd) DeleteRange ();
                break;
            case DELETE:
                if (TEXTREC->selStart != TEXTREC->selEnd) DeleteRange ();
                else if (TEXTREC->selStart > cmdStart) {
                    TEXTREC->clikStuff = 255;  /* to make sure the caret appears where it should */
                    TEKey (DELETE, hTERec);
                }
                break;
            case RETURN:
                if (wasOnLastLine) done = true;
                break;
            case ENTER:  /* ENTER ends command no matter what */
                done = true;
                break;
            default:
                DoStyle (boldStyle);
                TEXTREC->clikStuff = 255;  /* to make sure the caret appears where it should */
                TEKey (recentChar, hTERec);
                if ((recentChar == ')') && (TEXTREC->selStart > cmdStart)) {
                    short parenCount = -1;
                    Boolean inQuotes = false;
                    i = TEXTREC->selStart - 1;
                    while ((--i >= cmdStart) && (parenCount != 0))
                        switch ((*TEXTREC->hText)[i]) {
                        case DBLQUOTE: inQuotes = !inQuotes; break;
                        case '(': if (!inQuotes) parenCount++; break;
                        case ')': if (!inQuotes) parenCount--; break;
                        }
                    if (parenCount == 0) {
                        cursorBeforeFlash = TEXTREC->selStart;
                        SetSelection (i+1, i+2);  /* flash the matching open-paren */
                        flashTime = 10;
                    }
                } else if ((recentChar == DBLQUOTE) && (TEXTREC->selStart > cmdStart)) {
                    i = TEXTREC->selStart - 1;
                    while ((--i >= cmdStart) && ((*TEXTREC->hText)[i] != DBLQUOTE)) ;
                    if ((*TEXTREC->hText)[i] == DBLQUOTE) {
                        cursorBeforeFlash = TEXTREC->selStart;
                        SetSelection (i, i+1);  /* flash the matching double-quote */
                        flashTime = 10;
                    }
                }
            }
            AdjustScrollBar ();
        }
    } while (!done);
}

char *macgets (void) {
    /* retrieve a typed character */
    /* Note that this uses some extensive (and clever, if I may say so myself) buffering. */
    int i, b, bufSize;
    char *ptr, *buffer;
    Boolean done, onLastLine;
    
    PrepareForInput ();
    do {  /* repeat until a full expression has been typed */
        EditFreely ();  /* allow free editing for a while */

        /* Now, we have a complete command to parse, if and only if: */
        /* - the cursor was on the last line when the user pressed Return or Enter, and */
        /* - the user either pressed Enter, or else every '(' since the beginning */
        /*     of the command is matched by a ')'. */
        /* Quoting is watched for.  ( ") is not a complete expression. */

        done = true;
        if (TEXTREC->selStart != TEXTREC->teLength)  /* if we're not at the end already */
            SetSelection (TEXTREC->teLength, TEXTREC->teLength);  /* send cursor to end */
        TEXTREC->clikStuff = 255;  /* to make sure the caret appears where it should */
        TEKey ('\r', hTERec);

        /* check and see if we've completed the command yet */
        if (recentChar != ENTER) {
            Boolean inQuotes = false;
            short parenCount = 0;
            for (i = cmdStart; i < TEXTREC->teLength; i++)
                switch ((*TEXTREC->hText)[i]) {
                case DBLQUOTE: inQuotes = !inQuotes; break;
                case '(': if (!inQuotes) parenCount++; break;
                case ')': if (!inQuotes) parenCount--; break;
                }
            if ((parenCount > 0) || inQuotes) done = false;
        }

        AdjustScrollBar ();
    } while (!done);

    /* put the entire command into the buffer, and return it */
    bufSize = TEXTREC->teLength - cmdStart;
    buffer = (char *) NewPtr (bufSize + 1);
    BlockMoveData (*TEXTREC->hText + cmdStart, buffer, bufSize);
    buffer[bufSize] = '\0';
    return buffer;
}

void macputc (int ch) {
    /* put a char into the output buffer, and flush the buffer if necessary */
    switch (ch) {
        case '\t':
            do { macputc (' '); } while (cursorPos & 7);
            break;
        case DELETE:
            if (cursorPos) cursorPos--;  /* and fall through to default */
        default:
            if (outputBufferLength == MAX_BUF) FlushOutput ();
            if (ch == '\n') {
                cursorPos = 0;
                (*hOutputBuffer)[outputBufferLength++] = '\r';
            } else {
                cursorPos++;
                (*hOutputBuffer)[outputBufferLength++] = ch;
            }
    }
}

void macputs (char *s) {
    /* for completeness */
    while (*s) macputc (*s++);
}

void scrflush (void) {
    extern void osflush (void);
    /* clear out everything */
    FlushOutput ();
    osflush ();
}

void scrclear (void) {
    /* clear text window -- not implemented */
}

//=============================================================================
// static void UpdateCmdWindow (void)
//=============================================================================
// main command window update procedure
//=============================================================================


void UpdateCmdWindow (void) {
    long textBottom;
    Rect tempRect;
    
    InvalRect (&(gCommandWin->portRect));
    BeginUpdate (gCommandWin);
    BlockMoveData(&(gCommandWin->portRect), &tempRect, sizeof(Rect));
    tempRect.right -= SCROLLER_WIDTH;
    EraseRect (&tempRect);
    if (gCommandWinResized) {
        TEXTREC->viewRect = SetTERect ();
        TEXTREC->destRect.right = TEXTREC->viewRect.right;
        TECalText (hTERec);
        SetScrollRect ();
        gCommandWinResized = false;
    }
    DrawOnlyGrowIcon (gCommandWin);
    FlushOutput ();

    TEXTREC->viewRect = SetTERect ();  /* adjust for possible change in height of status line */

    textBottom = TEXTREC->destRect.top + (TEXTREC->nLines * LINEHEIGHT);
    if (TEXTREC->destRect.top > TEXTREC->viewRect.top)
        TEScroll (0, (TEXTREC->viewRect.top - TEXTREC->destRect.top), hTERec);

    if (TEXTREC->destRect.top < TEXTREC->viewRect.top) {  /* make sure we don't get fractions of lineheights */
        int amountOffTheTop = TEXTREC->viewRect.top - TEXTREC->destRect.top;
        if (amountOffTheTop % LINEHEIGHT) TEScroll (0, amountOffTheTop % LINEHEIGHT, hTERec);
    }
    TEUpdate (&(TEXTREC->viewRect), hTERec);
    AdjustScrollBar ();
    UpdateControls (gCommandWin, gCommandWin->visRgn);
    EndUpdate (gCommandWin);
}

void ActivateCmdWindow(void)
{
    TEActivate (hTERec);
    HiliteControl (vScroll, 0);
    DrawOnlyGrowIcon (gCommandWin);			
}

void DeactivateCmdWindow(void)
{
    TEDeactivate (hTERec);
    HiliteControl (vScroll, 255);
    DrawOnlyGrowIcon (gCommandWin);
}

void InitalizeCmdWindow(void)
{

    /* setup the font, size and writing mode for the command window */
    TextFont (kFontIDMonaco);
    TextSize (9);
    TextFace (0);
    TextMode (srcCopy);
    textStyle[plainStyle].tsFace = 0;
    textStyle[boldStyle].tsFace = bold;

    currentStyle = plainStyle;

    {  /* set up scroll bar */
        Rect scrollRect;
        vScroll = NewControl (gCommandWin, &scrollRect, "\p", 0, 0, 0, 0, scrollBarProc, 0L);
        SetScrollRect ();
        ShowControl (vScroll);
    }

    {  /* set up command text record */
        Rect teRect = SetTERect ();
        hTERec = (TEHandle)TEStyleNew (&teRect, &teRect);
         TECalText (hTERec);
        TEAutoView (true, hTERec);
        TESetClickLoop (uppScrollClickLoop, hTERec);
        TEActivate (hTERec);
    }

    hOutputBuffer = NewHandle (MAX_BUF);  /* a handle to a buffer for text to be displayed */
}

void CleanupCmdWindow(void)
{
    StopPasting ();
    CloseWindow (gCommandWin);
    TEDispose (hTERec);
    DisposeHandle (hOutputBuffer);
}
