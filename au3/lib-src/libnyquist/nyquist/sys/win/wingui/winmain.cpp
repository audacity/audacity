#include "windows.h"            /* required for all Windows applications */
#include "cppext.h"
#include "longque.h"
#include "button.h"
#include "slider.h"
#include "winmain.h"            /* specific to this program          */
#include "stdlib.h"
#include "string.h"
#include "stdio.h"
#include "textio.h"
#include "mmsystem.h"
#include <crtdbg.h>
#include <mapiwin.h>        // for Sleep()
#include "stdio.h"
#include "resource.h"
#include "typein.h"
#include "xlispfns.h"
#include "winfun.h"

//#define THUMBTRACK        // tries to get continuous scrollbars with SB_THUMBTRACK messages.
                            // doesn't work -- zone 1 doesn't seem to wake up until the button-up.

//#include "saudio.h"

//#define D if(0) 

HWND buttons[NUMBUTTONS];
HWND sliders[NUMSLIDERS];

HWND textinput;
HWND textoutput;
HFONT hfont;

HWND alt_win;
WNDPROC alt_proc;


extern "C" {
    HINSTANCE hInst;            /* current instance */
    HWND hMainWindow;           /* main window handle */
}
/* HINSTANCE hInst;            /* current instance */
/* HWND hMainWindow;           /* main window handle */


int quit_value; // return value from WinMain
int abort_flag = 0;


// there's some state here:
// when we get a message from the input queue, it is an entire
// string. We save the string and a pointer to the next char
//
char *get_ascii_string = NULL;
char *next_ascii = NULL;

//asciiwait -- wait for ascii input
char wait_ascii()
{
    if (next_ascii && *next_ascii) {
        char c = *next_ascii++;
        if (c == '\r') c = '\n';
        _RPT1(_CRT_WARN, "|%c|", c);
        return c;
    }
    if (get_ascii_string) {
        _RPT2(_CRT_WARN, "free get_ascii_string %x %s\n", 
              get_ascii_string, get_ascii_string);
        free(get_ascii_string);
        get_ascii_string = NULL;
        next_ascii = NULL;
    }
    // no input, so look for Windows messages
    while (typein::queue.emptyp() && !abort_flag) {
        process_win_events(USE_GET);
    }
    if (abort_flag == ABORT_LEVEL) return ABORT_CHAR;
    if (abort_flag == BREAK_LEVEL) return BREAK_CHAR;
    get_ascii_string = (char *) typein::queue.remove();
    _RPT2(_CRT_WARN, "removed %x: %s\n", get_ascii_string, get_ascii_string);
    edit_append(get_ascii_string);
    next_ascii = get_ascii_string;
    return wait_ascii();
}


//process_win_events -- receive and handle windows by either:
//  USE_PEEK: non-blocking 
//  USE_GET: blocks
void process_win_events(int method)
{
    MSG msg;
    edit_append(""); // flush the output
    if (method == USE_GET) {
        //blocks until at least a message arrives
        if (GetMessage(&msg,    //msg stored here
                NULL,           //receive ALL application messages
                NULL,NULL))     //no msg filtering
        {
            //standard windows loop
            TranslateMessage(&msg); //posts another msg if there is a virtual to WM_CHAR mapping
            DispatchMessage(&msg);  //calls object's receive function
        } else {
            //is this ever entered???
            quit_value = msg.wParam;
            abort_flag = ABORT_CHAR;
            exit(0);
        }
    } else {
        //default: process all messges that already exist (non-blocking)
        while (PeekMessage(&msg, NULL, NULL, NULL, PM_REMOVE | PM_NOYIELD)) 
        {
            TranslateMessage(&msg);    
            DispatchMessage(&msg);     
        }
    }
}

//STUFF TO PROVIDE CONSOLE-------------------------------------------------
//terminate strings and strip out LF
int lf2crlf(char *dest, char *src)
{
    char *d = dest;
    while (*src) {
        if (*src == '\n') {
            *dest++ = '\r';
        }
        *dest++ = *src++;
    }
    *dest = EOS;
    return dest - d; /* string length */
}

#define EDIT_HIGH_WATER 10000
#define EDIT_LOW_WATER 9000

#define EABUFFMAX 110
static char eabuff[EABUFFMAX]; /* edit_append buffer */
static char eabuffx = 0;

static void edit_append2(char *txt2);

/* edit_append -- optimizes output by buffering 
 *
 * call with empty string to flush buffer 
 */
void edit_append(char *txt)
{
    /* new algorithm to deal with long strings on input:
     *  if input is longer than 50, insert a zero and 
     *  call recursively; then undo the zero and continue.
     */
    char txt2[100];
    while (strlen(txt) > 50) {
        char temp = txt[50];
        txt[50] = 0;
        edit_append(txt); /* strlen(txt) == 50 */
        txt = txt + 50;
        txt[0] = temp;
    }
    int len = lf2crlf(txt2, txt);
    if ((eabuffx + len + 1 > EABUFFMAX) || ((len == 0) && eabuffx)) {
        edit_append2(eabuff);
        eabuffx = 0;
    }
    strcpy(eabuff + eabuffx, txt2);
    eabuffx += len;
}


static void edit_append2(char *txt2)
{
    int len;
    int lines;
    if (*txt2 == '\b') { // special case: erase last character
        long len = SendMessage(textoutput, WM_GETTEXTLENGTH, (WPARAM) 0, (LPARAM) 0);
        if (len > 0) {
            // select the last character:
            SendMessage(textoutput, EM_SETSEL, (WPARAM) len - 1, (LPARAM) -1);
            // delete the last character:
            SendMessage(textoutput, EM_REPLACESEL, (WPARAM) 0, (LPARAM) ((LPSTR) ""));
        }
        return;
    }
    // to put insertion point at the end, first select
    // everything ...
    //wparam is UINT ; lparam is LONG
    SendMessage(textoutput, EM_SETSEL, (WPARAM) 0, (LPARAM) -1); 
    // then remove selection, leaving cursor at the end:
    SendMessage(textoutput, EM_SETSEL, (WPARAM) -1, (LPARAM) -1);
    // now, replacement actually appends to the buffer:
    SendMessage(textoutput, EM_REPLACESEL, (WPARAM) 0, (LPARAM) ((LPSTR) txt2));
    // if the number of characters exceeds EDIT_HIGH_WATER, then
    // trim the number of characters to EDIT_LOW_WATER by deleting
    // all lines up to the one containing total-EDIT_LOW_WATER
    lines = (int) SendMessage(textoutput, EM_GETLINECOUNT, (WPARAM) 0, (LPARAM) 0);
    len = (int) SendMessage(textoutput, EM_LINEINDEX, (WPARAM)(lines - 1), (LPARAM) 0);
    len += (int) SendMessage(textoutput, EM_LINELENGTH, (WPARAM)(lines - 1), (LPARAM) 0);
    if (len > EDIT_HIGH_WATER) {
        //these SendMessages operate to completion
        lines = (int) SendMessage(textoutput, EM_LINEFROMCHAR, 
                        (WPARAM)(len - EDIT_LOW_WATER), (LPARAM) 0);
        len = (int) SendMessage(textoutput, EM_LINEINDEX, (WPARAM)(lines), (LPARAM) 0);
        SendMessage(textoutput, EM_SETSEL, (WPARAM) 0, (LPARAM)(len));
        SendMessage(textoutput, EM_REPLACESEL, (WPARAM) 0, (LPARAM) ((LPSTR) ""));
    } 
}

//THE STUFF REQUIRED BY WINDOWS APPLICATIONS----------------------------------


// we will subclass the textinput window with a window procedure that detects
// the Enter key (hex 0x0d) and sets a flag. Then in the main window proc, 
// when an EN_CHANGE message is received, we will know the user typed Enter,
// so we can transfer the text to XLISP.
// 
// This would be a good place also to add editing characters to scroll through
// previous entries.
//
WNDPROC DefaultEditWndProc = NULL;
#define ENTER_KEY 0x0d
bool enter_flag = false;


long CALLBACK EditWndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    if (message == WM_CHAR) {
        if (wParam == ENTER_KEY) enter_flag = true;
        _RPT1(_CRT_WARN, "wm_char is %x\n", wParam);
    }
    return CallWindowProc(DefaultEditWndProc, hWnd, message, wParam, lParam);
}



//All applications need to register
BOOL InitApplication(HINSTANCE hInstance /* current instance */)
{
    WNDCLASS  wc;

    /* Fill in window class structure with parameters that describe the       */
    /* main window.                                                           */
    wc.style = NULL;                    /* Class style(s).                    */
    wc.lpfnWndProc = MainWndProc;       /* Function to retrieve messages for  */
                                        /* windows of this class.             */
    wc.cbClsExtra = 0;                  /* No per-class extra data.           */
    wc.cbWndExtra = 0;                  /* No per-window extra data.          */
    wc.hInstance = hInstance;           /* Application that owns the class.   */
    wc.hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_NYCON));
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground = (HBRUSH) GetStockObject(WHITE_BRUSH);
    wc.lpszMenuName =  "MAINMENU";   /* Name of menu resource in .RC file. */
    wc.lpszClassName = "CMTWClass"; /* Name used in call to CreateWindow. */

    /* Register the window class and return success/failure code. */
    return (RegisterClass(&wc));

}


static char *button_names[NUMBUTTONS] = { "Load File", "Reload File", 
    "Replay Sound", "Break", "Top", "Up", "Info", "F1", "F2", "F3", "F4" };


//each time this application is run, the following program must be run
BOOL InitInstance(
    HINSTANCE       hInstance,          /* Current instance identifier.       */
    int             nCmdShow)           /* Param for first ShowWindow() call. */
{
    int i;
    RECT rect;
    LRESULT rslt;

    /* Save the instance handle in static variable, which will be used in  */
    /* many subsequent calls from this application to Windows.            */

    hInst = hInstance;

    /* Create a main window for this application instance.  */

    hMainWindow = CreateWindow(
        "CMTWClass",                    /* See RegisterClass() call.          */
        "Nyquist",                      /* Text for window title bar.         */
        WS_OVERLAPPEDWINDOW,            /* Window style.                      */
        CW_USEDEFAULT,                  /* Default horizontal position.       */
        CW_USEDEFAULT,                  /* Default vertical position.         */
        CW_USEDEFAULT,                  /* Default width.                     */
        CW_USEDEFAULT,                  /* Default height.                    */
        NULL,                           /* Overlapped windows have no parent. */
        NULL,                           /* Use the window class menu.         */
        hInstance,                      /* This instance owns this window.    */
        NULL                            /* Pointer not needed.                */
    );

    /* If window could not be created, return "failure" */

    if (!hMainWindow)
        return (FALSE);
        
    GetClientRect(hMainWindow, (LPRECT) &rect);

    /* Make the window visible; update its client area; and return "success" */

    const int button_y = 25;        // size in pixels
    const int slider_x = 20;
    const int slider_gap = 1;
    const int input_height = 48;   // how high is input type-in edit control
    int slider_xsum = slider_x * NUMSLIDERS;
    int button_ysum = button_y * NUMBUTTONS_VERT;


    ShowWindow(hMainWindow, nCmdShow);  /* Show the window                        */
    UpdateWindow(hMainWindow);          /* Sends WM_PAINT message                 */
    for (i = 0; i < NUMBUTTONS; i++) {
        int x = 0;
        int y = i * button_y;
        int width = slider_xsum;
        if (i > 2) {
            y = (3  + (i - 3) / 2) * button_y;
            width = width / 2;
            if ((i & 1) == 0) x = width;
        }
        buttons[i] = CreateWindow("Button",         //lpszClassName
                                    button_names[i],//windowName
                                    BS_PUSHBUTTON | //Style: 1) PB
                                        WS_CHILD |  //  2) must reside w/in parent
                                        WS_VISIBLE, //  3) initially visible
                                    x, y, width, button_y,
                                    hMainWindow,    //owner window
                                    (HMENU)(IDC_BUTTON+i),
                                    //&buttonsH[i], //child window Id
                                    hInstance,      //application instance 
                                    NULL);          //WM_CREATE argument

        //activate current window & display w/current size and position
        ShowWindow(buttons[i], SW_SHOW);
        //update (nonempty) client area via WM_PAINT
        UpdateWindow(buttons[i]);
    }

    for (i=0; i<NUMSLIDERS; ++i)  {
        char name[5];
        sprintf(name, "%d", i);
        sliders[i] = CreateWindow("Scrollbar", name, WS_CHILD | WS_VISIBLE | SBS_VERT /*| WS_BORDER */,
                                  slider_x * i + slider_gap, button_ysum, slider_x - slider_gap, (rect.bottom - rect.top) - button_ysum, 
                                  hMainWindow, (HMENU)(IDC_SLIDER+i), hInstance, NULL);     // (was IDC_BUTTON)
        SetScrollRange(sliders[i], SB_CTL, 0, 127, 1);
        ShowWindow(sliders[i], SW_SHOW);
        UpdateWindow(sliders[i]); 
    }
    


    textinput = CreateWindow("Edit", NULL,
                          WS_CHILD | WS_VISIBLE | ES_MULTILINE | 
                            WS_VSCROLL | ES_AUTOVSCROLL | WS_BORDER,
                          slider_xsum, 0, 
                          (rect.right - rect.left) - slider_xsum, 
                          input_height,
                          hMainWindow, (HMENU)(IDC_EDIT_INPUT), hInstance, NULL);
    // subclass the input window so we can catch the Enter key
    DefaultEditWndProc = (WNDPROC) SetWindowLong(textinput, GWL_WNDPROC, (long) EditWndProc);

    textoutput = CreateWindow("Edit", NULL,
                          WS_CHILD | WS_VISIBLE | ES_MULTILINE | ES_READONLY |
                            WS_VSCROLL | ES_AUTOVSCROLL | WS_BORDER,
                          slider_xsum, input_height - 1, 
                          (rect.right - rect.left) - slider_xsum, 
                          (rect.bottom - rect.top) - input_height,
                          hMainWindow, (HMENU)(IDC_EDIT), hInstance, NULL);

    hfont = CreateFont(0, 0, 0, 0, FW_DONTCARE, FALSE, FALSE, FALSE,
                       ANSI_CHARSET, OUT_RASTER_PRECIS, CLIP_DEFAULT_PRECIS,
                       DEFAULT_QUALITY, FIXED_PITCH | FF_DONTCARE,
                       "Courier");


    rslt = SendMessage(textoutput, WM_SETFONT, (WPARAM) hfont,
                               MAKELPARAM(TRUE, 0));
 
    ShowWindow(textoutput, SW_SHOW);
    UpdateWindow(textoutput); 

    rslt = SendMessage(textinput, WM_SETFONT, (WPARAM) hfont,
                               MAKELPARAM(TRUE, 0));
 
    ShowWindow(textinput, SW_SHOW);
    UpdateWindow(textinput); 

    return (TRUE);               /* Returns the value from PostQuitMessage */

}

//this is the hook called where any windows application starts up
extern "C" int WINAPI WinMain(
  HINSTANCE hInstance,              /* current instance         */
  HINSTANCE hPrevInstance,          /* previous instance        */
  LPSTR lpCmdLine,                 /* command line             */
  int nCmdShow)                    /* show-window type (open/icon) */
{
    _CrtSetReportMode(_CRT_WARN, _CRTDBG_MODE_DEBUG);
    _CrtSetReportMode(_CRT_ERROR, _CRTDBG_MODE_DEBUG | _CRTDBG_MODE_WNDW);      // EUB

#if defined(_DEBUG) && 0
    int flags = _CrtSetDbgFlag(_CRTDBG_REPORT_FLAG);
    flags |= _CRTDBG_CHECK_ALWAYS_DF;       // do expensive memory checking     -eub
    flags |= _CRTDBG_LEAK_CHECK_DF;         // check for leaks at termination
    _CrtSetDbgFlag(flags);
    _CrtCheckMemory();
#endif

    typein::init();
    
    if (!hPrevInstance)          /* Other instances of app running? */
    if (!InitApplication(hInstance)) /* Initialize shared things */
        return (FALSE);      /* Exits if unable to initialize     */

    /* Perform initializations that apply to a specific instance */

    if (!InitInstance(hInstance, nCmdShow)) return (FALSE);

    run_xlisp();

    return quit_value;
}


char *button_msgs[] = {
    "(ny:load-file)\n",
    "(ny:reload-file)\n",
    "(r)\n",
    "\002",
    "\003",
    "\007",
    "\024",
    "(F1)\n",
    "(F2)\n",
    "(F3)\n",
    "(F4)\n" };


static void type_this(char *ptr)
{
    char *s = (char *) malloc(strlen(ptr) + 1);
    _RPT1(_CRT_WARN, "type_this mallocs %x\n", s);
    strcpy(s, ptr);
    typein::handler(s);
}



//This is the Main Window Receive Function
long CALLBACK MainWndProc(
  HWND hWnd,                      /* window handle */
  UINT message,                   /* type of message */
  WPARAM wParam,                  /* additional information */
  LPARAM lParam)                  /* additional information */
{
   switch (message) {
        case WM_CHAR: {
            //message: character code is posted via Translate Message
            TCHAR c = (TCHAR) wParam;
            //lKeyData = lParam;    extended, ctrl, etc keys are ignored!!!
            // typein::handler(c);
            break;
        }
        case WM_COMMAND: {      
            //message: command from application menu
            short int code = HIWORD(wParam);
            int wid = LOWORD(wParam);
            HWND hwndCtrl = (HWND) lParam;
            if (code == 0 && wid == IDM_ABOUT) {
                DialogBox(hInst, /*current inst*/ "ABOUTBOX", /*resource to use*/
                    hWnd, /* parent*/ (DLGPROC) About /*About() instance address*/);
            } else if (code == 0 && wid == ID_FILE_LOAD) {
                type_this(button_msgs[0]);
            } else if (code == 0 && wid == ID_FILE_RELOAD) {
                type_this(button_msgs[1]);
            } else if (code == 0 && wid == ID_FILE_EXIT) {
                type_this("(exit)\n");
            } else if (wid >= IDC_BUTTON && wid < IDC_BUTTON + NUMBUTTONS) {
                SetFocus(textinput); /* get focus back */
                type_this(button_msgs[wid - IDC_BUTTON]);
            } else if (wid == IDC_EDIT_INPUT) {
                if (code == EN_CHANGE) {
                    if (enter_flag) {
                        enter_flag = false;
                        long len = SendMessage(hwndCtrl, WM_GETTEXTLENGTH, 0, 0);
                        len++; // allow for terminating null character
                        char *buffer = (char *) malloc(len);
                        SendMessage(hwndCtrl, WM_GETTEXT, (WPARAM) len, (LPARAM) buffer);
                        _RPT2(_CRT_WARN, "inserting %x: %s\n", buffer, buffer);
                        typein::handler(buffer);
                        SendMessage(hwndCtrl, WM_SETTEXT, (WPARAM) 0, (LPARAM) "");
                    }
                }
            } else if (wid == IDC_EDIT) { 
                if (code == EN_CHANGE) {
                } else {
                }
            } else {
                /*Let Windows process it*/
                printf("unhandled: message %d\n", message);
                return (DefWindowProc(hWnd, message, wParam, lParam));
            }
            break;
        }
        case WM_DESTROY:          
            //message: window being destroyed 
            PostQuitMessage(0);
            break;
#ifdef MOUSEBUTTON_INPUT
        case WM_LBUTTONDOWN: {
            //message: left mouse button pressed while cursor in client area
            //never used, need SetCaputure???
            int /*???*/ xPos LOWORD(lParam);
            int /*???*/ yPos HIWORD(lParam);
            //int /*???*/ fwKeys; virtual keys not used!!!
            buffer[0] = '<';
            _itoa(xPos,buffer+1,10/*radix*/);
            i = strlen(buffer);
            buffer[i++] = ',';
            _itoa(yPos,buffer+i,10/*radix*/);
            i = strlen(buffer);
            buffer[i++] = '>';
            buffer[i] = 0;
            edit_append(buffer);
            break;
#endif
#ifdef DEBUGINPUT
        //WM_USER msg codes are from the callbacks-------------------------
        case WM_USER_TIMEOUT: {
            //dummy message just unblocks GetMessage 
            break;
        }
        //remaining wMsg codes generated elsewhere------------------------------------
        case WM_KILLFOCUS: {
            //message: this window's loosing focus, focus must be set to new window
            HWND newFocus = (HWND) wParam;
            if (newFocus == textoutput) {
                //why not also set buttons, sliders...???
                SetFocus(hWnd);
            }
            break;
        }
        case WM_VSCROLL: {
            //message: control bar scroll I/O
            //where is window scroll bar handled???
            //why is this not also a WM_COMMAND???
            HWND hwndCtrl = (HWND) lParam;
            short int nPos = HIWORD(wParam);
            int code = LOWORD(wParam);
            int i;
            for (i = 0; i < NUMSLIDERS; i++) {
                if (sliders[i] == hwndCtrl) {
                    int pos = GetScrollPos(hwndCtrl /*parent*/, SB_CTL /*control*/);
                    switch (code) {
                      case SB_LINEUP:   pos--;      break;
                      case SB_LINEDOWN: pos++;      break;
                      case SB_PAGEUP:   pos -= 10;  break;
                      case SB_PAGEDOWN: pos += 10;  break;
                      case SB_ENDSCROLL:            
                          break;    //why not continue???
                      case SB_THUMBTRACK:       
#ifdef THUMBTRACK
                                        pos = nPos; 
                                                    break;
#else
                                                    continue;       // no silly multiple messages
#endif
                                                    //break;
                      case SB_THUMBPOSITION: 
#ifndef THUMBTRACK
                                        pos = nPos;
#endif
                                        break;
                      case SB_TOP:      pos = 0;    break;
                      case SB_BOTTOM:   pos = 127;  break;
                      default: continue;    //avoid SetScrollPos
                    }
                    // SetScrollRange() set the range to 0-127, but clip just to make sure:
                    if (pos < 0) pos = 0; 
                    if (pos > 127) pos = 127;
                    if (code != SB_ENDSCROLL
#ifdef THUMBTRACK
                        && code != SB_THUMBTRACK
#endif
                       ) {
                        //ctrlevents.insert(CTRLEVENT(IDC_SLIDER + i, 127 - pos));    
                    }
                    SetScrollPos(hwndCtrl /*parent*/, SB_CTL /*control sb*/, pos /*new position*/, TRUE /*redraw*/);
                    break;
                }
            }
            break;
        }
#endif
        default:                  
            //Pass on all unproccessed messages
            return (DefWindowProc(hWnd, message, wParam, lParam));
    }
    return (NULL);  //all messages currently return this...
}


/****************************************************************************

    FUNCTION: About(HWND, unsigned, WORD, LONG)

    PURPOSE:  Processes messages for "About" dialog box

    MESSAGES:

    WM_INITDIALOG - initialize dialog box
    WM_COMMAND    - Input received

    COMMENTS:

    No initialization is needed for this particular dialog box, but TRUE
    must be returned to Windows.

    Wait for user to click on "Ok" button, then close the dialog box.

****************************************************************************/

extern "C" BOOL CALLBACK About(
  HWND hDlg,               /* window handle of the dialog box */
  unsigned message,        /* type of message                 */
  WORD wParam,             /* message-specific information    */
  LONG lParam)
{
    switch (message)
    {
        case WM_INITDIALOG:            /* message: initialize dialog box */
            return (TRUE);

        case WM_COMMAND:               /* message: received a command */
            if (wParam == IDOK         /* "OK" box selected?          */
                || wParam == IDCANCEL) /* System menu close command?  */
            {
                EndDialog(hDlg, TRUE); /* Exits the dialog box        */
                return TRUE;
            }
            break;
    }
    return FALSE;               /* Didn't process a message    */
}


/* Load File support */

extern "C" 

void RegisterWindow(HWND w, WNDPROC p) {
    alt_win = w;
    alt_proc = p;
}
