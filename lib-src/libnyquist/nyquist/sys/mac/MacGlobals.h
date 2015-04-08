// Window pointers

extern WindowPtr gCommandWin, gGraphicsWin;

extern Boolean gCommandWinResized;



// Menu Handles

extern MenuHandle appleMenu, fileMenu, editMenu, controlMenu;



// The command window text handle

extern TEHandle hTERec;

#define TEXTREC		(*hTERec)  

#define TEXTCHAR(i)	((*(TEXTREC->hText))[i])



// more comand window text stuff

extern CharsHandle	pastedTextH;					/* a handle to pasted text */

extern int pastedLength;					/* how many chars there are in the paste buffer */

extern int outputBufferLength;

extern Rect dragRect, sizeRect;

extern int	flashTime, cursorBeforeFlash;

extern char	recentChar;							/* the last character typed */



// Allocate space for UPPs

extern ControlActionUPP uppScrollProc;	

extern TEClickLoopUPP uppScrollClickLoop;



extern Boolean gInBackground;

