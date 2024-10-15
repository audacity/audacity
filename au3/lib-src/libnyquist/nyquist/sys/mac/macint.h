#define INT_MAX	+32767

#define INT_MIN	-32767

/* resource id's */

#define CWINRES				400

#define GWINRES				401

#define MBAR_RES			400

#define APPLE_MENU_RES		400

#define FILE_MENU_RES		401

#define EDIT_MENU_RES		402

#define CONTROL_MENU_RES	403

#define STRINGS_RES			400



/* Apple menu */

#define ABOUT_ITEM			1

#define ABOUT_BOX			400

#define	ABOUT_PICT			400



/* File menu */

#define LOAD				1

#define LOAD_NOISILY		2

#define QUIT				4



/* Edit menu */

#define UNDO				1

#define CUT					3

#define COPY				4

#define PASTE				5

#define CLEAR				6



/* Control menu */

#define BREAK				1

#define CONTINUE			2

#define CLEAN_UP			3

#define CANCEL_INPUT		4

#define TOP_LEVEL			5

#define SHOW_GRAPHICS		7

#define SPLIT_SCREEN		8



/* window sizing/dragging stuff */

#define DRAG_THRESHOLD 8

#define MIN_WIN_HEIGHT 80

#define MIN_WIN_WIDTH 120



#define MAX_BUF 250						/* max chars in output buffer */

#define SCROLLBACK_THRESHHOLD 30000 	/* max chars kept in window */

#define DELETE_BLOCK 10000  			/* how many chars to delete when threshhold reached */



#define LINEHEIGHT 11					/* height in pixels of 9-point Geneva, the font used */

#define STACKMIN 400000					/* amout of memory for application stack */

#define MASTERS 3						/* arbitrary -- how many times to call MoreMasters() */



/* key codes */

#define RETURN		0x0d

#define ENTER		0x03

#define DELETE		0x08

#define FWDDEL		0x7F

#define CLRKEY		0x1b

#define PAGEUP		0x0b

#define PAGEDN		0x0c

#define HOMEKEY		0x01

#define ENDKEY		0x04

#define HELPKEY		0x05

#define FNKEY		0x10

#define LEFTARROW	0x1c

#define RIGHTARROW	0x1d

#define UPARROW		0x1e

#define DOWNARROW	0x1f

#define DBLQUOTE	'\"'



/* useful definitions */

#define MBAR_HEIGHT		20

#define TITLEBAR_HEIGHT	20

#define SCROLLER_WIDTH	15

#define SCREEN_MARGIN	2

#define TEXT_MARGIN		4

#define GRAFWIN_HEIGHT	232

void AdjustMenus(void);

void DoMenu(long choice);

void HideGrafWin(void);

void DoContent(EventRecord *theEvent);

void InitMac(void);

void MacWrapUp(void);


void DoEvent (void);
