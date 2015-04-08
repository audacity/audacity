extern int quit_flag;
extern int waiting_flag;
extern long start_time;	// initial system time in ms
extern long the_time;	// current time - updated by periodic interrupt

extern HWND hMainWindow;	/* main window handle */

void edit_append(char *txt);
void debugeventwait();
bool get_mouse(int &x, int &y);
void wait_mouse(int &x, int &y);
void pause(long ms);

// parameters to process_win_events()
#define USE_GET 	0
#define USE_PEEK 	1

void process_win_events(int method);

#define CTRLEVENT(a, b) (((long) (a))<<16 | (b)) 

#define WM_USER_TIMEOUT (WM_USER + 0)
#define WM_USER_TIMESHOW (WM_USER + 1)
#define WM_USER_TIMESHOW1 (WM_USER + 2)

#define WM_USER_MIDI_INPUT (WM_USER + 10)
#define WM_USER_MIDISHOW (WM_USER + 11)
#define WM_USER_MIDISHOW1 (WM_USER + 12)

#define WM_USER_MIDI_IN_ERROR (WM_USER + 20)
#define WM_USER_MIDI_OUT_ERROR (WM_USER + 21)

#define TIMERCB		1
#define MIDIINCB	2
#define FROMTIMERCB(x) ((x) == TIMERCB)
#define FROMMIDIINCB(x) ((x) == MIDIINCB)

#define TEXT_WIN_HT 200



