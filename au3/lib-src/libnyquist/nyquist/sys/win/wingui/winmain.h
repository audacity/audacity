#include "resource.h"

#ifdef __cplusplus
extern "C" {
#endif 

BOOL InitApplication(HANDLE);
BOOL InitInstance(HANDLE, int);
long CALLBACK MainWndProc(HWND, UINT, WPARAM, LPARAM);
BOOL CALLBACK About(HWND, unsigned, WORD, LONG);
void RegisterWindow(HWND, WNDPROC);

#define IDC_EDIT 300
#define IDC_EDIT_INPUT 301
#define IDC_BUTTON 400
#define IDC_SLIDER 500

#define NUMBUTTONS 11
// 7 buttons high:
#define NUMBUTTONS_VERT 7

extern int abort_flag;

#ifdef __cplusplus
}
#endif 

#include "winmain2.h"
