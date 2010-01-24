/* pmwin.c -- PortMidi os-dependent code */

/* This file only needs to implement:
       pm_init(), which calls various routines to register the 
           available midi devices,
       Pm_GetDefaultInputDeviceID(), and
       Pm_GetDefaultOutputDeviceID().
   This file must
   be separate from the main portmidi.c file because it is system
   dependent, and it is separate from, say, pmwinmm.c, because it
   might need to register devices for winmm, directx, and others.

 */

#include "stdlib.h"
#include "portmidi.h"
#include "pmutil.h"
#include "pminternal.h"
#include "pmwinmm.h"
#ifdef DEBUG
#include "stdio.h"
#endif
#include <windows.h>

/* pm_exit is called when the program exits.
   It calls pm_term to make sure PortMidi is properly closed.
   If DEBUG is on, we prompt for input to avoid losing error messages.
 */
static void pm_exit(void) {
    pm_term();
#ifdef DEBUG
#define STRING_MAX 80
    {
        char line[STRING_MAX];
        printf("Type ENTER...\n");
        /* note, w/o this prompting, client console application can not see one
           of its errors before closing. */
        fgets(line, STRING_MAX, stdin);
    }
#endif
}


/* pm_init is the windows-dependent initialization.*/
void pm_init(void)
{
    atexit(pm_exit);
#ifdef DEBUG
    printf("registered pm_exit with atexit()\n");
#endif
    pm_winmm_init();
    /* initialize other APIs (DirectX?) here */
}


void pm_term(void) {
    pm_winmm_term();
}


static PmDeviceID pm_get_default_device_id(int is_input, char *key) {
    HKEY hkey;
#define PATTERN_MAX 256
    char pattern[PATTERN_MAX];
    long pattern_max = PATTERN_MAX;
    DWORD dwType;
    /* Find first input or device -- this is the default. */
    PmDeviceID id = pmNoDevice;
    int i, j;
    Pm_Initialize(); /* make sure descriptors exist! */
    for (i = 0; i < pm_descriptor_index; i++) {
        if (descriptors[i].pub.input == is_input) {
            id = i;
            break;
        }
    }
    /* Look in registry for a default device name pattern. */
    if (RegOpenKeyEx(HKEY_CURRENT_USER, "Software", 0, KEY_READ, &hkey) != 
        ERROR_SUCCESS) {
        return id;
    }
    if (RegOpenKeyEx(hkey, "JavaSoft", 0, KEY_READ, &hkey) !=
        ERROR_SUCCESS) {
        return id;
    }
    if (RegOpenKeyEx(hkey, "Prefs", 0, KEY_READ, &hkey) !=
        ERROR_SUCCESS) {
        return id;
    }
    if (RegOpenKeyEx(hkey, "/Port/Midi", 0, KEY_READ, &hkey) !=
        ERROR_SUCCESS) {
        return id;
    }
    if (RegQueryValueEx(hkey, key, NULL, &dwType, pattern, &pattern_max) != 
	ERROR_SUCCESS) {
        return id;
    }

    /* decode pattern: upper case encoded with "/" prefix */
    i = j = 0;
    while (pattern[i]) {
        if (pattern[i] == '/' && pattern[i + 1]) {
            pattern[j++] = toupper(pattern[++i]);
	} else {
            pattern[j++] = tolower(pattern[i]);
	}
        i++;
    }
    pattern[j] = 0; /* end of string */

    /* now pattern is the string from the registry; search for match */
    i = pm_find_default_device(pattern, is_input);
    if (i != pmNoDevice) {
        id = i;
    }
    return id;
}


PmDeviceID Pm_GetDefaultInputDeviceID() {
    return pm_get_default_device_id(TRUE, 
           "/P/M_/R/E/C/O/M/M/E/N/D/E/D_/I/N/P/U/T_/D/E/V/I/C/E");
}


PmDeviceID Pm_GetDefaultOutputDeviceID() {
  return pm_get_default_device_id(FALSE,
          "/P/M_/R/E/C/O/M/M/E/N/D/E/D_/O/U/T/P/U/T_/D/E/V/I/C/E");
}


#include "stdio.h" 

void *pm_alloc(size_t s) {
    return malloc(s); 
}


void pm_free(void *ptr) { 
    free(ptr); 
}


