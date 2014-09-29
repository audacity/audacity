/* configwin.h

   Vaughan Johnson (vaughan @ audacityteam . org)
   October 2003

	Some definitions for Windows LADSPA plug-ins, the LADSPA_plugins-win project. 
	So far, we don't want all the declarations in config.h.

	Note that MSVC doesn't define fabsf, powf, or sqrtf for C, only C++, 
	so the GVerb.dsp project has the /TP global compiler option, to 
	tell it to treat the .c files as .cpp files, per
	"http://msdn.microsoft.com/library/default.asp?url=/library/en-us/vccore/html/_core_.2f.tc.2c_2f.tp.asp"
	*/
#ifndef _CONFIG_H
#define _CONFIG_H

// Fix "inline" keyword for MSVC.
#define inline __inline

// MSVC's math.h does not define isnan.
// According to "http://msdn.microsoft.com/library/default.asp?url=/library/en-us/vclang/html/vclrfNotNumberNANItems.asp", 
// this test should work.
static inline int isnan(double x) { return (int)(x != x); }

#define _USE_MATH_DEFINES
#define FFTW3 
#define WIN32
#include "LADSPA_plugins-win/ladspa.h"

#endif _CONFIG_H
