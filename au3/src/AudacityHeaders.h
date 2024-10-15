/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityHeaders.h

  Dominic Mazzoni

  This is not a normal include file - it's currently only used
  on Mac OS X as a "precompiled header" file that's automatically
  included by all source files, resulting in roughly a 2x increase
  in compilation speed.

  When gcc 3.4 is released, it will have precompiled header support
  on other platforms, and this file could be adapted to support
  precompiled headers on Linux, etc.

**********************************************************************/



#include <stdlib.h>
#include <string.h>
#include <math.h>
#ifdef __WXMSW__
#include <initializer_list>
#endif

#include <rapidjson/document.h>
#include <rapidjson/writer.h>

#include <wx/wx.h>
#include <wx/bitmap.h>
#include <wx/filefn.h>
#include <wx/ffile.h>
#include <wx/filename.h>
#include <wx/textfile.h>
#include <wx/thread.h>
#include <wx/tooltip.h>

#include "Identifier.h"

#include "AColor.h"
#include "Diags.h"
#include "Envelope.h"
#include "FFT.h"
#include "ImageManipulation.h"
#include "Mix.h"
#include "Prefs.h"
#include "UndoManager.h"
#include "widgets/ASlider.h"

// PRL:  These lines allow you to remove Project.h above.
// They must be included before the definition of macro NEW below.
#include <set>
#include <map>

//#ifdef __WXMSW__
// Enable this to diagnose memory leaks too!
//    #include <wx/msw/msvcrt.h>      // redefines the NEW() operator
//#endif

#ifdef _MSC_VER
#ifdef _DEBUG
#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#undef new
#define DEBUG_NEW new(_NORMAL_BLOCK, __FILE__, __LINE__)
#define new DEBUG_NEW
#endif
#endif
