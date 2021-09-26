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



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#ifdef __WXMSW__
#include <initializer_list>
#endif



#include <wx/wx.h>
#include <wx/bitmap.h>
#include <wx/filefn.h>
#include <wx/image.h>
#include <wx/ffile.h>
#include <wx/filename.h>
#include <wx/textfile.h>
#include <wx/thread.h>
#include <wx/tooltip.h>

#include "Identifier.h"

#include "AColor.h"
#include "AudioIO.h"
#include "Diags.h"
#include "Envelope.h"
#include "FFT.h"
#include "FileFormats.h"
#include "ImageManipulation.h"
#include "LabelTrack.h"
#include "Mix.h"
#include "NoteTrack.h"
#include "Prefs.h"
#include "Sequence.h"
#include "TimeTrack.h"
#include "UndoManager.h"
#include "WaveTrack.h"
#include "widgets/ASlider.h"
#include "widgets/ProgressDialog.h"
#include "widgets/Ruler.h"

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
