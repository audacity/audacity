/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP3.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORTMP3__
#define __AUDACITY_EXPORTMP3__

/* --------------------------------------------------------------------------*/

#include "../MemoryX.h"

#define MODE_SET           0
#define MODE_VBR           1
#define MODE_ABR           2
#define MODE_CBR           3

#if defined(__WXMSW__) || defined(__WXMAC__)
#define MP3_EXPORT_BUILT_IN 1
#endif

class wxString;
class wxWindow;

//----------------------------------------------------------------------------
// Get MP3 library version
//----------------------------------------------------------------------------
wxString GetMP3Version(wxWindow *parent, bool prompt);

#endif

