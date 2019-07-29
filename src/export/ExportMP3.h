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

class ExportPlugin;
class wxString;
class wxWindow;
/** Factory method New_ExportMP3() which creates a NEW ExportMP3 object and
 * returns a pointer to it. The rest of the class declaration is in ExportMP3.cpp
 */
std::unique_ptr<ExportPlugin> New_ExportMP3();

//----------------------------------------------------------------------------
// Get MP3 library version
//----------------------------------------------------------------------------
wxString GetMP3Version(wxWindow *parent, bool prompt);

#endif

