/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP3.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORTMP3__
#define __AUDACITY_EXPORTMP3__

/* --------------------------------------------------------------------------*/

class ExportPlugin;
class wxString;
class wxWindow;
/** Factory method New_ExportMP3() which creates a new ExportMP3 object and
 * returns a pointer to it. The rest of the class declaration is in ExportMP3.cpp
 */
ExportPlugin *New_ExportMP3();

//----------------------------------------------------------------------------
// Get MP3 library versioqn
//----------------------------------------------------------------------------
wxString GetMP3Version(wxWindow *parent, bool prompt);

#endif

