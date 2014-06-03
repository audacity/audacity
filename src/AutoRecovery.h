/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2.  See License.txt.

   AutoRecovery.h

*******************************************************************/

#ifndef __AUDACITY_AUTORECOVERY__
#define __AUDACITY_AUTORECOVERY__

#include "Project.h"
#include "xml/XMLTagHandler.h"

#include <wx/debug.h>

//
// Show auto recovery dialog if there are projects to recover. Should be
// called once at Audacity startup.
//
// This function possibly opens new project windows while it recovers all
// projects. If so, it will re-use *pproj, if != NULL and set it to NULL.
//
// Returns: True, if the start of Audacity should continue as normal
//          False if Audacity should be quit immediately
//
// The didRecoverAnything param is strictly for a return value.
// Any value passed in is ignored.
//
bool ShowAutoRecoveryDialogIfNeeded(AudacityProject** pproj,
                                    bool *didRecoverAnything);

//
// XML Handler for a <recordingrecovery> tag
//
class RecordingRecoveryHandler: public XMLTagHandler
{
public:
   RecordingRecoveryHandler(AudacityProject* proj);
   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag);

   // This class only knows reading tags
   virtual void WriteXML(XMLWriter & WXUNUSED(xmlFile)) { wxASSERT(false); }

private:
   AudacityProject* mProject;
   int mChannel;
   int mNumChannels;
};

#endif
