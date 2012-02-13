/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadNyquist.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>
#include <wx/filefn.h>
#include <wx/list.h>
#include <wx/log.h>
#include <wx/string.h>

#include "../../Audacity.h"
#include "../../AudacityApp.h"
#include "../EffectManager.h"
#include "Nyquist.h"

void LoadNyquistEffect(wxString fname)
{
   EffectNyquist *effect = new EffectNyquist(fname);
   if (effect->LoadedNyFile())
      EffectManager::Get().RegisterEffect(effect);
   else
      delete effect;
}

void LoadNyquistPlugins()
{
   wxArrayString audacityPathList = wxGetApp().audacityPathList;
   wxArrayString pathList;
   wxArrayString files;
   unsigned int i;

   // Create one "interactive Nyquist"
   EffectNyquist *effect = new EffectNyquist(wxT(""));
   EffectManager::Get().RegisterEffect(effect);

   // Load .ny plug-ins
   for(i=0; i<audacityPathList.GetCount(); i++) {
      wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
      wxGetApp().AddUniquePathToPathList(prefix + wxT("nyquist"),
                                         pathList);
      wxGetApp().AddUniquePathToPathList(prefix + wxT("plugins"),
                                         pathList);
      wxGetApp().AddUniquePathToPathList(prefix + wxT("plug-ins"),
                                         pathList);
   }

   wxGetApp().FindFilesInPathList(wxT("*.ny"), pathList, files);
#ifndef  __WXMSW__
   wxGetApp().FindFilesInPathList(wxT("*.NY"), pathList, files); // Ed's fix for bug 179
#endif

   for(i=0; i<files.GetCount(); i++)
      LoadNyquistEffect(files[i]);
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 4d0ce991-4c11-40a7-952f-3d9250b2f813

