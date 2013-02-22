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
#include "LoadNyquist.h"

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
#ifdef  __WXGTK__
   wxGetApp().FindFilesInPathList(wxT("*.NY"), pathList, files); // Ed's fix for bug 179
#endif

   for(i=0; i<files.GetCount(); i++)
      LoadNyquistEffect(files[i]);
}
