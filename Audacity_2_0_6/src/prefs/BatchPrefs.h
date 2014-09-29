/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchPrefs.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_BATCH_PREFS__
#define __AUDACITY_BATCH_PREFS__

#include <wx/defs.h>

#include <wx/window.h>

#include "../ShuttleGui.h"

#include "PrefsPanel.h"

class BatchPrefs : public PrefsPanel
{
public:
   BatchPrefs(wxWindow * parent);
   ~BatchPrefs();
   virtual bool Apply();

private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);

   DECLARE_EVENT_TABLE();
};

#endif
