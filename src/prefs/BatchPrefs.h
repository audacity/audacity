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

#include "PrefsPanel.h"

class ShuttleGui;

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

class BatchPrefsFactory : public PrefsPanelFactory
{
public:
   virtual PrefsPanel *Create(wxWindow *parent);
};
#endif
