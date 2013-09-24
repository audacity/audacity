/**********************************************************************

  Audacity: A Digital Audio Editor

  RecordingPrefs.h

  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_RECORDING_PREFS__
#define __AUDACITY_RECORDING_PREFS__

#include <wx/defs.h>

#include <wx/window.h>

#include "../ShuttleGui.h"

#include "PrefsPanel.h"

class RecordingPrefs:public PrefsPanel
{
 public:
   RecordingPrefs(wxWindow * parent);
   virtual ~RecordingPrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
};

#endif
