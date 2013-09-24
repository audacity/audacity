/**********************************************************************

  Audacity: A Digital Audio Editor

  DevicePrefs.h

  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_DEVICE_PREFS__
#define __AUDACITY_DEVICE_PREFS__

#include <wx/defs.h>

#include <wx/choice.h>
#include <wx/string.h>
#include <wx/window.h>
#include <wx/dynarray.h>

#include "../ShuttleGui.h"

#include "PrefsPanel.h"

class DevicePrefs:public PrefsPanel
{
 public:
   DevicePrefs(wxWindow * parent);
   virtual ~DevicePrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void GetNamesAndLabels();

   void OnHost(wxCommandEvent & e);
   void OnDevice(wxCommandEvent & e);

   wxArrayString mHostNames;
   wxArrayString mHostLabels;

   wxString mPlayDevice;
   wxString mRecordDevice;
   wxString mRecordSource;
   long mRecordChannels;

   wxChoice *mHost;
   wxChoice *mPlay;
   wxChoice *mRecord;
   wxChoice *mChannels;

   DECLARE_EVENT_TABLE();
};

#endif
