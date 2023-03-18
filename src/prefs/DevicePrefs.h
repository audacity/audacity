/**********************************************************************

  Audacity: A Digital Audio Editor

  DevicePrefs.h

  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_DEVICE_PREFS__
#define __AUDACITY_DEVICE_PREFS__

#include <wx/defs.h>

#include "PrefsPanel.h"

class wxChoice;
class ShuttleGui;

#define DEVICE_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Device") }

class DevicePrefs final : public PrefsPanel
{
 public:
   DevicePrefs(wxWindow * parent, wxWindowID winid, AudacityProject* project);
   virtual ~DevicePrefs();
   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;

   bool Commit() override;
   ManualPageID HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

 private:
   void Populate();
   void GetNamesAndLabels();

   void OnHost(wxCommandEvent & e);
   void OnDevice(wxCommandEvent & e);

   AudacityProject* mProject;

   TranslatableStrings mHostNames;
   wxArrayStringEx mHostLabels;

   wxString mPlayDevice;
   wxString mRecordDevice;
   wxString mRecordSource;
   long mRecordChannels;

   wxChoice *mHost;
   wxChoice *mPlay;
   wxChoice *mRecord;
   wxChoice *mChannels;

   DECLARE_EVENT_TABLE()
};

PrefsPanel *DevicePrefsFactory(wxWindow *parent, wxWindowID winid, AudacityProject *);

#endif
