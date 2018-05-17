/**********************************************************************

  Audacity: A Digital Audio Editor

  TransportOSCPrefs.h by Grégory David <groolot@groolot.net>

  Based on PalybackPrefs.h by:
   - Joshua Haberman
   - James Crook

**********************************************************************/

#include "../Experimental.h"

#ifdef USE_LIBLO

#ifndef __AUDACITY_TRANSPORT_OSC_PREFS__
#define __AUDACITY_TRANSPORT_OSC_PREFS__

class ShuttleGui;
class wxTextCtrl;

#include <map>

#include <wx/defs.h>
#include <lo/lo.h>

#include "PrefsPanel.h"

class TransportOSCPrefs final : public PrefsPanel
{
 public:
   TransportOSCPrefs(wxWindow * parent, wxWindowID winid);
   virtual ~TransportOSCPrefs();
   bool Commit() override;
   bool Validate();
   wxString HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

 private:
   void Populate();

   wxTextCtrl *mOSCDestinationHost;
   wxTextCtrl *mOSCDestinationPort;
   wxTextCtrl *mOSCRecordAddress;
   wxTextCtrl *mOSCPlayAddress;
   wxTextCtrl *mOSCStopAddress;
   wxTextCtrl *mOSCPauseAddress;
   wxTextCtrl *mOSCRewindAddress;
   wxTextCtrl *mOSCFastForwardAddress;

   std::map<wxString, wxTextCtrl *> OSCAddresses;
};

class TransportOSCPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *operator () (wxWindow *parent, wxWindowID winid) override;
};

#endif // __AUDACITY_TRANSPORT_OSC_PREFS__

#endif // USE_LIBLO
