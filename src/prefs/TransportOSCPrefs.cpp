/**********************************************************************

  Audacity: A Digital Audio Editor

  TransportOSCPrefs.cpp

  2018 - Grégory David <groolot@groolot.net>

  Based on PalybackPrefs.cpp by:
   - Joshua Haberman
   - Dominic Mazzoni
   - James Crook

*******************************************************************//**

\class TransportOSCPrefs
\brief A PrefsPanel used to select TransportOSC options.

  Presents interface for user to update the various TransportOSC options
  like the host, port and OSC paths used when transport buttons are
  triggered.

*//********************************************************************/

#include "../Audacity.h"
#include "../Experimental.h"
#ifdef USE_LIBLO

#include <wx/defs.h>
#include <wx/textctrl.h>
#include <wx/regex.h>

#include "../ShuttleGui.h"
#include "../Prefs.h"
#include "../Internat.h"

#include "TransportOSCPrefs.h"

TransportOSCPrefs::TransportOSCPrefs(wxWindow * parent, wxWindowID winid)
:  PrefsPanel(parent, winid, _("Transport OSC"))
{
   Populate();
}

TransportOSCPrefs::~TransportOSCPrefs()
{
}

void TransportOSCPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void TransportOSCPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.StartScroller();
   S.SetBorder(2);

   S.StartStatic(_("Target OSC configuration"));
   {
      S.StartMultiColumn(2,wxEXPAND);
      {
         S.SetStretchyCol( 0 );
         S.SetStretchyCol( 1 );
         mOSCDestinationHost =
               S.TieTextBox(_("&Host:"),
                            wxT("/TransportOSC/Host"),
                            wxT("localhost"),
                            50);
         mOSCDestinationPort =
               S.TieNumericTextBox(_("&Port:"),
                                   wxT("/TransportOSC/Port"),
                                   wxT("9876"),
                                   5);
         S.AddFixedText(_("Protocol:"));
         S.StartMultiColumn(2, wxEXPAND);
         {
            S.StartRadioButtonGroup(wxT("/TransportOSC/Protocol"), LO_UDP);
            {
               S.TieRadioButton(wxT("TCP"), LO_TCP);
               S.TieRadioButton(wxT("UDP"), LO_UDP);
            }
            S.EndRadioButtonGroup();
         }
         S.EndMultiColumn();
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
   S.StartStatic(_("Transport triggering"));
   {
      S.AddFixedText(_("IMPORTANT: Each message sends, as its lonely argument, the trigger state: 'T' if ON, 'F' else."));
      S.StartMultiColumn(2,wxEXPAND);
      {
         S.SetStretchyCol( 0 );
         S.SetStretchyCol( 1 );
         S.StartStatic(_("Record trigger"));
         {
            S.TieCheckBox(_("Enable RECORD triggering"),
                          wxT("/TransportOSC/Record/Triggering"),
                          true);
            mOSCRecordAddress =
                  S.TieTextBox(_("Address:"),
                               wxT("/TransportOSC/Record/Address"),
                               wxT("/record"),
                               50);
         }
         S.EndStatic();
         S.StartStatic(_("Play trigger"));
         {
            S.TieCheckBox(_("Enable PLAY triggering"),
                          wxT("/TransportOSC/Play/Triggering"),
                          true);
            mOSCPlayAddress =
                  S.TieTextBox(_("Address:"),
                               wxT("/TransportOSC/Play/Address"),
                               wxT("/play"),
                               50);
         }
         S.EndStatic();
      }
      S.EndMultiColumn();
      S.StartMultiColumn(2,wxEXPAND);
      {
         S.SetStretchyCol( 0 );
         S.SetStretchyCol( 1 );
         S.StartStatic(_("Stop trigger"));
         {
            S.TieCheckBox(_("Enable STOP triggering"),
                          wxT("/TransportOSC/Stop/Triggering"),
                          true);
            mOSCStopAddress =
                  S.TieTextBox(_("Address:"),
                               wxT("/TransportOSC/Stop/Address"),
                               wxT("/stop"),
                               50);
         }
         S.EndStatic();
         S.StartStatic(_("Pause trigger"));
         {
            S.TieCheckBox(_("Enable PAUSE triggering"),
                          wxT("/TransportOSC/Pause/Triggering"),
                          true);
            mOSCPauseAddress =
                  S.TieTextBox(_("Address:"),
                               wxT("/TransportOSC/Pause/Address"),
                               wxT("/pause"),
                               50);
         }
         S.EndStatic();
      }
      S.EndMultiColumn();
      S.StartMultiColumn(2,wxEXPAND);
      {
         S.SetStretchyCol( 0 );
         S.SetStretchyCol( 1 );
         S.StartStatic(_("Rewind trigger"));
         {
            S.TieCheckBox(_("Enable REWIND triggering"),
                          wxT("/TransportOSC/Rewind/Triggering"),
                          true);
            mOSCRewindAddress =
                  S.TieTextBox(_("Address:"),
                               wxT("/TransportOSC/Rewind/Address"),
                               wxT("/rewind"),
                               50);
         }
         S.EndStatic();
         S.StartStatic(_("FastForward trigger"));
         {
            S.TieCheckBox(_("Enable FastForward triggering"),
                          wxT("/TransportOSC/FastForward/Triggering"),
                          true);
            mOSCFastForwardAddress =
                  S.TieTextBox(_("Address:"),
                               wxT("/TransportOSC/FastForward/Address"),
                               wxT("/fastforward"),
                               50);
         }
         S.EndStatic();
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
   S.EndScroller();
}

bool TransportOSCPrefs::Commit()
{
   if (!Validate())
      return false;

   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

bool TransportOSCPrefs::Validate()
{
   bool output = true;
   wxString messageBoxCaption(_("Validation error"));

#ifdef wxUSE_REGEX
   // Validate the OSC destination host
   // FQDN host RegExp taken from https://www.regextester.com/23 and set case insensitive
   wxRegEx reOSCHost(wxString("^(([a-z0-9]|[a-z0-9][-a-z0-9]*[a-z0-9])\\.)*([a-z0-9]|[a-z0-9][-a-z0-9]*[a-z0-9])$"),
                     wxRE_EXTENDED|wxRE_ICASE);
   // IPv4 RegExp available at https://regexr.com/3pjnh
   wxRegEx reOSCIPv4(wxString("^((25[0-5]|(2[0-4]|1?[1-9])?[0-9])\\.){3}(25[0-5]|(2[0-4]|1?[1-9])?[0-9])$"),
                     wxRE_EXTENDED);
   if (!reOSCHost.Matches(mOSCDestinationHost->GetValue())
       && !reOSCIPv4.Matches(mOSCDestinationHost->GetValue()))
   {
      AudacityMessageBox(_("OSC host must be a Fully Qualified Domain Name or an IPv4 address"),
                         messageBoxCaption,
                         wxOK|wxCENTER|wxICON_ERROR);
      output = false;
   }

   // Validate the OSC destination port (TCP or UDP)
   unsigned long OSCDestinationPort;
   if (!mOSCDestinationPort->GetValue().ToULong(&OSCDestinationPort))
   {
      AudacityMessageBox(_("The destination OSC port (TCP/UDP) must be an unsigned integer"),
                         messageBoxCaption,
                         wxOK|wxCENTER|wxICON_ERROR);
      output = false;
   }
   else if (OSCDestinationPort > 65535)
   {
      AudacityMessageBox(_("The destination OSC port number (TCP/UDP) must be positive and less or equal than 65535"),
                         messageBoxCaption,
                         wxOK|wxCENTER|wxICON_ERROR);
      output = false;
   }

   // Validate the triggers addresses
   wxString errorMessage;
   // OSC Address pattern available at https://regexr.com/3pkc5
   wxRegEx reOSCAddress(wxString("^(\\/[a-z0-9?*]*(\\[!?([a-z0-9]|[a-z0-9]-[a-z0-9])+]|\\{([a-z0-9]+,)*[a-z0-9]+\\})*)+$"),
                        wxRE_EXTENDED|wxRE_ICASE);

   OSCAddresses[wxString("Record")] = mOSCRecordAddress;
   OSCAddresses[wxString("Play")] = mOSCPlayAddress;
   OSCAddresses[wxString("Stop")] = mOSCStopAddress;
   OSCAddresses[wxString("Pause")] = mOSCPauseAddress;
   OSCAddresses[wxString("Rewind")] = mOSCRewindAddress;
   OSCAddresses[wxString("FastForward")] = mOSCFastForwardAddress;
   for (auto &OSCAddress : OSCAddresses)
   {
      if (!reOSCAddress.Matches(OSCAddress.second->GetValue()))
      {
         errorMessage.Printf(_("The OSC %s trigger address \"%s\" is not valid"),
                             OSCAddress.first,
                             OSCAddress.second->GetValue());
         AudacityMessageBox(errorMessage,
                            messageBoxCaption,
                            wxOK|wxCENTER|wxICON_ERROR);
         output = false;
      }
   }

#endif
   return output;
}

wxString TransportOSCPrefs::HelpPageName()
{
   return "TransportOSC_Preferences";
}

PrefsPanel *TransportOSCPrefsFactory::operator () (wxWindow *parent, wxWindowID winid)
{
   wxASSERT(parent); // to justify safenew
   return safenew TransportOSCPrefs(parent, winid);
}

#endif
