/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormatPrefs.h

  Joshua Haberman
  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_FILE_FORMAT_PREFS__
#define __AUDACITY_FILE_FORMAT_PREFS__

#include <wx/defs.h>

#include "PrefsPanel.h"

class wxStaticText;
class ShuttleGui;

class LibraryPrefs final : public PrefsPanel
{
 public:
   LibraryPrefs(wxWindow * parent, wxWindowID winid);
   ~LibraryPrefs();
   bool Commit() override;
   wxString HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

 private:
   void Populate();
   void SetMP3VersionText(bool prompt = false);
   void SetFFmpegVersionText();

   void OnMP3FindButton(wxCommandEvent & e);
   void OnMP3DownButton(wxCommandEvent & e);
   void OnFFmpegFindButton(wxCommandEvent & e);
   void OnFFmpegDownButton(wxCommandEvent & e);

   wxStaticText *mMP3Version;
   wxStaticText *mFFmpegVersion;

   DECLARE_EVENT_TABLE()
};

/// A PrefsPanelFactory that creates one LibraryPrefs panel.
class LibraryPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *operator () (wxWindow *parent, wxWindowID winid) override;
};
#endif
