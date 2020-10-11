/**********************************************************************

  Audacity: A Digital Audio Editor

  @file MP3Prefs.cpp
  @brief adds controls for MP3 import/export to Library preferences

  Paul Licameli split from LibraryPrefs.cpp

**********************************************************************/

#include "ExportMP3.h"
#include "Internat.h"
#include "ShuttleGui.h"
#include "../prefs/LibraryPrefs.h"
#include "widgets/HelpSystem.h"
#include "widgets/ReadOnlyText.h"
#include <wx/stattext.h>

namespace {

#if 0
////////////////////////////////////////////////////////////////////////////////

#define ID_FFMPEG_FIND_BUTTON       7003
#define ID_FFMPEG_DOWN_BUTTON       7004

BEGIN_EVENT_TABLE(LibraryPrefs, PrefsPanel)
   EVT_BUTTON(ID_FFMPEG_FIND_BUTTON, LibraryPrefs::OnFFmpegFindButton)
   EVT_BUTTON(ID_FFMPEG_DOWN_BUTTON, LibraryPrefs::OnFFmpegDownButton)
END_EVENT_TABLE()

LibraryPrefs::LibraryPrefs(wxWindow * parent, wxWindowID winid)
/* i18-hint: refers to optional plug-in software libraries */
:   PrefsPanel(parent, winid, XO("Libraries"))
{
   Populate();
}

LibraryPrefs::~LibraryPrefs()
{
}

ComponentInterfaceSymbol LibraryPrefs::GetSymbol() const
{
   return LIBRARY_PREFS_PLUGIN_SYMBOL;
}

TranslatableString LibraryPrefs::GetDescription() const
{
   return XO("Preferences for Library");
}

ManualPageID LibraryPrefs::HelpPageName()
{
   return "Libraries_Preferences";
}

/// Creates the dialog and its contents.
void LibraryPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------

   // Set the MP3 Version string.
   SetMP3VersionText();
   SetFFmpegVersionText();
}

#endif

void AddControls( ShuttleGui &S )
{
   S.StartStatic(XO("LAME MP3 Export Library"));
   {
      S.StartTwoColumn();
      {
         auto MP3Version = S
            .Position(wxALIGN_CENTRE_VERTICAL)
            .AddReadOnlyText(XO("MP3 Library Version:"), "");
         MP3Version->SetValue(GetMP3Version(S.GetParent(), false));
      }
      S.EndTwoColumn();
   }
   S.EndStatic();
}

LibraryPrefs::RegisteredControls reg{ wxT("MP3"), AddControls };

}
