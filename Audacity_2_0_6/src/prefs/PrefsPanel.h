/**********************************************************************

  Audacity: A Digital Audio Editor

  PrefsPanel.h

  Joshua Haberman

*******************************************************************//**

\class PrefsPanel
\brief Used within the PrefsDialog, classes derived from this class
include AudioIOPrefs, BatchPrefs, DirectoriesPrefs, FileFormatPrefs,
GUIPrefs, KeyConfigPrefs, MousePrefs, QualityPrefs, SpectrumPrefs and
ThemePrefs.

  The interface works like this: Each panel in the preferences dialog
  must derive from PrefsPanel. You must override Apply() with code
  to validate fields (returning false if any are bad), updating the
  global preferences object gPrefs, and instructing the applicable parts
  of the program to re-read the preference options.

  To actually add a the new panel, edit the PrefsDialog constructor
  to append the panel to its list of panels.

*//*******************************************************************/

#ifndef __AUDACITY_PREFS_PANEL__
#define __AUDACITY_PREFS_PANEL__

#include <wx/panel.h>
#include <wx/window.h>

/* A few constants for an attempt at semi-uniformity */
#define PREFS_FONT_SIZE     8

/* these are spacing guidelines: ie. radio buttons should have a 5 pixel
 * border on each side */
#define RADIO_BUTTON_BORDER    5
#define TOP_LEVEL_BORDER       5
#define GENERIC_CONTROL_BORDER 5

class PrefsPanel:public wxPanel
{
 public:
   PrefsPanel(wxWindow * parent, wxString title)
   :  wxPanel(parent, wxID_ANY)
   {
      SetLabel(title);     // Provide visual label
      SetName(title);      // Provide audible label
   }

   virtual ~PrefsPanel()
   {
   }

   virtual bool Apply() = 0;

   virtual void Cancel()
   {
   }
};

#endif
