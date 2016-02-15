/**********************************************************************

  Audacity: A Digital Audio Editor

  ThemePrefs.cpp

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

********************************************************************//**

\class ThemePrefs
\brief A PrefsPanel that configures dynamic loading of Theme
icons and colours.

Provides:
 - Button to save current theme as a single png image.
 - Button to load theme from a single png image.
 - Button to save current theme to multiple png images.
 - Button to load theme from multiple png images.
 - (Optional) Button to save theme as Cee data.
 - Button to read theme from default values in program.
 - CheckBox for loading custom themes at startup.

\see \ref Themability

*//********************************************************************/

#include "../Audacity.h"

#include <wx/wxprec.h>
#include "../Prefs.h"
#include "../Theme.h"
#include "../Project.h"
#include "../ShuttleGui.h"
#include "ThemePrefs.h"
#include "../AColor.h"

enum eThemePrefsIds {
   idLoadThemeCache=7000,
   idSaveThemeCache,
   idLoadThemeComponents,
   idSaveThemeComponents,
   idReadThemeInternal,
   idSaveThemeAsCode
};

BEGIN_EVENT_TABLE(ThemePrefs, PrefsPanel)
   EVT_BUTTON(idLoadThemeCache,      ThemePrefs::OnLoadThemeCache)
   EVT_BUTTON(idSaveThemeCache,      ThemePrefs::OnSaveThemeCache)
   EVT_BUTTON(idLoadThemeComponents, ThemePrefs::OnLoadThemeComponents)
   EVT_BUTTON(idSaveThemeComponents, ThemePrefs::OnSaveThemeComponents)
   EVT_BUTTON(idReadThemeInternal,   ThemePrefs::OnReadThemeInternal)
   EVT_BUTTON(idSaveThemeAsCode,     ThemePrefs::OnSaveThemeAsCode)
END_EVENT_TABLE()

ThemePrefs::ThemePrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Theme"))
{
   Populate();
}

ThemePrefs::~ThemePrefs(void)
{
}

/// Creates the dialog and its contents.
void ThemePrefs::Populate()
{
   // First any pre-processing for constructing the GUI.

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

/// Create the dialog contents, or exchange data with it.
void ThemePrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(_("Info"));
   {
      S.AddFixedText(
         _("Themability is an experimental feature.\n\nTo try it out, click \"Save Theme Cache\" then find and modify the images and colors in\nImageCacheVxx.png using an image editor such as the Gimp.\n\nClick \"Load Theme Cache\" to load the changed images and colors back into Audacity.\n\n(Only the Transport Toolbar and the colors on the wavetrack are currently affected, even\nthough the image file shows other icons too.)")
         );

#ifdef __WXDEBUG__
      S.AddFixedText(
         _("You have compiled Audacity with an extra button, 'Output Sourcery'.  This will save a\nC version of the image cache that can be compiled in as a default.")
         );
#endif

      S.AddFixedText(
         _("If 'Load Theme Cache At Startup' is checked, then the Theme Cache will be loaded\nwhen the program starts up.")
         );
      S.AddFixedText(
         _("Saving and loading individual theme files uses a separate file for each image, but is\notherwise the same idea.")
         );
   }
   S.EndStatic();

   /* i18n-hint: && in here is an escape character to get a single & on screen,
    * so keep it as is */
   S.StartStatic(		_("Theme Cache - Images && Color"));
   {
      S.StartHorizontalLay(wxALIGN_LEFT);
      {
         S.Id(idSaveThemeCache).AddButton(_("Save Theme Cache"));
         S.Id(idLoadThemeCache).AddButton(_("Load Theme Cache"));

         // This next button is only provided in Debug mode.
         // It is for developers who are compiling Audacity themselves
         // and who who wish to generate a NEW ThemeAsCeeCode.h and compile it in.
#ifdef __WXDEBUG__
         S.Id(idSaveThemeAsCode).AddButton(wxT("Output Sourcery"));
#endif

         S.Id(idReadThemeInternal).AddButton(_("&Defaults"));
      }
      S.EndHorizontalLay();

      S.StartHorizontalLay(wxALIGN_LEFT);
      {
         S.TieCheckBox(_("Load Theme Cache At Startup"),
                       wxT("/Theme/LoadAtStart"),
                       false);
      }
      S.EndHorizontalLay();
   }
   S.EndStatic();

   // JKC: 'Ergonomic' details:
   // Theme components are used much less frequently than
   // the ImageCache.  Yet it's easy to click them 'by mistake'.
   //
   // To reduce that risk, we use a separate box to separate them off.
   // And choose text on the buttons that is shorter, making the
   // buttons smaller and less tempting to click.
   S.StartStatic( _("Individual Theme Files"),1);
   {
      S.StartHorizontalLay(wxALIGN_LEFT);
      {
         S.Id(idSaveThemeComponents).AddButton( _("Save Files"));
         S.Id(idLoadThemeComponents).AddButton( _("Load Files"));
      }
      S.EndHorizontalLay();
   }
   S.EndStatic();
}

/// Load Theme from multiple png files.
void ThemePrefs::OnLoadThemeComponents(wxCommandEvent & WXUNUSED(event))
{
   theTheme.LoadComponents();
   theTheme.ApplyUpdatedImages();
}

/// Save Theme to multiple png files.
void ThemePrefs::OnSaveThemeComponents(wxCommandEvent & WXUNUSED(event))
{
   theTheme.SaveComponents();
}

/// Load Theme from single png file.
void ThemePrefs::OnLoadThemeCache(wxCommandEvent & WXUNUSED(event))
{
   theTheme.ReadImageCache();
   AColor::ReInit();
   theTheme.ApplyUpdatedImages();
}

/// Save Theme to single png file.
void ThemePrefs::OnSaveThemeCache(wxCommandEvent & WXUNUSED(event))
{
   theTheme.CreateImageCache();
   theTheme.WriteImageMap();// bonus - give them the html version.
}

/// Read Theme from internal storage.
void ThemePrefs::OnReadThemeInternal(wxCommandEvent & WXUNUSED(event))
{
   theTheme.ReadThemeInternal();
   theTheme.ApplyUpdatedImages();
}

/// Save Theme as C source code.
void ThemePrefs::OnSaveThemeAsCode(wxCommandEvent & WXUNUSED(event))
{
   theTheme.SaveThemeAsCode();
   theTheme.WriteImageDefs();// bonus - give them the Defs too.
}

/// Update the preferences stored on disk.
bool ThemePrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

PrefsPanel *ThemePrefsFactory::Create(wxWindow *parent)
{
   wxASSERT(parent); // to justify safenew
   return safenew ThemePrefs(parent);
}
