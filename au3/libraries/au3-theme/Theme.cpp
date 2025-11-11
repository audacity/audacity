/**********************************************************************

  Audacity: A Digital Audio Editor

  Theme.cpp

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

********************************************************************//**

\class Theme
\brief Based on ThemeBase, Theme manages image and icon resources.

   Theme is a class which manages theme resources.
   It maps sets of ids to the resources and to names of the resources,
   so that they can be loaded/saved from files.

   Theme adds the Audacity specific images to ThemeBase.

\see \ref Themability

*//*****************************************************************//**

\class ThemeBase
\brief Theme management - Image loading and saving.

   Base for the Theme class. ThemeBase is a generic
   non-Audacity specific class.

\see \ref Themability

*//*****************************************************************//**

\class FlowPacker
\brief Packs rectangular boxes into a rectangle, using simple first fit.

This class is currently used by Theme to pack its images into the image
cache.  Perhaps someday we will improve FlowPacker and make it more flexible,
and use it for toolbar and window layouts too.

*//*****************************************************************//**

\class SourceOutputStream
\brief Allows us to capture output of the Save .png and 'pipe' it into
our own output function which gives a series of numbers.

This class is currently used by Theme to pack its images into the image
cache.  Perhaps someday we will improve FlowPacker and make it more flexible,
and use it for toolbar and window layouts too.

*//*****************************************************************/

#include "Theme.h"

#include <map>

#include <wx/wxprec.h>
#include <wx/brush.h>
#include <wx/pen.h>
#include <wx/file.h>
#include <wx/ffile.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>
#include <wx/mstream.h>
#include <wx/settings.h>
#include <regex>

#include "AllThemeResources.h"
#include "au3-basic-ui/BasicUI.h"
#include "au3-files/FileNames.h"
#include "au3-preferences/Prefs.h"
#include "ImageManipulation.h"
#include "au3-strings/Internat.h"
#include "au3-utility/MemoryX.h"

// theTheme is a global variable.
THEME_API Theme theTheme;

QColor& ThemeBase::Colour(int iIndex)
{
    wxASSERT(iIndex >= 0);
    auto& resources = *mpSet;
    EnsureInitialised();
    return resources.mColours[iIndex];
}

void Theme::EnsureInitialised()
{
    if (!mpSet || mpSet->bInitialised) {
        return;
    }
    RegisterColours();
}

void ThemeBase::RegisterColour(NameSet& allNames,
                               int& iIndex, const QColor& Clr, const wxString& Name)
{
    auto& resources = *mpSet;
    resources.mColours.push_back(Clr);
    auto index = resources.mColours.size() - 1;
    if (iIndex == -1) {
        // First time assignment of global variable identifying a colour
        iIndex = index;
        mColourNames.push_back(Name);
        wxASSERT(allNames.insert(Name).second);
    } else {
        // If revisiting for another theme set,
        // colours should be re-done in the same sequence
        wxASSERT(iIndex == index);
        wxASSERT(mColourNames[index] == Name);
    }
}

void Theme::RegisterColours()
{
    if (!mpSet || mpSet->bInitialised) {
        return;
    }
    mpSet->bInitialised = true;

// This initialises the variables e.g
// RegisterImage( myFlags, bmpRecordButton, some image, wxT("RecordButton"));
    int myFlags = resFlagPaired;
    NameSet allNames;
#define THEME_INITS
#include "AllThemeResources.h"
}
