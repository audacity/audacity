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

#include "spectrogramcolorregister.h"


#include "spectrogramcolordefinitions.h"

// theTheme is a global variable.
THEME_API Theme theTheme;

namespace {
enum teResourceFlags
{
    resFlagNone   =0x00,
    resFlagPaired =0x01,
    resFlagCursor =0x02,
    resFlagNewLine = 0x04,
    resFlagInternal = 0x08, // For image manipulation.  Don't save or load.
    resFlagSkip = 0x10
};
}

QColor& ThemeBase::Colour(int iIndex)
{
    wxASSERT(iIndex >= 0);
    EnsureInitialised();
    return mSet.mColours[iIndex];
}

void Theme::EnsureInitialised()
{
    if (mSet.bInitialised) {
        return;
    }
    RegisterColours();
}

void ThemeBase::RegisterColour(NameSet& allNames,
                               int& iIndex, const QColor& Clr, const wxString& Name)
{
    mSet.mColours.push_back(Clr);
    auto index = mSet.mColours.size() - 1;
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
    if (mSet.bInitialised) {
        return;
    }
    mSet.bInitialised = true;

// This initialises the variables e.g
// RegisterImage( myFlags, bmpRecordButton, some image, wxT("RecordButton"));
    int myFlags = resFlagPaired;
    NameSet allNames;
#define THEME_INITS
#include "spectrogramcolordefinitions.h"
}
