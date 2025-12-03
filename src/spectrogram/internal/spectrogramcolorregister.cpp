/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramcolorregister.h"

#include "spectrogramcolordefinitions.h"

// theTheme is a global variable.
SpectrogramColorRegister theTheme;

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

QColor& SpectrogramColorRegisterBase::Colour(int iIndex)
{
    wxASSERT(iIndex >= 0);
    EnsureInitialised();
    return mSet.mColours[iIndex];
}

void SpectrogramColorRegister::EnsureInitialised()
{
    if (mSet.bInitialised) {
        return;
    }
    RegisterColours();
}

void SpectrogramColorRegisterBase::RegisterColour(NameSet& allNames,
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

void SpectrogramColorRegister::RegisterColours()
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
