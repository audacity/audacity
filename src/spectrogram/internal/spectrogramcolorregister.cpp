/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramcolorregister.h"

#include "spectrogramcolordefinitions.h"

// spectrogramColorRegister is a global variable.
SpectrogramColorRegister spectrogramColorRegister;

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

QColor& SpectrogramColorRegister::Colour(int iIndex)
{
    wxASSERT(iIndex >= 0);
    EnsureInitialised();
    return mColours[iIndex];
}

void SpectrogramColorRegister::EnsureInitialised()
{
    if (mColorsInitialized) {
        return;
    }
    RegisterColours();
}

void SpectrogramColorRegister::RegisterColour(NameSet& allNames,
                                              int& iIndex, const QColor& Clr, const wxString& Name)
{
    mColours.push_back(Clr);
    auto index = mColours.size() - 1;
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
    if (mColorsInitialized) {
        return;
    }
    mColorsInitialized = true;

// This initialises the variables e.g
// RegisterImage( myFlags, bmpRecordButton, some image, wxT("RecordButton"));
    int myFlags = resFlagPaired;
    NameSet allNames;
#define SPECTROGRAM_COLORS_INITS
#include "spectrogramcolordefinitions.h"
}
