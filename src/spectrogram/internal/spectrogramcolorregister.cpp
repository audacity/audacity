/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramcolorregister.h"

#include "spectrogramcolordefinitions.h"

#include <cassert>

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
    assert(iIndex >= 0);
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
                                              int& iIndex, const QColor& Clr, const std::string& Name)
{
    mColours.push_back(Clr);
    auto index = static_cast<int>(mColours.size()) - 1;
    if (iIndex == -1) {
        // First time assignment of global variable identifying a colour
        iIndex = index;
        allNames.insert(Name);
    } else {
        // If revisiting for another theme set,
        // colours should be re-done in the same sequence
        assert(iIndex == index);
    }
}

void SpectrogramColorRegister::RegisterColours()
{
    if (mColorsInitialized) {
        return;
    }
    mColorsInitialized = true;

// This initialises the variables e.g
// RegisterImage( myFlags, bmpRecordButton, some image, "RecordButton");
    [[maybe_unused]] int myFlags = resFlagPaired;
    NameSet allNames;
#define SPECTROGRAM_COLORS_INITS
#include "spectrogramcolordefinitions.h"
}
