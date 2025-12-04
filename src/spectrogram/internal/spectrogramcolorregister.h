/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QColor>

#include <wx/arrstr.h>

#include <unordered_set>
#include <vector>

class SpectrogramColorRegister final
{
public:
    using NameSet = std::unordered_set<wxString>;

    void EnsureInitialised();
    void RegisterColour(NameSet& allNames, int& iIndex, const QColor& Clr, const wxString& Name);

    QColor& Colour(int iIndex);
    void RegisterColours();

private:
    wxArrayString mColourNames;
    std::vector<QColor> mColours;
    bool mColorsInitialized = false;
};

extern SpectrogramColorRegister spectrogramColorRegister;
