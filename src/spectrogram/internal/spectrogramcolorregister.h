/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QColor>

#include <wx/arrstr.h>

#include <unordered_set>
#include <vector>

struct ThemeSet
{
    // wxImage, wxBitmap copy cheaply using reference counting
    std::vector<QColor> mColours;

    bool bInitialised = false;
};

class SpectrogramColorRegisterBase
{
public:
    SpectrogramColorRegisterBase (const SpectrogramColorRegisterBase&) = delete;
    SpectrogramColorRegisterBase& operator =(const SpectrogramColorRegisterBase&) = delete;

    SpectrogramColorRegisterBase() = default;
    virtual ~SpectrogramColorRegisterBase() = default;

    virtual void EnsureInitialised() = 0;

    using NameSet = std::unordered_set<wxString>;

    void RegisterColour(NameSet& allNames, int& iIndex, const QColor& Clr, const wxString& Name);

    QColor& Colour(int iIndex);

protected:
    wxArrayString mColourNames;

    ThemeSet mSet;
};

class SpectrogramColorRegister final : public SpectrogramColorRegisterBase
{
public:
    SpectrogramColorRegister() = default;
    ~SpectrogramColorRegister() override = default;

    void EnsureInitialised() override;
    void RegisterColours();
};

extern SpectrogramColorRegister spectrogramColorRegister;
