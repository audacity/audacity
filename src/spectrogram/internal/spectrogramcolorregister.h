/**********************************************************************

  Audacity: A Digital Audio Editor

  Theme.h

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

**********************************************************************/

#ifndef __AUDACITY_THEME__
#define __AUDACITY_THEME__

#include "ComponentInterfaceSymbol.h" // Identifier

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

class THEME_API ThemeBase
{
public:
    ThemeBase (const ThemeBase&) = delete;
    ThemeBase& operator =(const ThemeBase&) = delete;

    ThemeBase() = default;
    virtual ~ThemeBase() = default;

    virtual void EnsureInitialised() = 0;

    using NameSet = std::unordered_set<wxString>;

    void RegisterColour(NameSet& allNames, int& iIndex, const QColor& Clr, const wxString& Name);

    QColor& Colour(int iIndex);

protected:
    wxArrayString mColourNames;

    ThemeSet mSet;
};

class THEME_API Theme final : public ThemeBase
{
public:
    Theme() = default;
    ~Theme() override = default;

    void EnsureInitialised() override;
    void RegisterColours();
};

extern THEME_API Theme theTheme;

#endif // __AUDACITY_THEME__
