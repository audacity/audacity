/**********************************************************************

  Audacity: A Digital Audio Editor

  Theme.h

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

**********************************************************************/

#ifndef __AUDACITY_THEME__
#define __AUDACITY_THEME__

#include <map>
#include <unordered_set>
#include <vector>
#include <optional>

#include <QColor>

#include "au3-components/ComponentInterfaceSymbol.h"

#include "au3-utility/Observer.h"
#include "au3-preferences/Prefs.h"

//! A choice of theme such as "Light", "Dark", ...
using teThemeType = Identifier;

//! A system theme, that matches selected theme best (only works on macOS with builtin themes).
enum class PreferredSystemAppearance
{
    Light,
    Dark,
    HighContrastDark
};

class wxArrayString;
class wxBitmap;
class wxColour;
class wxImage;
class wxPen;

class ChoiceSetting;

// JKC: will probably change name from 'teBmps' to 'tIndexBmp';
using teBmps = int; /// The index of a bitmap resource in Theme Resources.

enum teResourceType
{
    resTypeColour,
    resTypeBitmap,
    resTypeImage = resTypeBitmap,
};

enum teResourceFlags
{
    resFlagNone   =0x00,
    resFlagPaired =0x01,
    resFlagCursor =0x02,
    resFlagNewLine = 0x04,
    resFlagInternal = 0x08, // For image manipulation.  Don't save or load.
    resFlagSkip = 0x10
};

struct ThemeSet
{
    // wxImage, wxBitmap copy cheaply using reference counting
    std::vector<QColor> mColours;

    bool bInitialised = false;
};

struct ThemeChangeMessage {
    std::optional<PreferredSystemAppearance> appearance; /*!<
      An enum value when preferred system appearance changes, or nullopt
      for change of the image set */
};

class THEME_API ThemeBase /* not final */ : public Observer::Publisher<ThemeChangeMessage>
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

    std::map<Identifier, ThemeSet> mSets;
    ThemeSet* mpSet = nullptr;
};

class THEME_API Theme final : public ThemeBase
{
    friend class AColor; // So it can publish
public:
    Theme();
    ~Theme() override = default;

    void EnsureInitialised() override;
    void RegisterColours();
};

extern THEME_API Theme theTheme;

extern THEME_API ChoiceSetting
& GUITheme()
;

#endif // __AUDACITY_THEME__
