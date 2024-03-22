/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#ifndef MU_UI_UITYPES_H
#define MU_UI_UITYPES_H

#include <vector>
#include <optional>
#include <cstring>
#include <QString>
#include <QMetaType>

#include "actions/actiontypes.h"
#include "context/shortcutcontext.h"
#include "shortcuts/shortcutstypes.h"
#include "types/mnemonicstring.h"
#include "types/ret.h"
#include "types/translatablestring.h"
#include "types/val.h"
#include "view/iconcodes.h"

namespace mu::ui {
using ThemeCode = std::string;

inline ThemeCode themeCodeFromString(const QString& str)
{
    return str.toStdString();
}

static const ThemeCode LIGHT_THEME_CODE("light");
static const ThemeCode DARK_THEME_CODE("dark");
static const ThemeCode HIGH_CONTRAST_WHITE_THEME_CODE("high_contrast_white");
static const ThemeCode HIGH_CONTRAST_BLACK_THEME_CODE("high_contrast_black");

inline std::vector<ThemeCode> allStandardThemeCodes()
{
    return {
        LIGHT_THEME_CODE,
        DARK_THEME_CODE,
        HIGH_CONTRAST_WHITE_THEME_CODE,
        HIGH_CONTRAST_BLACK_THEME_CODE
    };
}

inline bool isDarkTheme(const ThemeCode& themeCode)
{
    return themeCode == DARK_THEME_CODE
           || themeCode == HIGH_CONTRAST_BLACK_THEME_CODE;
}

inline bool isHighContrastTheme(const ThemeCode& themeCode)
{
    return themeCode == HIGH_CONTRAST_WHITE_THEME_CODE
           || themeCode == HIGH_CONTRAST_BLACK_THEME_CODE;
}

enum ThemeStyleKey
{
    UNKNOWN = -1,

    BACKGROUND_PRIMARY_COLOR = 0,
    BACKGROUND_SECONDARY_COLOR,
    POPUP_BACKGROUND_COLOR,
    TEXT_FIELD_COLOR,
    ACCENT_COLOR,
    STROKE_COLOR,
    BUTTON_COLOR,
    FONT_PRIMARY_COLOR,
    FONT_SECONDARY_COLOR,
    LINK_COLOR,
    FOCUS_COLOR,

    BORDER_WIDTH,
    NAVIGATION_CONTROL_BORDER_WIDTH,

    ACCENT_OPACITY_NORMAL,
    ACCENT_OPACITY_HOVER,
    ACCENT_OPACITY_HIT,

    BUTTON_OPACITY_NORMAL,
    BUTTON_OPACITY_HOVER,
    BUTTON_OPACITY_HIT,

    ITEM_OPACITY_DISABLED
};

struct ThemeInfo
{
    ThemeCode codeKey;
    std::string title;
    QMap<ThemeStyleKey, QVariant> values;
};

using ThemeList = std::vector<ThemeInfo>;

enum class FontSizeType {
    BODY,
    BODY_LARGE,
    TAB,
    HEADER,
    TITLE
};

enum class IconSizeType {
    Regular,
    Toolbar
};

class ContainerType
{
    Q_GADGET
public:
    enum Type
    {
        Undefined = 0,
        PrimaryPage,
        QmlDialog,
        QWidgetDialog
    };
    Q_ENUM(Type)
};

struct ContainerMeta
{
    ContainerType::Type type = ContainerType::Undefined;
    QString qmlPath;
    int widgetMetaTypeId = QMetaType::UnknownType;

    ContainerMeta() = default;

    ContainerMeta(const ContainerType::Type& type)
        : type(type) {}
    ContainerMeta(const ContainerType::Type& type, const QString& qmlPath)
        : type(type), qmlPath(qmlPath) {}
    ContainerMeta(const ContainerType::Type& type, int widgetMetaTypeId)
        : type(type), widgetMetaTypeId(widgetMetaTypeId) {}
};

// UiActions/Menu

struct UiContext
{
    UiContext() = default;
    constexpr UiContext(const char* ctx)
        : const_data(ctx) {}

    inline bool operator ==(const UiContext& ctx) const
    {
        return std::strcmp(const_data, ctx.const_data) == 0;
    }

    inline bool operator !=(const UiContext& ctx) const
    {
        return !this->operator ==(ctx);
    }

    std::string toString() const { return const_data ? std::string(const_data) : std::string(); }

private:
    const char* const_data = nullptr;
};

//! NOTE Only general UI contexts are declared here, which do not depend on the specifics of the application.
//! Application-specific UI contexts are declared in the `context/uicontext.h` file
static constexpr UiContext UiCtxUnknown = "UiCtxUnknown";
static constexpr UiContext UiCtxAny = "UiCtxAny";

enum class Checkable {
    No = 0,
    Yes
};

struct UiAction
{
    actions::ActionCode code;
    UiContext uiCtx = UiCtxAny;
    std::string scCtx = "any";
    MnemonicString title;
    TranslatableString description;
    IconCode::Code iconCode = IconCode::Code::NONE;
    Checkable checkable = Checkable::No;
    std::vector<std::string> shortcuts;

    UiAction() = default;
    UiAction(const actions::ActionCode& code, UiContext ctx, std::string scCtx, Checkable ch = Checkable::No)
        : code(code), uiCtx(ctx), scCtx(scCtx), checkable(ch) {}

    UiAction(const actions::ActionCode& code, UiContext ctx, std::string scCtx, const MnemonicString& title,
             Checkable ch = Checkable::No)
        : code(code), uiCtx(ctx), scCtx(scCtx), title(title), checkable(ch) {}

    UiAction(const actions::ActionCode& code, UiContext ctx, std::string scCtx, const MnemonicString& title,
             const TranslatableString& desc, Checkable ch = Checkable::No)
        : code(code), uiCtx(ctx), scCtx(scCtx), title(title), description(desc),  checkable(ch) {}

    UiAction(const actions::ActionCode& code, UiContext ctx, std::string scCtx, const MnemonicString& title,
             const TranslatableString& desc, IconCode::Code icon, Checkable ch = Checkable::No)
        : code(code), uiCtx(ctx), scCtx(scCtx), title(title), description(desc), iconCode(icon), checkable(ch) {}

    UiAction(const actions::ActionCode& code, UiContext ctx, std::string scCtx, const MnemonicString& title, IconCode::Code icon,
             Checkable ch = Checkable::No)
        : code(code), uiCtx(ctx), scCtx(scCtx), title(title), iconCode(icon), checkable(ch) {}

    bool isValid() const
    {
        return !code.empty();
    }

    bool operator==(const UiAction& other) const
    {
        return code == other.code
               && uiCtx == other.uiCtx
               && scCtx == other.scCtx
               && title == other.title
               && description == other.description
               && iconCode == other.iconCode
               && checkable == other.checkable
               && shortcuts == shortcuts;
    }
};

class UiActionList : public std::vector<UiAction>
{
public:
    UiActionList() = default;
    UiActionList(std::initializer_list<UiAction> l)
        : std::vector<UiAction>(l) {}
    UiActionList(std::vector<UiAction>::iterator b, std::vector<UiAction>::iterator e)
        : std::vector<UiAction>(b, e) {}

    bool contains(const actions::ActionCode& code) const
    {
        auto it = std::find_if(cbegin(), cend(), [code](const UiAction& a) {
            return a.code == code;
        });
        return it != cend();
    }

    std::optional<size_t> indexOf(const actions::ActionCode& code) const
    {
        for (size_t i = 0; i < size(); ++i) {
            if (at(i).code == code) {
                return i;
            }
        }

        return std::nullopt;
    }
};

struct UiActionState
{
    bool enabled = false;
    bool checked = false;

    inline bool operator ==(const UiActionState& st) const
    {
        return st.enabled == enabled && st.checked == checked;
    }

    inline bool operator !=(const UiActionState& st) const
    {
        return !this->operator ==(st);
    }

    static UiActionState make_disabled(bool checked = false)
    {
        return UiActionState { false, checked };
    }

    static UiActionState make_enabled(bool checked = false)
    {
        return UiActionState { true, checked };
    }

    QVariantMap toMap() const
    {
        return {
            { "enabled", enabled },
            { "checked", checked }
        };
    }
};

struct ToolConfig
{
    struct Item
    {
        actions::ActionCode action;
        bool show = true;

        Item() = default;
        Item(const actions::ActionCode& a, bool sh)
            : action(a), show(sh) {}

        bool isSeparator() const
        {
            return action.empty();
        }

        bool operator ==(const Item& other) const
        {
            return action == other.action
                   && show == other.show;
        }
    };

    QList<Item> items;

    bool isValid() const { return !items.isEmpty(); }
};
}

#endif // MU_UI_UITYPES_H
