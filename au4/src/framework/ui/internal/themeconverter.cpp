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

#include "themeconverter.h"

#include "translation.h"

#include "log.h"

using namespace mu;
using namespace mu::ui;

static const QString CODEKEY_KEY("codeKey");
static const QString TITLE_KEY("title");

static const std::vector<std::pair<ThemeStyleKey, QString > > s_keys = {
    { UNKNOWN, QString() },

    { BACKGROUND_PRIMARY_COLOR, "backgroundPrimaryColor" },
    { BACKGROUND_SECONDARY_COLOR, "backgroundSecondaryColor" },
    { POPUP_BACKGROUND_COLOR, "popupBackgroundColor" },
    { TEXT_FIELD_COLOR, "textFieldColor" },
    { ACCENT_COLOR, "accentColor" },
    { STROKE_COLOR, "strokeColor" },
    { BUTTON_COLOR, "buttonColor" },
    { BORDER_WIDTH, "borderWidth" },
    { FONT_PRIMARY_COLOR, "fontPrimaryColor" },
    { FONT_SECONDARY_COLOR, "fontSecondaryColor" },
    { LINK_COLOR, "linkColor" },
    { FOCUS_COLOR, "focusColor" },

    { BORDER_WIDTH, "borderWidth" },
    { NAVIGATION_CONTROL_BORDER_WIDTH, "navigationControlBorderWidth" },

    { ACCENT_OPACITY_NORMAL, "accentOpacityNormal" },
    { ACCENT_OPACITY_HOVER, "accentOpacityHover" },
    { ACCENT_OPACITY_HIT, "accentOpacityHit" },
    { BUTTON_OPACITY_NORMAL, "buttonOpacityNormal" },
    { BUTTON_OPACITY_HOVER, "buttonOpacityHover" },
    { BUTTON_OPACITY_HIT, "buttonOpacityHit" },

    { ITEM_OPACITY_DISABLED, "itemOpacityDisabled" },
};

static QString titleForTheme(const ThemeInfo& theme)
{
    if (theme.codeKey == LIGHT_THEME_CODE) {
        //: The name of the light ui theme
        return qtrc("ui", "Light");
    } else if (theme.codeKey == DARK_THEME_CODE) {
        //: The name of the dark ui theme
        return qtrc("ui", "Dark");
    } else if (theme.codeKey == HIGH_CONTRAST_WHITE_THEME_CODE) {
        //: The name of the high contrast light ui theme
        return qtrc("ui", "White");
    } else if (theme.codeKey == HIGH_CONTRAST_BLACK_THEME_CODE) {
        //: The name of the high contrast dark ui theme
        return qtrc("ui", "Black");
    }

    return QString::fromStdString(theme.title);
}

static const QString& themeStyleKeyToString(ThemeStyleKey key)
{
    for (const auto& p : s_keys) {
        if (p.first == key) {
            return p.second;
        }
    }

    IF_ASSERT_FAILED_X(false, QString("not found string key for enum key: %1").arg(static_cast<int>(key))) {
    }

    static const QString null;
    return null;
}

static ThemeStyleKey themeStyleKeyFromString(const QString& str)
{
    for (const auto& p : s_keys) {
        if (p.second == str) {
            return p.first;
        }
    }

    /*IF_ASSERT_FAILED_X(false, QString("not found enum key for string key: %1").arg(str)) {
    }*/

    return ThemeStyleKey::UNKNOWN;
}

QVariantMap ThemeConverter::toMap(const ThemeInfo& theme)
{
    QVariantMap obj;

    obj[CODEKEY_KEY] = QString::fromStdString(theme.codeKey);
    obj[TITLE_KEY] = titleForTheme(theme);

    for (ThemeStyleKey key : theme.values.keys()) {
        obj[themeStyleKeyToString(key)] = theme.values[key];
    }

    return obj;
}

ThemeInfo ThemeConverter::fromMap(const QVariantMap& map)
{
    ThemeInfo theme;
    theme.codeKey = map[CODEKEY_KEY].toString().toStdString();
    theme.title = map[TITLE_KEY].toString().toStdString();

    for (const QString& key : map.keys()) {
        theme.values[themeStyleKeyFromString(key)] = map[key];
    }

    return theme;
}
