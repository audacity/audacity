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

#ifndef MU_UI_IUICONFIGURATION_H
#define MU_UI_IUICONFIGURATION_H

#include <optional>

#include "types/retval.h"
#include "async/notification.h"

#include "modularity/imoduleinterface.h"

#include "uitypes.h"

class QByteArray;
class QWindow;

namespace mu::ui {
class IUiConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IUiConfiguration)

public:
    virtual ~IUiConfiguration() = default;

    virtual ThemeList themes() const = 0;
    virtual QStringList possibleFontFamilies() const = 0;
    virtual QStringList possibleAccentColors() const = 0;

    virtual bool isDarkMode() const = 0;
    virtual void setIsDarkMode(bool dark) = 0;

    virtual bool isHighContrast() const = 0;
    virtual void setIsHighContrast(bool highContrast) = 0;

    virtual const ThemeInfo& currentTheme() const = 0;
    virtual async::Notification currentThemeChanged() const = 0;
    virtual void setCurrentTheme(const ThemeCode& codeKey) = 0;
    virtual void setCurrentThemeStyleValue(ThemeStyleKey key, const Val& val) = 0;
    virtual void resetThemes() = 0;

    virtual bool isFollowSystemThemeAvailable() const = 0;
    virtual ValNt<bool> isFollowSystemTheme() const = 0;
    virtual void setFollowSystemTheme(bool follow) = 0;

    virtual std::string fontFamily() const = 0;
    virtual void setFontFamily(const std::string& family) = 0;
    virtual int fontSize(FontSizeType type = FontSizeType::BODY) const = 0;
    virtual void setBodyFontSize(int size) = 0;
    virtual async::Notification fontChanged() const = 0;

    virtual std::string iconsFontFamily() const = 0;
    virtual int iconsFontSize(IconSizeType type) const = 0;
    virtual async::Notification iconsFontChanged() const = 0;

    virtual std::string musicalFontFamily() const = 0;
    virtual int musicalFontSize() const = 0;
    virtual async::Notification musicalFontChanged() const = 0;

    virtual std::string defaultFontFamily() const = 0;
    virtual int defaultFontSize() const = 0;

    virtual void resetFonts() = 0;

    virtual double guiScaling() const = 0;
    virtual double physicalDpi() const = 0;
    virtual double logicalDpi() const = 0;

    //! NOTE Maybe set from command line
    virtual void setPhysicalDotsPerInch(std::optional<double> dpi) = 0;

    virtual ValNt<QByteArray> pageState(const QString& pageName) const = 0;
    virtual void setPageState(const QString& pageName, const QByteArray& state) = 0;

    virtual QByteArray windowGeometry() const = 0;
    virtual void setWindowGeometry(const QByteArray& state) = 0;
    virtual async::Notification windowGeometryChanged() const = 0;

    virtual bool isGlobalMenuAvailable() const = 0;

    virtual void applyPlatformStyle(QWindow* window) = 0;

    virtual bool isVisible(const QString& key, bool def = true) const = 0;
    virtual void setIsVisible(const QString& key, bool val) = 0;
    virtual async::Notification isVisibleChanged(const QString& key) const = 0;

    virtual ToolConfig toolConfig(const QString& toolName, const ToolConfig& defaultConfig) const = 0;
    virtual void setToolConfig(const QString& toolName, const ToolConfig& config) = 0;
    virtual async::Notification toolConfigChanged(const QString& toolName) const = 0;

    virtual int flickableMaxVelocity() const = 0;
};
}

#endif // MU_UI_IUICONFIGURATION_H
