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

#ifndef MU_UI_UICONFIGURATION_H
#define MU_UI_UICONFIGURATION_H

#include "iuiconfiguration.h"

#include "modularity/ioc.h"
#include "imainwindow.h"
#include "internal/iplatformtheme.h"

#include "types/val.h"
#include "uiarrangement.h"
#include "async/asyncable.h"

namespace mu::ui {
class UiConfiguration : public IUiConfiguration, public async::Asyncable
{
    INJECT(IMainWindow, mainWindow)
    INJECT(IPlatformTheme, platformTheme)

public:
    void init();
    void load();
    void deinit();

    ThemeList themes() const override;
    QStringList possibleFontFamilies() const override;
    QStringList possibleAccentColors() const override;

    bool isDarkMode() const override;
    void setIsDarkMode(bool dark) override;

    bool isHighContrast() const override;
    void setIsHighContrast(bool highContrast) override;

    const ThemeInfo& currentTheme() const override;
    async::Notification currentThemeChanged() const override;
    void setCurrentTheme(const ThemeCode& codeKey) override;
    void setCurrentThemeStyleValue(ThemeStyleKey key, const Val& val) override;
    void resetThemes() override;

    bool isFollowSystemThemeAvailable() const override;
    ValNt<bool> isFollowSystemTheme() const override;
    void setFollowSystemTheme(bool follow) override;

    std::string fontFamily() const override;
    void setFontFamily(const std::string& family) override;
    int fontSize(FontSizeType type = FontSizeType::BODY) const override;
    void setBodyFontSize(int size) override;
    async::Notification fontChanged() const override;

    std::string iconsFontFamily() const override;
    int iconsFontSize(IconSizeType type) const override;
    async::Notification iconsFontChanged() const override;

    std::string musicalFontFamily() const override;
    int musicalFontSize() const override;
    async::Notification musicalFontChanged() const override;

    std::string defaultFontFamily() const override;
    int defaultFontSize() const override;

    void resetFonts() override;

    double guiScaling() const override;
    double physicalDpi() const override;
    double logicalDpi() const override;

    void setPhysicalDotsPerInch(std::optional<double> dpi) override;

    ValNt<QByteArray> pageState(const QString& pageName) const override;
    void setPageState(const QString& pageName, const QByteArray& state) override;

    QByteArray windowGeometry() const override;
    void setWindowGeometry(const QByteArray& geometry) override;
    async::Notification windowGeometryChanged() const override;

    bool isGlobalMenuAvailable() const override;

    void applyPlatformStyle(QWindow* window) override;

    bool isVisible(const QString& key, bool def = true) const override;
    void setIsVisible(const QString& key, bool val) override;
    async::Notification isVisibleChanged(const QString& key) const override;

    ToolConfig toolConfig(const QString& toolName, const ToolConfig& defaultConfig) const override;
    void setToolConfig(const QString& toolName, const ToolConfig& config) override;
    async::Notification toolConfigChanged(const QString& toolName) const override;

    int flickableMaxVelocity() const override;

private:
    void initThemes();
    void notifyAboutCurrentThemeChanged();
    void updateCurrentTheme();
    void updateThemes();

    void updateSystemThemeListeningStatus();
    void synchThemeWithSystemIfNecessary();

    void doSetIsDarkMode(bool dark);
    void doSetCurrentTheme(const ThemeCode& themeCode);

    ThemeCode currentThemeCodeKey() const;
    ThemeInfo makeStandardTheme(const ThemeCode& codeKey) const;

    ThemeList readThemes() const;
    void writeThemes(const ThemeList& themes);

    void updateToolConfig(const QString& toolName, ToolConfig& userConfig, const ToolConfig& defaultConfig) const;

    UiArrangement m_uiArrangement;

    async::Notification m_currentThemeChanged;
    async::Notification m_fontChanged;
    async::Notification m_musicalFontChanged;
    async::Notification m_iconsFontChanged;
    async::Notification m_windowGeometryChanged;

    ValNt<bool> m_isFollowSystemTheme;

    ThemeList m_themes;
    size_t m_currentThemeIndex = 0;
    std::optional<double> m_customDPI;
};
}

#endif // MU_UI_UICONFIGURATION_H
