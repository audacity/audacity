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
#include "uiconfiguration.h"

#include "async/async.h"
#include "settings.h"
#include "themeconverter.h"

#include <QScreen>
#include <QFontDatabase>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>

#ifdef Q_OS_WIN
#include <QOperatingSystemVersion>
#endif

#include "log.h"

using namespace mu;
using namespace mu::ui;
using namespace mu::async;

static const Settings::Key UI_THEMES_KEY("ui", "ui/application/themes");
static const Settings::Key UI_CURRENT_THEME_CODE_KEY("ui", "ui/application/currentThemeCode");
static const Settings::Key UI_FOLLOW_SYSTEM_THEME_KEY("ui", "ui/application/followSystemTheme");
static const Settings::Key UI_FONT_FAMILY_KEY("ui", "ui/theme/fontFamily");
static const Settings::Key UI_FONT_SIZE_KEY("ui", "ui/theme/fontSize");
static const Settings::Key UI_ICONS_FONT_FAMILY_KEY("ui", "ui/theme/iconsFontFamily");
static const Settings::Key UI_MUSICAL_FONT_FAMILY_KEY("ui", "ui/theme/musicalFontFamily");
static const Settings::Key UI_MUSICAL_FONT_SIZE_KEY("ui", "ui/theme/musicalFontSize");

static const QString WINDOW_GEOMETRY_KEY("window");

static const int FLICKABLE_MAX_VELOCITY = 1500;

static const QMap<ThemeStyleKey, QVariant> LIGHT_THEME_VALUES {
    { BACKGROUND_PRIMARY_COLOR, "#F5F5F6" },
    { BACKGROUND_SECONDARY_COLOR, "#E6E9ED" },
    { POPUP_BACKGROUND_COLOR, "#F5F5F6" },
    { TEXT_FIELD_COLOR, "#FFFFFF" },
    { ACCENT_COLOR, "#70AFEA" },
    { STROKE_COLOR, "#CED1D4" },
    { BUTTON_COLOR, "#CFD5DD" },
    { FONT_PRIMARY_COLOR, "#111132" },
    { FONT_SECONDARY_COLOR, "#FFFFFF" },
    { LINK_COLOR, "#0B69BF" },
    { FOCUS_COLOR, "#75507b" },

    { BORDER_WIDTH, 0 },
    { NAVIGATION_CONTROL_BORDER_WIDTH, 2.0 },

    { ACCENT_OPACITY_NORMAL, 0.5 },
    { ACCENT_OPACITY_HOVER, 0.3 },
    { ACCENT_OPACITY_HIT, 0.7 },

    { BUTTON_OPACITY_NORMAL, 0.7 },
    { BUTTON_OPACITY_HOVER, 0.5 },
    { BUTTON_OPACITY_HIT, 1.0 },

    { ITEM_OPACITY_DISABLED, 0.3 }
};

static const QMap<ThemeStyleKey, QVariant> DARK_THEME_VALUES {
    { BACKGROUND_PRIMARY_COLOR, "#2D2D30" },
    { BACKGROUND_SECONDARY_COLOR, "#363638" },
    { POPUP_BACKGROUND_COLOR, "#39393C" },
    { TEXT_FIELD_COLOR, "#242427" },
    { ACCENT_COLOR, "#2093FE" },
    { STROKE_COLOR, "#1E1E1E" },
    { BUTTON_COLOR, "#595959" },
    { FONT_PRIMARY_COLOR, "#EBEBEB" },
    { FONT_SECONDARY_COLOR, "#BDBDBD" },
    { LINK_COLOR, "#8EC9FF" },
    { FOCUS_COLOR, "#75507b" },

    { BORDER_WIDTH, 0 },
    { NAVIGATION_CONTROL_BORDER_WIDTH, 2.0 },

    { ACCENT_OPACITY_NORMAL, 0.5 },
    { ACCENT_OPACITY_HOVER, 0.3 },
    { ACCENT_OPACITY_HIT, 0.7 },

    { BUTTON_OPACITY_NORMAL, 0.7 },
    { BUTTON_OPACITY_HOVER, 0.5 },
    { BUTTON_OPACITY_HIT, 1.0 },

    { ITEM_OPACITY_DISABLED, 0.3 }
};

static const QMap<ThemeStyleKey, QVariant> HIGH_CONTRAST_BLACK_THEME_VALUES {
    { BACKGROUND_PRIMARY_COLOR, "#000000" },
    { BACKGROUND_SECONDARY_COLOR, "#000000" },
    { POPUP_BACKGROUND_COLOR, "#000000" },
    { TEXT_FIELD_COLOR, "#000000" },
    { ACCENT_COLOR, "#0071DA" },
    { STROKE_COLOR, "#FFFFFF" },
    { BUTTON_COLOR, "#000000" },
    { FONT_PRIMARY_COLOR, "#FFFD38" },
    { FONT_SECONDARY_COLOR, "#BDBDBD" },
    { LINK_COLOR, "#FFFFFF" },
    { FOCUS_COLOR, "#75507b" },

    { BORDER_WIDTH, 1.0 },
    { NAVIGATION_CONTROL_BORDER_WIDTH, 2.0 },

    { ACCENT_OPACITY_NORMAL, 0.5 },
    { ACCENT_OPACITY_HOVER, 0.3 },
    { ACCENT_OPACITY_HIT, 0.7 },

    { BUTTON_OPACITY_NORMAL, 0.7 },
    { BUTTON_OPACITY_HOVER, 0.5 },
    { BUTTON_OPACITY_HIT, 1.0 },

    { ITEM_OPACITY_DISABLED, 0.3 }
};

static const QMap<ThemeStyleKey, QVariant> HIGH_CONTRAST_WHITE_THEME_VALUES {
    { BACKGROUND_PRIMARY_COLOR, "#FFFFFF" },
    { BACKGROUND_SECONDARY_COLOR, "#FFFFFF" },
    { POPUP_BACKGROUND_COLOR, "#FFFFFF" },
    { TEXT_FIELD_COLOR, "#FFFFFF" },
    { ACCENT_COLOR, "#00D87D" },
    { STROKE_COLOR, "#000000" },
    { BUTTON_COLOR, "#FFFFFF" },
    { FONT_PRIMARY_COLOR, "#1E0073" },
    { FONT_SECONDARY_COLOR, "#000000" },
    { LINK_COLOR, "#000000" },
    { FOCUS_COLOR, "#75507b" },

    { BORDER_WIDTH, 1.0 },
    { NAVIGATION_CONTROL_BORDER_WIDTH, 2.0 },

    { ACCENT_OPACITY_NORMAL, 0.5 },
    { ACCENT_OPACITY_HOVER, 0.3 },
    { ACCENT_OPACITY_HIT, 0.7 },

    { BUTTON_OPACITY_NORMAL, 0.7 },
    { BUTTON_OPACITY_HOVER, 0.5 },
    { BUTTON_OPACITY_HIT, 1.0 },

    { ITEM_OPACITY_DISABLED, 0.3 }
};

void UiConfiguration::init()
{
    settings()->setDefaultValue(UI_CURRENT_THEME_CODE_KEY, Val(LIGHT_THEME_CODE));
    settings()->setDefaultValue(UI_FOLLOW_SYSTEM_THEME_KEY, Val(false));
    settings()->setDefaultValue(UI_FONT_FAMILY_KEY, Val(defaultFontFamily()));
    settings()->setDefaultValue(UI_FONT_SIZE_KEY, Val(defaultFontSize()));
    settings()->setDefaultValue(UI_ICONS_FONT_FAMILY_KEY, Val("MusescoreIcon"));
    settings()->setDefaultValue(UI_MUSICAL_FONT_FAMILY_KEY, Val("Leland"));
    settings()->setDefaultValue(UI_MUSICAL_FONT_SIZE_KEY, Val(24));
    settings()->setDefaultValue(UI_THEMES_KEY, Val(""));

    settings()->valueChanged(UI_THEMES_KEY).onReceive(this, [this](const Val&) {
        updateThemes();
        notifyAboutCurrentThemeChanged();
    });

    settings()->valueChanged(UI_CURRENT_THEME_CODE_KEY).onReceive(this, [this](const Val&) {
        notifyAboutCurrentThemeChanged();
    });

    settings()->valueChanged(UI_FOLLOW_SYSTEM_THEME_KEY).onReceive(this, [this](const Val& val) {
        m_isFollowSystemTheme.set(val.toBool());
        updateSystemThemeListeningStatus();
    });

    settings()->valueChanged(UI_FONT_FAMILY_KEY).onReceive(this, [this](const Val&) {
        m_fontChanged.notify();
    });

    settings()->valueChanged(UI_FONT_SIZE_KEY).onReceive(this, [this](const Val&) {
        m_fontChanged.notify();
        m_iconsFontChanged.notify();
    });

    settings()->valueChanged(UI_ICONS_FONT_FAMILY_KEY).onReceive(this, [this](const Val&) {
        m_iconsFontChanged.notify();
    });

    settings()->valueChanged(UI_MUSICAL_FONT_FAMILY_KEY).onReceive(this, [this](const Val&) {
        m_musicalFontChanged.notify();
    });

    settings()->valueChanged(UI_MUSICAL_FONT_SIZE_KEY).onReceive(this, [this](const Val&) {
        m_musicalFontChanged.notify();
    });

    m_uiArrangement.stateChanged(WINDOW_GEOMETRY_KEY).onNotify(this, [this]() {
        m_windowGeometryChanged.notify();
    });

    initThemes();
}

void UiConfiguration::load()
{
    m_uiArrangement.load();
}

void UiConfiguration::deinit()
{
    platformTheme()->stopListening();
}

void UiConfiguration::initThemes()
{
    m_isFollowSystemTheme.val = settings()->value(UI_FOLLOW_SYSTEM_THEME_KEY).toBool();

    platformTheme()->platformThemeChanged().onNotify(this, [this]() {
        synchThemeWithSystemIfNecessary();
    });

    updateSystemThemeListeningStatus();

    for (const ThemeCode& codeKey : allStandardThemeCodes()) {
        m_themes.push_back(makeStandardTheme(codeKey));
    }

    updateThemes();
    updateCurrentTheme();
}

void UiConfiguration::updateCurrentTheme()
{
    ThemeCode currentCodeKey = currentThemeCodeKey();

    for (size_t i = 0; i < m_themes.size(); ++i) {
        if (m_themes[i].codeKey == currentCodeKey) {
            m_currentThemeIndex = i;
            break;
        }
    }

    platformTheme()->applyPlatformStyleOnAppForTheme(currentCodeKey);
}

void UiConfiguration::updateThemes()
{
    ThemeList modifiedThemes = readThemes();

    if (modifiedThemes.empty()) {
        m_themes.clear();
        for (const ThemeCode& codeKey : allStandardThemeCodes()) {
            m_themes.push_back(makeStandardTheme(codeKey));
        }

        return;
    }

    for (ThemeInfo& theme: m_themes) {
        auto it = std::find_if(modifiedThemes.begin(), modifiedThemes.end(), [theme](const ThemeInfo& modifiedTheme) {
            return modifiedTheme.codeKey == theme.codeKey;
        });

        bool isModified = it != modifiedThemes.end();
        if (isModified) {
            theme = *it;
        }
    }
}

bool UiConfiguration::isFollowSystemThemeAvailable() const
{
    return platformTheme()->isFollowSystemThemeAvailable();
}

ValNt<bool> UiConfiguration::isFollowSystemTheme() const
{
    return m_isFollowSystemTheme;
}

void UiConfiguration::setFollowSystemTheme(bool follow)
{
    settings()->setSharedValue(UI_FOLLOW_SYSTEM_THEME_KEY, Val(follow));
}

void UiConfiguration::updateSystemThemeListeningStatus()
{
    if (isFollowSystemTheme().val) {
        platformTheme()->startListening();
        synchThemeWithSystemIfNecessary();
    } else {
        platformTheme()->stopListening();
    }
}

void UiConfiguration::synchThemeWithSystemIfNecessary()
{
    if (!m_isFollowSystemTheme.val) {
        return;
    }

    doSetIsDarkMode(platformTheme()->isSystemThemeDark());
}

void UiConfiguration::notifyAboutCurrentThemeChanged()
{
    updateCurrentTheme();
    m_currentThemeChanged.notify();
}

ThemeInfo UiConfiguration::makeStandardTheme(const ThemeCode& codeKey) const
{
    ThemeInfo theme;
    theme.codeKey = codeKey;

    if (codeKey == LIGHT_THEME_CODE) {
        theme.values = LIGHT_THEME_VALUES;
    } else if (codeKey == DARK_THEME_CODE) {
        theme.values = DARK_THEME_VALUES;
    } else if (codeKey == HIGH_CONTRAST_WHITE_THEME_CODE) {
        theme.values = HIGH_CONTRAST_WHITE_THEME_VALUES;
    } else if (codeKey == HIGH_CONTRAST_BLACK_THEME_CODE) {
        theme.values = HIGH_CONTRAST_BLACK_THEME_VALUES;
    }

    return theme;
}

ThemeList UiConfiguration::readThemes() const
{
    TRACEFUNC;

    ThemeList result;

    QByteArray json = QByteArray::fromStdString(settings()->value(UI_THEMES_KEY).toString());
    if (json.isEmpty()) {
        return result;
    }

    QJsonParseError err;
    QJsonDocument jsodDoc = QJsonDocument::fromJson(json, &err);
    if (err.error != QJsonParseError::NoError) {
        LOGE() << "Couldn't read themes: " << err.errorString();
        return result;
    }

    if (!jsodDoc.isArray()) {
        return result;
    }

    QVariantList objList = jsodDoc.array().toVariantList();

    for (const QVariant& themeObj: objList) {
        result.push_back(ThemeConverter::fromMap(themeObj.toMap()));
    }

    return result;
}

void UiConfiguration::writeThemes(const ThemeList& themes)
{
    TRACEFUNC;

    QJsonArray jsonArray;
    for (const ThemeInfo& theme : themes) {
        QVariantMap map = ThemeConverter::toMap(theme);
        jsonArray << QJsonObject::fromVariantMap(map);
    }

    QJsonDocument jsonDoc(jsonArray);

    Val value(jsonDoc.toJson(QJsonDocument::Compact).constData());
    settings()->setSharedValue(UI_THEMES_KEY, value);
}

ThemeList UiConfiguration::themes() const
{
    return m_themes;
}

QStringList UiConfiguration::possibleFontFamilies() const
{
#ifdef MU_QT5_COMPAT
    QFontDatabase db;
    QStringList allFonts = db.families();
#else
    QStringList allFonts = QFontDatabase::families();
#endif
    QStringList smuflFonts
        = { "Bravura", "Campania", "Edwin", "Finale Broadway", "Finale Maestro", "Gootville", "Leland", "MScore", "MuseJazz", "Petaluma" };
    for (const QString& font : smuflFonts) {
        allFonts.removeAll(font);
    }
    return allFonts;
}

QStringList UiConfiguration::possibleAccentColors() const
{
    static const QStringList lightAccentColors {
        "#F28585",
        "#EDB17A",
        "#E0CC87",
        "#8BC9C5",
        "#70AFEA",
        "#A09EEF",
        "#DBA0C7"
    };

    static const QStringList darkAccentColors {
        "#F25555",
        "#E1720B",
        "#AC8C1A",
        "#27A341",
        "#2093FE",
        "#926BFF",
        "#E454C4"
    };

    if (isDarkMode()) {
        return darkAccentColors;
    }

    return lightAccentColors;
}

void UiConfiguration::resetThemes()
{
    m_themes.clear();
    for (const ThemeCode& codeKey : allStandardThemeCodes()) {
        m_themes.push_back(makeStandardTheme(codeKey));
    }

    writeThemes(m_themes);

    notifyAboutCurrentThemeChanged();
}

const ThemeInfo& UiConfiguration::currentTheme() const
{
    return m_themes[m_currentThemeIndex];
}

ThemeCode UiConfiguration::currentThemeCodeKey() const
{
    ThemeCode preferredThemeCode = settings()->value(UI_CURRENT_THEME_CODE_KEY).toString();
    return preferredThemeCode.empty() ? LIGHT_THEME_CODE : preferredThemeCode;
}

bool UiConfiguration::isDarkMode() const
{
    return isDarkTheme(currentThemeCodeKey());
}

void UiConfiguration::setIsDarkMode(bool dark)
{
    setFollowSystemTheme(false);

    doSetIsDarkMode(dark);
}

void UiConfiguration::doSetIsDarkMode(bool dark)
{
    if (isHighContrast()) {
        doSetCurrentTheme(dark ? HIGH_CONTRAST_BLACK_THEME_CODE : HIGH_CONTRAST_WHITE_THEME_CODE);
    } else {
        doSetCurrentTheme(dark ? DARK_THEME_CODE : LIGHT_THEME_CODE);
    }
}

bool UiConfiguration::isHighContrast() const
{
    return isHighContrastTheme(currentThemeCodeKey());
}

void UiConfiguration::setIsHighContrast(bool highContrast)
{
    if (isDarkMode()) {
        doSetCurrentTheme(highContrast ? HIGH_CONTRAST_BLACK_THEME_CODE : DARK_THEME_CODE);
    } else {
        doSetCurrentTheme(highContrast ? HIGH_CONTRAST_WHITE_THEME_CODE : LIGHT_THEME_CODE);
    }
}

void UiConfiguration::setCurrentTheme(const ThemeCode& themeCode)
{
    setFollowSystemTheme(false);

    doSetCurrentTheme(themeCode);
}

void UiConfiguration::doSetCurrentTheme(const ThemeCode& themeCode)
{
    settings()->setSharedValue(UI_CURRENT_THEME_CODE_KEY, Val(themeCode));
}

void UiConfiguration::setCurrentThemeStyleValue(ThemeStyleKey key, const Val& val)
{
    ThemeInfo& currentTheme = m_themes[m_currentThemeIndex];
    currentTheme.values[key] = val.toQVariant();

    ThemeList modifiedThemes = readThemes();

    auto it = std::find_if(modifiedThemes.begin(), modifiedThemes.end(), [currentTheme](const ThemeInfo& theme) {
        return theme.codeKey == currentTheme.codeKey;
    });

    if (it != modifiedThemes.end()) {
        modifiedThemes.erase(it);
    }

    modifiedThemes.push_back(currentTheme);
    writeThemes(modifiedThemes);
}

Notification UiConfiguration::currentThemeChanged() const
{
    return m_currentThemeChanged;
}

std::string UiConfiguration::fontFamily() const
{
    return settings()->value(UI_FONT_FAMILY_KEY).toString();
}

void UiConfiguration::setFontFamily(const std::string& family)
{
    settings()->setSharedValue(UI_FONT_FAMILY_KEY, Val(family));
}

int UiConfiguration::fontSize(FontSizeType type) const
{
    int bodyFontSize = settings()->value(UI_FONT_SIZE_KEY).toInt();

    /*
     * DEFAULT SIZE:
     * body: 12
     * body large: 14
     * tab: 16
     * header: 22
     * title: 32
     */
    switch (type) {
    case FontSizeType::BODY: return bodyFontSize;
    case FontSizeType::BODY_LARGE: return bodyFontSize + bodyFontSize / 6;
    case FontSizeType::TAB: return bodyFontSize + bodyFontSize / 3;
    case FontSizeType::HEADER: return bodyFontSize + bodyFontSize / 1.2;
    case FontSizeType::TITLE: return bodyFontSize + bodyFontSize / 0.6;
    }

    return bodyFontSize;
}

void UiConfiguration::setBodyFontSize(int size)
{
    settings()->setSharedValue(UI_FONT_SIZE_KEY, Val(size));
}

Notification UiConfiguration::fontChanged() const
{
    return m_fontChanged;
}

std::string UiConfiguration::iconsFontFamily() const
{
    return settings()->value(UI_ICONS_FONT_FAMILY_KEY).toString();
}

int UiConfiguration::iconsFontSize(IconSizeType type) const
{
    int bodyFontSize = fontSize(FontSizeType::BODY) + 4;

    switch (type) {
    case IconSizeType::Regular: return bodyFontSize;
    case IconSizeType::Toolbar: return bodyFontSize + 2;
    }

    return bodyFontSize;
}

Notification UiConfiguration::iconsFontChanged() const
{
    return m_iconsFontChanged;
}

std::string UiConfiguration::musicalFontFamily() const
{
    return settings()->value(UI_MUSICAL_FONT_FAMILY_KEY).toString();
}

int UiConfiguration::musicalFontSize() const
{
    return settings()->value(UI_MUSICAL_FONT_SIZE_KEY).toInt();
}

Notification UiConfiguration::musicalFontChanged() const
{
    return m_musicalFontChanged;
}

std::string UiConfiguration::defaultFontFamily() const
{
#ifdef Q_OS_WIN
    static const QString defaultWinFamily = "Segoe UI";

#ifdef MU_QT5_COMPAT
    QFontDatabase fontDatabase;
    if (fontDatabase.hasFamily(defaultWinFamily)) {
#else
    if (QFontDatabase::hasFamily(defaultWinFamily)) {
#endif
        return defaultWinFamily.toStdString();
    }
#endif

    return QFontDatabase::systemFont(QFontDatabase::GeneralFont).family().toStdString();
}

int UiConfiguration::defaultFontSize() const
{
    return 12;
}

void UiConfiguration::resetFonts()
{
    settings()->setSharedValue(UI_FONT_FAMILY_KEY, settings()->defaultValue(UI_FONT_FAMILY_KEY));
    settings()->setSharedValue(UI_FONT_SIZE_KEY, settings()->defaultValue(UI_FONT_SIZE_KEY));
    settings()->setSharedValue(UI_ICONS_FONT_FAMILY_KEY, settings()->defaultValue(UI_ICONS_FONT_FAMILY_KEY));
    settings()->setSharedValue(UI_MUSICAL_FONT_FAMILY_KEY, settings()->defaultValue(UI_MUSICAL_FONT_FAMILY_KEY));
    settings()->setSharedValue(UI_MUSICAL_FONT_SIZE_KEY, settings()->defaultValue(UI_MUSICAL_FONT_SIZE_KEY));
}

double UiConfiguration::guiScaling() const
{
    const QScreen* screen = mainWindow() ? mainWindow()->screen() : nullptr;
    return screen ? screen->devicePixelRatio() : 1;
}

void UiConfiguration::setPhysicalDotsPerInch(std::optional<double> dpi)
{
    m_customDPI = dpi;
}

double UiConfiguration::physicalDpi() const
{
    if (m_customDPI) {
        return m_customDPI.value();
    }

    constexpr double DEFAULT_DPI = 96;
    const QScreen* screen = mainWindow() ? mainWindow()->screen() : nullptr;
    if (!screen) {
        return DEFAULT_DPI;
    }

    auto physicalSize = screen->physicalSize();
    // Work around xrandr reporting a 1x1mm size if
    // the screen doesn't have a valid physical size
    if (physicalSize.height() <= 1 && physicalSize.width() <= 1) {
        return DEFAULT_DPI;
    }

#ifdef Q_OS_WIN
    //! NOTE: copied from MU3, `MuseScore::MuseScore()`
    if (QOperatingSystemVersion::current() <= QOperatingSystemVersion(QOperatingSystemVersion::Windows, 7)) {
        return screen->logicalDotsPerInch() * screen->devicePixelRatio();
    }
#endif
    return screen->physicalDotsPerInch();
}

double UiConfiguration::logicalDpi() const
{
    const QScreen* screen = mainWindow() ? mainWindow()->screen() : nullptr;
    if (!screen) {
        constexpr double DEFAULT_DPI = 96;
        return DEFAULT_DPI;
    }

    return screen->logicalDotsPerInch();
}

mu::ValNt<QByteArray> UiConfiguration::pageState(const QString& pageName) const
{
    ValNt<QByteArray> result;
    result.val = m_uiArrangement.state(pageName);
    result.notification = m_uiArrangement.stateChanged(pageName);

    return result;
}

void UiConfiguration::setPageState(const QString& pageName, const QByteArray& state)
{
    m_uiArrangement.setState(pageName, state);
}

QByteArray UiConfiguration::windowGeometry() const
{
    return m_uiArrangement.state(WINDOW_GEOMETRY_KEY);
}

void UiConfiguration::setWindowGeometry(const QByteArray& geometry)
{
    m_uiArrangement.setState(WINDOW_GEOMETRY_KEY, geometry);
}

Notification UiConfiguration::windowGeometryChanged() const
{
    return m_windowGeometryChanged;
}

bool UiConfiguration::isGlobalMenuAvailable() const
{
    return platformTheme()->isGlobalMenuAvailable();
}

void UiConfiguration::applyPlatformStyle(QWindow* window)
{
    platformTheme()->applyPlatformStyleOnWindowForTheme(window, currentThemeCodeKey());
}

bool UiConfiguration::isVisible(const QString& key, bool def) const
{
    QString val = m_uiArrangement.value(key);
    bool ok = false;
    int valInt = val.toInt(&ok);
    return ok ? bool(valInt) : def;
}

void UiConfiguration::setIsVisible(const QString& key, bool val)
{
    m_uiArrangement.setValue(key, QString::number(val ? 1 : 0));
}

mu::async::Notification UiConfiguration::isVisibleChanged(const QString& key) const
{
    return m_uiArrangement.valueChanged(key);
}

ToolConfig UiConfiguration::toolConfig(const QString& toolName, const ToolConfig& defaultConfig) const
{
    ToolConfig config = m_uiArrangement.toolConfig(toolName);
    if (!config.isValid()) {
        return defaultConfig;
    }

    updateToolConfig(toolName, config, defaultConfig);
    return config;
}

void UiConfiguration::setToolConfig(const QString& toolName, const ToolConfig& config)
{
    m_uiArrangement.setToolConfig(toolName, config);
}

mu::async::Notification UiConfiguration::toolConfigChanged(const QString& toolName) const
{
    return m_uiArrangement.toolConfigChanged(toolName);
}

void UiConfiguration::updateToolConfig(const QString& toolName, ToolConfig& userConfig, const ToolConfig& defaultConfig) const
{
    bool hasChanged = false;

    // Remove items that are not in the default config
    {
        QList<ToolConfig::Item> itemsToRemove;
        for (const auto& item : userConfig.items) {
            if (item.isSeparator()) {
                continue;
            }

            if (std::find_if(defaultConfig.items.cbegin(), defaultConfig.items.cend(), [item](const auto& defaultItem) {
                return item.action == defaultItem.action;
            }) == defaultConfig.items.cend()) {
                itemsToRemove << item;
            }
        }

        for (const auto& itemToRemove : itemsToRemove) {
            hasChanged = true;
            userConfig.items.removeAll(itemToRemove);
        }
    }

    // Insert items that are missing in the user config
    {
        for (const auto& defaultItem : defaultConfig.items) {
            if (defaultItem.isSeparator()) {
                continue;
            }

            if (std::find_if(userConfig.items.cbegin(), userConfig.items.cend(), [defaultItem](const auto& item) {
                return defaultItem.action == item.action;
            }) == userConfig.items.cend()) {
                hasChanged = true;

                // Try to find a good place to insert the item
                int indexOfDefaultItem = defaultConfig.items.indexOf(defaultItem);

                // If it was at the start of the default items...
                if (indexOfDefaultItem == 0) {
                    // insert it at the start of the user items
                    userConfig.items.prepend(defaultItem);
                    continue;
                }

                // If it was at the end of the default items...
                if (indexOfDefaultItem == defaultConfig.items.size() - 1) {
                    // insert it at the end of the user items
                    userConfig.items.append(defaultItem);
                    continue;
                }

                // Look at the item before it...
                {
                    const auto& itemBefore = defaultConfig.items[indexOfDefaultItem - 1];
                    if (!itemBefore.isSeparator()) {
                        auto it = std::find_if(userConfig.items.begin(), userConfig.items.end(), [itemBefore](const auto& item) {
                            return item.action == itemBefore.action;
                        });

                        if (it != userConfig.items.end()) {
                            userConfig.items.insert(++it, defaultItem);
                            continue;
                        }
                    }
                }

                // Look at the item after it...
                {
                    const auto& itemAfter  = defaultConfig.items[indexOfDefaultItem + 1];
                    if (!itemAfter.isSeparator()) {
                        auto it = std::find_if(userConfig.items.begin(), userConfig.items.end(), [itemAfter](const auto& item) {
                            return item.action == itemAfter.action;
                        });

                        userConfig.items.insert(it, defaultItem);
                        continue;
                    }
                }

                // Last resort: just insert at the end
                userConfig.items.append(defaultItem);
            }
        }
    }

    if (hasChanged) {
        // Save for later
        auto self = const_cast<UiConfiguration*>(this);

        Async::call(self, [self, toolName, userConfig]() {
            self->setToolConfig(toolName, userConfig);
        });
    }
}

int UiConfiguration::flickableMaxVelocity() const
{
    return FLICKABLE_MAX_VELOCITY;
}
