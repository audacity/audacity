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
#ifndef MU_APPSHELL_APPEARANCEPREFERENCESMODEL_H
#define MU_APPSHELL_APPEARANCEPREFERENCESMODEL_H

#include <QObject>

#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"
#include "notation/inotationconfiguration.h"
#include "async/asyncable.h"

namespace mu::appshell {
class AppearancePreferencesModel : public QObject, public async::Asyncable
{
    Q_OBJECT

    INJECT(ui::IUiConfiguration, uiConfiguration)
    INJECT(notation::INotationConfiguration, notationConfiguration)
    INJECT(engraving::IEngravingConfiguration, engravingConfiguration)

    Q_PROPERTY(bool isFollowSystemThemeAvailable READ isFollowSystemThemeAvailable CONSTANT)
    Q_PROPERTY(bool isFollowSystemTheme READ isFollowSystemTheme WRITE setFollowSystemTheme NOTIFY isFollowSystemThemeChanged)

    Q_PROPERTY(bool highContrastEnabled READ highContrastEnabled WRITE setHighContrastEnabled NOTIFY themesChanged)
    Q_PROPERTY(QVariantList generalThemes READ generalThemes NOTIFY themesChanged)
    Q_PROPERTY(QVariantList highContrastThemes READ highContrastThemes NOTIFY themesChanged)
    Q_PROPERTY(QStringList accentColors READ accentColors NOTIFY themesChanged)

    Q_PROPERTY(QString currentThemeCode READ currentThemeCode WRITE setCurrentThemeCode NOTIFY themesChanged)
    Q_PROPERTY(int currentAccentColorIndex READ currentAccentColorIndex WRITE setCurrentAccentColorIndex NOTIFY themesChanged)

    Q_PROPERTY(int currentFontIndex READ currentFontIndex WRITE setCurrentFontIndex NOTIFY currentFontIndexChanged)
    Q_PROPERTY(int bodyTextSize READ bodyTextSize WRITE setBodyTextSize NOTIFY bodyTextSizeChanged)

    Q_PROPERTY(bool backgroundUseColor READ backgroundUseColor WRITE setBackgroundUseColor NOTIFY backgroundUseColorChanged)
    Q_PROPERTY(QColor backgroundColor READ backgroundColor WRITE setBackgroundColor NOTIFY backgroundColorChanged)
    Q_PROPERTY(
        QString backgroundWallpaperPath READ backgroundWallpaperPath WRITE setBackgroundWallpaperPath NOTIFY backgroundWallpaperPathChanged)

    Q_PROPERTY(bool foregroundUseColor READ foregroundUseColor WRITE setForegroundUseColor NOTIFY foregroundUseColorChanged)
    Q_PROPERTY(QColor foregroundColor READ foregroundColor WRITE setForegroundColor NOTIFY foregroundColorChanged)
    Q_PROPERTY(
        QString foregroundWallpaperPath READ foregroundWallpaperPath WRITE setForegroundWallpaperPath NOTIFY foregroundWallpaperPathChanged)

    Q_PROPERTY(bool scoreInversionEnabled READ scoreInversionEnabled WRITE setScoreInversionEnabled NOTIFY invertScoreColorChanged)

public:
    explicit AppearancePreferencesModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    enum ColorType {
        AccentColor,
        TextAndIconsColor,
        DisabledColor,
        BorderColor
    };
    Q_ENUM(ColorType)

    bool isFollowSystemThemeAvailable() const;
    bool isFollowSystemTheme() const;

    bool highContrastEnabled() const;
    QVariantList generalThemes() const;
    QVariantList highContrastThemes() const;

    QStringList accentColors() const;

    QString currentThemeCode() const;
    int currentAccentColorIndex() const;

    int currentFontIndex() const;
    int bodyTextSize() const;

    bool backgroundUseColor() const;
    QColor backgroundColor() const;
    QString backgroundWallpaperPath() const;

    bool foregroundUseColor() const;
    QColor foregroundColor() const;
    QString foregroundWallpaperPath() const;

    bool scoreInversionEnabled() const;

    Q_INVOKABLE void resetAppearancePreferencesToDefault();
    Q_INVOKABLE void setNewColor(const QColor& newColor, ColorType colorType);
    Q_INVOKABLE QStringList allFonts() const;
    Q_INVOKABLE QStringList wallpaperPathFilter() const;
    Q_INVOKABLE QString wallpapersDir() const;

public slots:
    void setFollowSystemTheme(bool enabled);
    void setHighContrastEnabled(bool enabled);
    void setCurrentThemeCode(const QString& themeCode);
    void setCurrentAccentColorIndex(int index);
    void setCurrentFontIndex(int index);
    void setBodyTextSize(int size);
    void setBackgroundUseColor(bool value);
    void setBackgroundColor(const QColor& color);
    void setBackgroundWallpaperPath(const QString& path);
    void setForegroundUseColor(bool value);
    void setForegroundColor(const QColor& color);
    void setForegroundWallpaperPath(const QString& path);
    void setScoreInversionEnabled(bool value);

signals:
    void isFollowSystemThemeChanged();
    void themesChanged();
    void currentFontIndexChanged();
    void bodyTextSizeChanged();
    void backgroundUseColorChanged();
    void backgroundColorChanged();
    void backgroundWallpaperPathChanged();
    void foregroundUseColorChanged();
    void foregroundColorChanged();
    void foregroundWallpaperPathChanged();
    void invertScoreColorChanged();

private:
    ui::ThemeInfo currentTheme() const;
    ui::ThemeList allThemes() const;
};
}

#endif // MU_APPSHELL_APPEARANCEPREFERENCESMODEL_H
