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

#ifndef MU_API_THEMEAPI_H
#define MU_API_THEMEAPI_H

#include <QFont>
#include <QPainter>
#include <QProxyStyle>

#include "global/api/apiobject.h"
#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"
#include "async/asyncable.h"

namespace mu::api {
class ProxyStyle;
class ThemeApi : public ApiObject, public async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(bool isDark READ isDark NOTIFY themeChanged)

    Q_PROPERTY(QColor backgroundPrimaryColor READ backgroundPrimaryColor NOTIFY themeChanged)
    Q_PROPERTY(QColor backgroundSecondaryColor READ backgroundSecondaryColor NOTIFY themeChanged)
    Q_PROPERTY(QColor popupBackgroundColor READ popupBackgroundColor NOTIFY themeChanged)
    Q_PROPERTY(QColor textFieldColor READ textFieldColor NOTIFY themeChanged)
    Q_PROPERTY(QColor strokeColor READ strokeColor NOTIFY themeChanged)
    Q_PROPERTY(QColor accentColor READ accentColor NOTIFY themeChanged)
    Q_PROPERTY(QColor buttonColor READ buttonColor NOTIFY themeChanged)
    Q_PROPERTY(QColor fontPrimaryColor READ fontPrimaryColor NOTIFY themeChanged)
    Q_PROPERTY(QColor fontSecondaryColor READ fontSecondaryColor NOTIFY themeChanged)
    Q_PROPERTY(QColor linkColor READ linkColor NOTIFY themeChanged)
    Q_PROPERTY(QColor focusColor READ focusColor NOTIFY themeChanged)

    Q_PROPERTY(qreal borderWidth READ borderWidth NOTIFY themeChanged)
    Q_PROPERTY(qreal navCtrlBorderWidth READ navCtrlBorderWidth NOTIFY themeChanged)

    Q_PROPERTY(qreal accentOpacityNormal READ accentOpacityNormal NOTIFY themeChanged)
    Q_PROPERTY(qreal accentOpacityHit READ accentOpacityHit NOTIFY themeChanged)
    Q_PROPERTY(qreal accentOpacityHover READ accentOpacityHover NOTIFY themeChanged)

    Q_PROPERTY(qreal buttonOpacityNormal READ buttonOpacityNormal NOTIFY themeChanged)
    Q_PROPERTY(qreal buttonOpacityHover READ buttonOpacityHover NOTIFY themeChanged)
    Q_PROPERTY(qreal buttonOpacityHit READ buttonOpacityHit NOTIFY themeChanged)

    Q_PROPERTY(qreal itemOpacityDisabled READ itemOpacityDisabled NOTIFY themeChanged)

    Q_PROPERTY(QFont bodyFont READ bodyFont NOTIFY themeChanged)
    Q_PROPERTY(QFont bodyBoldFont READ bodyBoldFont NOTIFY themeChanged)
    Q_PROPERTY(QFont largeBodyFont READ largeBodyFont NOTIFY themeChanged)
    Q_PROPERTY(QFont largeBodyBoldFont READ largeBodyBoldFont NOTIFY themeChanged)
    Q_PROPERTY(QFont tabFont READ tabFont NOTIFY themeChanged)
    Q_PROPERTY(QFont tabBoldFont READ tabBoldFont NOTIFY themeChanged)
    Q_PROPERTY(QFont headerFont READ headerFont NOTIFY themeChanged)
    Q_PROPERTY(QFont headerBoldFont READ headerBoldFont NOTIFY themeChanged)
    Q_PROPERTY(QFont titleBoldFont READ titleBoldFont NOTIFY themeChanged)

    Q_PROPERTY(QFont iconsFont READ iconsFont NOTIFY themeChanged)
    Q_PROPERTY(QFont toolbarIconsFont READ toolbarIconsFont NOTIFY themeChanged)

    Q_PROPERTY(QFont musicalFont READ musicalFont NOTIFY themeChanged)

    Q_PROPERTY(QFont defaultFont READ defaultFont CONSTANT)

    Q_PROPERTY(qreal defaultButtonSize READ defaultButtonSize NOTIFY themeChanged)

    Q_PROPERTY(int flickableMaxVelocity READ flickableMaxVelocity CONSTANT)

public:

    Inject<ui::IUiConfiguration> configuration;

public:
    ThemeApi();
#ifdef MU_QT5_COMPAT
    ThemeApi(const ThemeApi& api)
        : ApiObject(api.engine()) {}
#endif
    ~ThemeApi();

    void init();
    void update();

    bool isDark() const;

    QColor backgroundPrimaryColor() const;
    QColor backgroundSecondaryColor() const;
    QColor popupBackgroundColor() const;
    QColor textFieldColor() const;
    QColor accentColor() const;
    QColor strokeColor() const;
    QColor buttonColor() const;
    QColor fontPrimaryColor() const;
    QColor fontSecondaryColor() const;
    QColor linkColor() const;
    QColor focusColor() const;

    QFont bodyFont() const;
    QFont bodyBoldFont() const;
    QFont largeBodyFont() const;
    QFont largeBodyBoldFont() const;
    QFont tabFont() const;
    QFont tabBoldFont() const;
    QFont headerFont() const;
    QFont headerBoldFont() const;
    QFont titleBoldFont() const;

    QFont iconsFont() const;
    QFont toolbarIconsFont() const;
    QFont musicalFont() const;

    QFont defaultFont() const;

    qreal defaultButtonSize() const;
    qreal borderWidth() const;
    qreal navCtrlBorderWidth() const;
    qreal accentOpacityNormal() const;
    qreal accentOpacityHover() const;
    qreal accentOpacityHit() const;

    qreal buttonOpacityNormal() const;
    qreal buttonOpacityHover() const;
    qreal buttonOpacityHit() const;

    qreal itemOpacityDisabled() const;

    int flickableMaxVelocity() const;

signals:
    void themeChanged();

private:

    void initThemeValues();

    void initUiFonts();
    void initIconsFont();
    void initMusicalFont();

    void setupUiFonts();
    void setupIconsFont();
    void setupMusicFont();

    void calculateDefaultButtonSize();

    void setupWidgetTheme();

    void notifyAboutThemeChanged();

    QFont m_bodyFont;
    QFont m_bodyBoldFont;
    QFont m_largeBodyFont;
    QFont m_largeBodyBoldFont;
    QFont m_tabFont;
    QFont m_tabBoldFont;
    QFont m_headerFont;
    QFont m_headerBoldFont;
    QFont m_titleBoldFont;
    QFont m_iconsFont;
    QFont m_toolbarIconsFont;
    QFont m_musicalFont;
    QFont m_defaultFont;

    QColor m_backgroundPrimaryColor;
    QColor m_backgroundSecondaryColor;
    QColor m_popupBackgroundColor;
    QColor m_textFieldColor;
    QColor m_accentColor;
    QColor m_strokeColor;
    QColor m_buttonColor;
    QColor m_fontPrimaryColor;
    QColor m_fontSecondaryColor;
    QColor m_linkColor;
    QColor m_focusColor;

    qreal m_defaultButtonSize = 0;
    qreal m_borderWidth = 0;
    qreal m_navCtrlBorderWidth = 0;
    qreal m_accentOpacityNormal = 0;
    qreal m_accentOpacityHover = 0;
    qreal m_accentOpacityHit = 0;
    qreal m_buttonOpacityNormal = 0;
    qreal m_buttonOpacityHover = 0;
    qreal m_buttonOpacityHit = 0;
    qreal m_itemOpacityDisabled = 0;

    ProxyStyle* m_style = nullptr;
};

class ProxyStyle : public QProxyStyle
{
public:
    ProxyStyle(ThemeApi* t);

    void polish(QWidget* widget) override;
    void unpolish(QWidget* widget) override;

    void drawPrimitive(PrimitiveElement element, const QStyleOption* option, QPainter* painter, const QWidget* widget) const override;
    void drawComplexControl(ComplexControl control, const QStyleOptionComplex* option, QPainter* painter,
                            const QWidget* widget = nullptr) const override;
    QRect subControlRect(QStyle::ComplexControl control, const QStyleOptionComplex* option, QStyle::SubControl subControl,
                         const QWidget* widget = nullptr) const override;
    int pixelMetric(PixelMetric metric, const QStyleOption* option, const QWidget* widget) const override;
    QSize sizeFromContents(ContentsType type, const QStyleOption* option, const QSize& contentsSize,
                           const QWidget* widget = nullptr) const override;
    QIcon standardIcon(QStyle::StandardPixmap standardIcon, const QStyleOption* option = nullptr,
                       const QWidget* widget = nullptr) const override;
    int styleHint(StyleHint hint, const QStyleOption* option = nullptr, const QWidget* widget = nullptr,
                  QStyleHintReturn* returnData = nullptr) const override;
private:
    struct StyleState {
        bool enabled = false;
        bool hovered = false;
        bool pressed = false;
        bool focused = false;
    };

    void drawButtonBackground(QPainter* painter, const QRect& rect, const StyleState& styleState, bool accentButton, bool flat,
                              const QColor& defaultBackground) const;
    void drawCheckboxIndicator(QPainter* painter, const QRect& rect, const StyleState& styleState, bool checked, bool indeterminate,
                               bool inMenu) const;
    void drawRadioButtonIndicator(QPainter* painter, const QRect& rect, const StyleState& styleState, bool selected) const;
    void drawLineEditBackground(QPainter* painter, const QRect& rect, const StyleState& styleState, bool editing) const;
    void drawIndicatorIcon(QPainter* painter, const QRect& rect, const StyleState& styleState, QStyle::PrimitiveElement element) const;
    void drawViewItemBackground(QPainter* painter, const QRect& rect, const StyleState& styleState, bool selected) const;
    void drawToolbarGrip(QPainter* painter, const QRect& rect, bool horizontal) const;

    ThemeApi* m_theme = nullptr;
};
}

#endif // MU_API_THEMEAPI_H
