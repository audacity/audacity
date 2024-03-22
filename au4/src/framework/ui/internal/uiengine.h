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

#ifndef MU_UI_UIENGINE_H
#define MU_UI_UIENGINE_H

#include <QObject>
#include <memory>

#include "../iuiengine.h"
#include "../api/themeapi.h"
#include "../view/qmltooltip.h"
#include "../view/qmltranslation.h"
#include "../view/interactiveprovider.h"
#include "../view/qmlapi.h"

#include "languages/ilanguagesservice.h"

class QQmlEngine;

namespace mu::ui {
class UiEngine : public QObject, public IUiEngine
{
    Q_OBJECT

    INJECT(languages::ILanguagesService, languagesService)

    Q_PROPERTY(api::ThemeApi * theme READ theme NOTIFY themeChanged)
    Q_PROPERTY(QmlToolTip * tooltip READ tooltip CONSTANT)

    Q_PROPERTY(QQuickItem * rootItem READ rootItem WRITE setRootItem NOTIFY rootItemChanged)

    // for internal use
    Q_PROPERTY(InteractiveProvider * _interactiveProvider READ interactiveProvider_property CONSTANT)

public:
    ~UiEngine() override;

    static UiEngine* instance();

    QmlApi* api() const;
    api::ThemeApi* theme() const;
    QmlToolTip* tooltip() const;
    InteractiveProvider* interactiveProvider_property() const;
    std::shared_ptr<InteractiveProvider> interactiveProvider() const;

    Q_INVOKABLE Qt::KeyboardModifiers keyboardModifiers() const;
    Q_INVOKABLE Qt::LayoutDirection currentLanguageLayoutDirection() const;

    // IUiEngine
    void updateTheme() override;
    QQmlEngine* qmlEngine() const override;
    void clearComponentCache() override;
    void addSourceImportPath(const QString& path) override;
    // ---

    void moveQQmlEngine(QQmlEngine* e);
    void quit();

    QQuickItem* rootItem() const;

public slots:
    void setRootItem(QQuickItem* rootItem);

signals:
    void themeChanged(api::ThemeApi* theme);
    void rootItemChanged(QQuickItem* rootItem);

private:
    UiEngine();

    QQmlEngine* engine();
    void setup(QQmlEngine* engine);

    QQmlEngine* m_engine = nullptr;
    QStringList m_sourceImportPaths;
    api::ThemeApi* m_theme = nullptr;
    QmlTranslation* m_translation = nullptr;
    std::shared_ptr<InteractiveProvider> m_interactiveProvider = nullptr;
    QmlApi* m_api = nullptr;
    QmlToolTip* m_tooltip = nullptr;
    QQuickItem* m_rootItem = nullptr;
};
}

#endif // MU_UI_UIENGINE_H
