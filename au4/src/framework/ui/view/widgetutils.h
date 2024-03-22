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
#ifndef MU_UI_WIDGETUTILS_H
#define MU_UI_WIDGETUTILS_H

#include <type_traits>

#include "iconcodes.h"

#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"

class QToolButton;
class QWidget;

namespace mu::ui {
class WidgetUtils
{
    INJECT_STATIC(IUiConfiguration, uiConfiguration)

public:
    template<class W>
    static inline std::enable_if_t<std::is_base_of_v<QWidget, W>, void>
    setWidgetIcon(W* widget, IconCode::Code iconCode)
    {
        QChar icon = iconCodeToChar(iconCode);
        QString styleSheet = QString("font-family: %1; font-size: %2px")
                             .arg(QString::fromStdString(uiConfiguration()->iconsFontFamily()))
                             .arg(uiConfiguration()->iconsFontSize(IconSizeType::Regular));
        widget->setStyleSheet(styleSheet);
        widget->setText(icon);

        if constexpr (std::is_same_v<W, QToolButton>) {
            widget->setFixedSize(30, 30);
        }
    }
};
}

#endif // MU_UI_WIDGETUTILS_H
