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

#include "radiobuttongroupbox.h"

#include <QStyle>
#include <QStyleOption>
#include <QStylePainter>

using namespace mu::uicomponents;

RadioButtonGroupBox::RadioButtonGroupBox(QWidget* parent)
    : QGroupBox(parent)
{
}

RadioButtonGroupBox::RadioButtonGroupBox(const QString& title, QWidget* parent)
    : QGroupBox(title, parent)
{
}

void RadioButtonGroupBox::paintEvent(QPaintEvent*)
{
    QStylePainter painter(this);
    QStyleOptionGroupBox styleOption;
    initStyleOption(&styleOption);

    // Paint the default QGroupBox-style control, which includes an unwanted checkbox that we'll cover up afterwards.
    painter.drawComplexControl(QStyle::CC_GroupBox, styleOption);

    // Calculate the background color the same way Qt does in QFusionStylePrivate::tabFrameColor().
    const QColor& buttonColor = styleOption.palette.button().color();
    QColor bgColor = buttonColor.lighter(100 + std::max(1, (180 - qGray(buttonColor.rgb())) / 6));
    bgColor.setHsv(bgColor.hue(), 3 * bgColor.saturation() / 4, bgColor.value());
    bgColor = bgColor.lighter(104);

    // Adjust the style options to use the checkbox's rectangle.
    styleOption.rect = style()->subControlRect(QStyle::CC_GroupBox, &styleOption, QStyle::SC_GroupBoxCheckBox, this);

    // Cover up the checkbox, making sure to enlarge the rectangle a bit to cover up any anti-aliasing around the edges.
    painter.fillRect(styleOption.rect.adjusted(-2, -2, 2, 2), bgColor);

    // Paint the radio button.
    painter.drawPrimitive(QStyle::PE_IndicatorRadioButton, styleOption);
}
