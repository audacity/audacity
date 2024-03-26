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
#include "canvaspreferencesmodel.h"

#include "log.h"
#include "types/translatablestring.h"

using namespace au::appshell;
using namespace mu::notation;

CanvasPreferencesModel::CanvasPreferencesModel(QObject* parent)
    : QObject(parent)
{
}

void CanvasPreferencesModel::load()
{
    setupConnections();
}

QVariantList CanvasPreferencesModel::zoomTypes() const
{
    QVariantList types = {
        QVariantMap { { "title", zoomTypeTitle(ZoomType::Percentage).qTranslated() }, { "value", static_cast<int>(ZoomType::Percentage) } },
        QVariantMap { { "title", zoomTypeTitle(ZoomType::PageWidth).qTranslated() }, { "value", static_cast<int>(ZoomType::PageWidth) } },
        QVariantMap { { "title", zoomTypeTitle(ZoomType::WholePage).qTranslated() }, { "value", static_cast<int>(ZoomType::WholePage) } },
        QVariantMap { { "title", zoomTypeTitle(ZoomType::TwoPages).qTranslated() }, { "value", static_cast<int>(ZoomType::TwoPages) } }
    };

    return types;
}

QVariantMap CanvasPreferencesModel::defaultZoom() const
{
    QVariantMap zoom;
    ZoomType zoomType = defaultZoomType();
    zoom["type"] = static_cast<int>(zoomType);
    zoom["isPercentage"] = zoomType == ZoomType::Percentage;
    zoom["level"] = notationConfiguration()->defaultZoom();

    return zoom;
}

int CanvasPreferencesModel::mouseZoomPrecision() const
{
    return notationConfiguration()->mouseZoomPrecision();
}

int CanvasPreferencesModel::scrollPagesOrientation() const
{
    return static_cast<int>(notationConfiguration()->canvasOrientation().val);
}

bool CanvasPreferencesModel::limitScrollArea() const
{
    return notationConfiguration()->isLimitCanvasScrollArea();
}

int CanvasPreferencesModel::selectionProximity() const
{
    return notationConfiguration()->selectionProximity();
}

void CanvasPreferencesModel::setDefaultZoomType(int zoomType)
{
    ZoomType type = static_cast<ZoomType>(zoomType);
    if (defaultZoomType() == type) {
        return;
    }

    notationConfiguration()->setDefaultZoomType(type);
    emit defaultZoomChanged();
}

void CanvasPreferencesModel::setDefaultZoomLevel(int zoom)
{
    if (defaultZoomLevel() == zoom) {
        return;
    }

    notationConfiguration()->setDefaultZoom(zoom);
    emit defaultZoomChanged();
}

void CanvasPreferencesModel::setMouseZoomPrecision(int precision)
{
    if (mouseZoomPrecision() == precision) {
        return;
    }

    notationConfiguration()->setMouseZoomPrecision(precision);
    emit mouseZoomPrecisionChanged();
}

void CanvasPreferencesModel::setScrollPagesOrientation(int orientation)
{
    if (orientation == scrollPagesOrientation()) {
        return;
    }

    notationConfiguration()->setCanvasOrientation(static_cast<mu::Orientation>(orientation));
}

void CanvasPreferencesModel::setLimitScrollArea(bool limit)
{
    if (limitScrollArea() == limit) {
        return;
    }

    notationConfiguration()->setIsLimitCanvasScrollArea(limit);
    emit limitScrollAreaChanged();
}

void CanvasPreferencesModel::setSelectionProximity(int proximity)
{
    if (selectionProximity() == proximity) {
        return;
    }

    notationConfiguration()->setSelectionProximity(proximity);
    emit selectionProximityChanged(proximity);
}

void CanvasPreferencesModel::setupConnections()
{
    notationConfiguration()->canvasOrientation().ch.onReceive(this, [this](mu::Orientation) {
        emit scrollPagesOrientationChanged();
    });
}

ZoomType CanvasPreferencesModel::defaultZoomType() const
{
    return notationConfiguration()->defaultZoomType();
}

int CanvasPreferencesModel::defaultZoomLevel() const
{
    return notationConfiguration()->defaultZoom();
}
