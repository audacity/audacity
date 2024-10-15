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
#ifndef AU_APPSHELL_CANVASPREFERENCESMODEL_H
#define AU_APPSHELL_CANVASPREFERENCESMODEL_H

#include <QObject>

#include "modularity/ioc.h"
#include "async/asyncable.h"

#include "notation/inotationconfiguration.h"

namespace au::appshell {
class CanvasPreferencesModel : public QObject, public async::Asyncable
{
    Q_OBJECT

    INJECT(notation::INotationConfiguration, notationConfiguration)

    Q_PROPERTY(QVariantMap defaultZoom READ defaultZoom NOTIFY defaultZoomChanged)
    Q_PROPERTY(int mouseZoomPrecision READ mouseZoomPrecision WRITE setMouseZoomPrecision NOTIFY mouseZoomPrecisionChanged)

    Q_PROPERTY(int scrollPagesOrientation READ scrollPagesOrientation WRITE setScrollPagesOrientation NOTIFY scrollPagesOrientationChanged)
    Q_PROPERTY(bool limitScrollArea READ limitScrollArea WRITE setLimitScrollArea NOTIFY limitScrollAreaChanged)

    Q_PROPERTY(int selectionProximity READ selectionProximity WRITE setSelectionProximity NOTIFY selectionProximityChanged)

public:
    explicit CanvasPreferencesModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();
    Q_INVOKABLE QVariantList zoomTypes() const;

    QVariantMap defaultZoom() const;
    Q_INVOKABLE void setDefaultZoomType(int zoomType);
    Q_INVOKABLE void setDefaultZoomLevel(int zoom);

    int mouseZoomPrecision() const;

    int scrollPagesOrientation() const;
    bool limitScrollArea() const;

    int selectionProximity() const;

public slots:
    void setMouseZoomPrecision(int precision);

    void setScrollPagesOrientation(int orientation);
    void setLimitScrollArea(bool limit);

    void setSelectionProximity(int proximity);

signals:
    void defaultZoomChanged();
    void mouseZoomPrecisionChanged();
    void scrollPagesOrientationChanged();
    void limitScrollAreaChanged();
    void selectionProximityChanged(int selectionProximity);

private:
    void setupConnections();

    notation::ZoomType defaultZoomType() const;
    int defaultZoomLevel() const;
};
}

#endif // AU_APPSHELL_CANVASPREFERENCESMODEL_H
