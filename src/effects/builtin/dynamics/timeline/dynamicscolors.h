/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QColor>
#include <QObject>
#include <QQmlEngine>

namespace au::effects {
class DynamicsColors : public QObject
{
    Q_OBJECT
    QML_ELEMENT
    QML_SINGLETON

public:
    static QColor backgroundColor() { return QColor("#313147"); }
    static QColor gridColor() { return QColor("#4A4D5D"); }
    static QColor clippingColor() { return QColor("#FF1C1C"); } // Same as color used in VolumePressureMeter.qml.

    static QColor timelineDataFillColor() { return QColor("#565695"); }
    static QColor timelineDataFillColorSemiTransparent() { return QColor("#80565695"); }

    static QColor timelineCompressionDbColor() { return QColor("#FFD12C"); }
    static QColor timelineCompressionDbColorSemiTransparent() { return QColor("#80FFD12C"); }

    static QColor timelineOutputDbLineColor() { return QColor("white"); }

    Q_PROPERTY(QColor backgroundColor READ backgroundColor CONSTANT)
    Q_PROPERTY(QColor gridColor READ gridColor CONSTANT)
    Q_PROPERTY(QColor clippingColor READ clippingColor CONSTANT)

    Q_PROPERTY(QColor timelineDataFillColor READ timelineDataFillColor CONSTANT)
    Q_PROPERTY(QColor timelineDataFillColorSemiTransparent READ timelineDataFillColorSemiTransparent CONSTANT)

    Q_PROPERTY(QColor timelineCompressionDbColor READ timelineCompressionDbColor CONSTANT)
    Q_PROPERTY(QColor timelineCompressionDbColorSemiTransparent READ timelineCompressionDbColorSemiTransparent CONSTANT)

    Q_PROPERTY(QColor timelineOutputDbLineColor READ timelineOutputDbLineColor CONSTANT)
};
}
