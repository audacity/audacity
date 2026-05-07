/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effectstypes.h"

#include "framework/uicomponents/qml/Muse/UiComponents/internal/tableviewheader.h"

#include <QObject>

namespace au::effects {
class PluginManagerTableViewVerticalHeader : public muse::uicomponents::TableViewHeader
{
    Q_OBJECT
    QML_ELEMENT;

public:
    explicit PluginManagerTableViewVerticalHeader(QObject* parent = nullptr);

    const EffectId& effectId() const;
    void setEffectId(const EffectId& effectId);

private:
    EffectId m_effectId;
};
}
