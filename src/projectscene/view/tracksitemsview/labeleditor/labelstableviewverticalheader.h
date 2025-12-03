/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <qqmlintegration.h>

#include <QObject>

#include "uicomponents/qml/Muse/UiComponents/internal/tableviewheader.h"

#include "types/projectscenetypes.h"

namespace au::projectscene {
class LabelsTableViewVerticalHeader : public muse::uicomponents::TableViewHeader
{
    Q_OBJECT
    QML_ELEMENT;

public:
    explicit LabelsTableViewVerticalHeader(QObject* parent = nullptr);

    LabelKey labelKey() const;
    void setLabelKey(const LabelKey& labelKey);

private:
    LabelKey m_labelKey;
};
}
