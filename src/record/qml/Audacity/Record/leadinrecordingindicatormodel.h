/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QVariantList>
#include <QtQml/qqmlregistration.h>

#include "async/asyncable.h"
#include "modularity/ioc.h"

#include "record/irecordcontroller.h"

namespace au::record {
class LeadInRecordingIndicatorModel : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT
    QML_ELEMENT

public:
    Q_PROPERTY(bool visible READ visible NOTIFY visibleChanged FINAL)
    Q_PROPERTY(double startTime READ startTime NOTIFY visibleChanged FINAL)
    Q_PROPERTY(QVariantList trackIds READ trackIds NOTIFY visibleChanged FINAL)

    muse::ContextInject<IRecordController> recordController{ this };

    explicit LeadInRecordingIndicatorModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    bool visible() const;
    double startTime() const;
    QVariantList trackIds() const;

signals:
    void visibleChanged();
};
}
