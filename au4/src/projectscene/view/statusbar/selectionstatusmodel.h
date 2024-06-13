/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "processing/iselectioncontroller.h"

namespace au::projectscene {
class SelectionStatusModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(double startTime READ startTime NOTIFY startTimeChanged FINAL)
    Q_PROPERTY(double endTime READ endTime NOTIFY endTimeChanged FINAL)

    muse::Inject<processing::ISelectionController> selectionController;

public:
    SelectionStatusModel();

    double startTime() const;
    double endTime() const;

    Q_INVOKABLE void init();

signals:
    void startTimeChanged();
    void endTimeChanged();

private:

    double m_startTime = -1.0;
    double m_endTime = -1.0;
};
}
