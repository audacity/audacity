/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <QObject>

#include "async/asyncable.h"

#include "modularity/ioc.h"

#include "projectscene/iprojectsceneactionscontroller.h"

namespace au::projectscene {
class RealtimeEffectRowActionsController : public QObject, public muse::async::Asyncable, public muse::Injectable
{
    Q_OBJECT

    Q_PROPERTY(bool enabled READ enabled WRITE setEnabled NOTIFY enabledChanged FINAL)

    muse::ContextInject<IProjectSceneActionsController> projectSceneActionsController{ this };

public:
    explicit RealtimeEffectRowActionsController(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    bool enabled() const;
    void setEnabled(bool enabled);

signals:
    void enabledChanged();
    void moveUpRequested();
    void moveDownRequested();

private:
    bool m_enabled = false;
    bool m_initialized = false;
};
}
