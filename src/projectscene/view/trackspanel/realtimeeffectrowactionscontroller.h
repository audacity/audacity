/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <QObject>

#include "actions/actionable.h"
#include "async/asyncable.h"

#include "modularity/ioc.h"

namespace au::projectscene {
class RealtimeEffectRowActionsController : public QObject, public muse::actions::Actionable, public muse::async::Asyncable,
    public muse::Injectable
{
    Q_OBJECT

    Q_PROPERTY(bool enabled READ enabled WRITE setEnabled NOTIFY enabledChanged FINAL)

    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher{ this };

public:
    explicit RealtimeEffectRowActionsController(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    bool enabled() const;
    void setEnabled(bool enabled);

    bool canReceiveAction(const muse::actions::ActionCode& code) const override;

signals:
    void enabledChanged();
    void moveUpRequested();
    void moveDownRequested();

private:
    bool m_enabled = false;
    bool m_initialized = false;
};
}
