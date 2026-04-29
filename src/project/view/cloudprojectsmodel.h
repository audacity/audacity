/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "abstractitemmodel.h"

#include "framework/global/async/asyncable.h"

#include "modularity/ioc.h"
#include "iprojectconfiguration.h"
#include "au3cloud/iau3audiocomservice.h"
#include "framework/interactive/iinteractive.h"

namespace au::project {
class CloudProjectsModel : public AbstractItemModel, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    muse::GlobalInject<au::project::IProjectConfiguration> configuration;
    muse::ContextInject<au::au3cloud::IAu3AudioComService> audioComService { this };
    muse::ContextInject<muse::IInteractive> interactive { this };

    Q_PROPERTY(State state READ state NOTIFY stateChanged)
    Q_PROPERTY(bool hasMore READ hasMore NOTIFY hasMoreChanged)

    Q_PROPERTY(int desiredRowCount READ desiredRowCount WRITE setDesiredRowCount NOTIFY desiredRowCountChanged)

public:
    CloudProjectsModel(QObject* parent = nullptr);

    enum class State {
        Fine,
        Loading,
        Error
    };
    Q_ENUM(State)

    void load() override;
    Q_INVOKABLE void reload();

    State state() const;
    bool hasMore() const;

    // Used by the view to request more items
    int desiredRowCount() const;
    void setDesiredRowCount(int count);

signals:
    void stateChanged();
    void hasMoreChanged();

    void desiredRowCountChanged();

private:
    void setState(State state);

    void loadItemsIfNecessary();
    bool needsLoading();

    State m_state = State::Fine;
    bool m_isWaitingForPromise = false;

    size_t m_totalItems = muse::nidx;

    int m_desiredRowCount = 0;

    uint64_t m_reloadGeneration = 0;
};
}
