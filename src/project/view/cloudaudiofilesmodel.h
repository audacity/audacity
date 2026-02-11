/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "abstractprojectsmodel.h"

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "iprojectconfiguration.h"
#include "au3cloud/iau3audiocomservice.h"
#include "au3cloud/iauthorization.h"

namespace au::project {
class CloudAudioFilesModel : public AbstractProjectsModel, public muse::async::Asyncable, public muse::Injectable
{
    Q_OBJECT

    muse::GlobalInject<au::project::IProjectConfiguration> configuration;

    muse::Inject<au::au3cloud::IAu3AudioComService> audioComService { this };
    muse::Inject<au::au3cloud::IAuthorization> authorization { this };

    Q_PROPERTY(State state READ state NOTIFY stateChanged)
    Q_PROPERTY(bool hasMore READ hasMore NOTIFY hasMoreChanged)

    Q_PROPERTY(int desiredRowCount READ desiredRowCount WRITE setDesiredRowCount NOTIFY desiredRowCountChanged)

public:
    CloudAudioFilesModel(QObject* parent = nullptr);

    enum class State {
        Fine,
        Loading,
        NotSignedIn,
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
};
}
