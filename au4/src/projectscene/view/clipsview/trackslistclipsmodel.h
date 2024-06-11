/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "iprojectsceneconfiguration.h"
#include "processing/iprocessingselectioncontroller.h"

#include "processing/dom/track.h"

namespace au::projectscene {
class TracksListClipsModel : public QAbstractListModel, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(bool isVerticalRulersVisible READ isVerticalRulersVisible NOTIFY isVerticalRulersVisibleChanged)

    //! NOTE Can be changed from Qml, directly during selection
    //! Or via selection controller (if selection was not made from view)
    Q_PROPERTY(QList<int> dataSelectedTracks READ dataSelectedTracks WRITE setDataSelectedTracks NOTIFY dataSelectedTracksChanged FINAL)

    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<IProjectSceneConfiguration> configuration;
    muse::Inject<processing::IProcessingSelectionController> processingSelectionController;

public:

    TracksListClipsModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();

    int rowCount(const QModelIndex& parent) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

    bool isVerticalRulersVisible() const;

    QList<int> dataSelectedTracks() const;
    void setDataSelectedTracks(const QList<int>& newDataSelectedTracks);

signals:
    void dataSelectedTracksChanged();
    void isVerticalRulersVisibleChanged(bool isVerticalRulersVisible);

private:
    void setIsVerticalRulersVisible(bool isVerticalRulersVisible);

    enum RoleNames {
        TypeRole = Qt::UserRole + 1,
        TrackIdRole,
        IsDataSelectedRole
    };

    void setDataSelectedTracks(const std::vector<processing::TrackId>& tracks);

    muse::async::NotifyList<au::processing::Track> m_trackList;
    QList<int> m_dataSelectedTracks;
    bool m_isVerticalRulersVisible = false;
};
}
