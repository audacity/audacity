/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QAbstractListModel>
#include <QVariantList>

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include "context/iglobalcontext.h"
#include "trackedit/itrackeditinteraction.h"

namespace au::projectscene {
//! What a track sends to busses besides its output
class TrackSendsModel : public QAbstractListModel, public muse::Contextable, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(QVariant trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged)

    muse::ContextInject<context::IGlobalContext> globalContext{ this };
    muse::ContextInject<trackedit::ITrackeditInteraction> trackeditInteraction{ this };

public:
    explicit TrackSendsModel(QObject* parent = nullptr);

    QVariant trackId() const;
    void setTrackId(const QVariant& trackId);

    Q_INVOKABLE void load();
    //! The busses, those it can't send to disabled: it sends to them already, or they would feed back
    Q_INVOKABLE QVariantList addMenu() const;
    //! @param busId as `addMenu` gives it
    Q_INVOKABLE void addSend(const QString& busId);
    Q_INVOKABLE void removeSend(int row);
    Q_INVOKABLE void setLevel(int row, float level, bool completed);
    Q_INVOKABLE void setPreFader(int row, bool preFader);

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

signals:
    void trackIdChanged();

private:
    enum Roles {
        TitleRole = Qt::UserRole + 1,
        LevelRole,
        PreFaderRole,
    };

    QString busTitle(trackedit::TrackId busId) const;

    trackedit::TrackId m_trackId = trackedit::INVALID_TRACK;
    trackedit::TrackSendList m_sends;
};
}
