/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>
#include <QQmlParserStatus>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "trackedit/itrackeditinteraction.h"
#include "trackedit/iprojecthistory.h"

namespace au::projectscene {
class HistoryPanelModel : public QAbstractListModel, public QQmlParserStatus, public muse::Contextable, public muse::async::Asyncable
{
    Q_OBJECT
    Q_INTERFACES(QQmlParserStatus)

    Q_PROPERTY(int currentIndex READ currentIndex NOTIFY currentIndexChanged)

    muse::ContextInject<context::IGlobalContext> context = { this };
    muse::ContextInject<trackedit::ITrackeditInteraction> trackeditInteraction = { this };
    muse::ContextInject<trackedit::IProjectHistory> projectHistory = { this };

public:
    explicit HistoryPanelModel(QObject* parent = nullptr);

    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    int currentIndex() const;

    Q_INVOKABLE void undoRedoToIndex(int index);

signals:
    void currentIndexChanged();

private:
    void classBegin() override;
    void componentComplete() override {}

    bool isProjectLoaded() const;
    void onCurrentProjectChanged();

    void onHistoryEvent();

    void updateCurrentIndex();

    int m_rowCount = 0;
};
}
