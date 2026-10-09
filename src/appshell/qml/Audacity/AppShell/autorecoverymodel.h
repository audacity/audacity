/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>
#include <QQmlParserStatus>
#include <qqmlintegration.h>

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"
#include "framework/uicomponents/qml/Muse/UiComponents/abstracttableviewmodel.h"

#include "appshell/internal/isessionsmanager.h"

namespace au::appshell {
namespace AutoRecoveryTableViewCellType {
Q_NAMESPACE;
QML_ELEMENT;

enum class Type {
    Selected = static_cast<int>(muse::uicomponents::TableViewCellType::Type::UserType) + 1,
};

Q_ENUM_NS(Type)
}

class AutoRecoveryModel : public muse::uicomponents::AbstractTableViewModel, public QQmlParserStatus, public muse::Contextable
{
    Q_OBJECT
    Q_INTERFACES(QQmlParserStatus)
    QML_ELEMENT

    Q_PROPERTY(bool hasSelection READ hasSelection NOTIFY selectionChanged);

    muse::ContextInject<ISessionsManager> sessionsManager { this };
    muse::ContextInject<muse::IInteractive> interactive { this };

public:
    explicit AutoRecoveryModel(QObject* parent = nullptr);

    bool hasSelection() const;

    Q_INVOKABLE void toggleSelected(int row);
    Q_INVOKABLE void invertSelection();
    Q_INVOKABLE void discardSelected();
    //! NOTE Recovers on a later event loop iteration, once the dialog is closed
    Q_INVOKABLE void recoverSelectedAsync();

signals:
    void selectionChanged();
    void allProjectsDiscarded();

private:
    static constexpr int s_selectedColumn = 0;

    void classBegin() override {}
    void componentComplete() override;

    void rebuildTable();
    muse::io::paths_t selectedProjects() const;
    void setSelected(int row, bool selected);

    struct Item {
        muse::io::path_t path;
        bool selected = true;
    };

    std::vector<Item> m_items;
};
}
