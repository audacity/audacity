/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>

#include <QAbstractListModel>
#include <QtQml/qqmlregistration.h>

#include "modularity/ioc.h"
#include "async/asyncable.h"
#include "interactive/iinteractive.h"

#include "macromanager/imacrorunner.h"
#include "macromanager/imacrosrepository.h"

#include "runonfileslauncher.h"

namespace au::macromanager {
class MacroManagerModel : public QAbstractListModel, public muse::Contextable, public muse::async::Asyncable
{
    Q_OBJECT
    QML_ELEMENT;

    Q_PROPERTY(bool isEmpty READ isEmpty NOTIFY isEmptyChanged FINAL)

    muse::GlobalInject<IMacrosRepository> repository;

    muse::ContextInject<IMacroRunner> runner = { this };
    muse::ContextInject<muse::IInteractive> interactive = { this };

public:
    explicit MacroManagerModel(QObject* parent = nullptr);

    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    bool isEmpty() const;

    Q_INVOKABLE void init();
    Q_INVOKABLE void newMacro();
    Q_INVOKABLE void edit(int row);
    Q_INVOKABLE void rename(int row);
    Q_INVOKABLE void remove(int row);
    Q_INVOKABLE void run(int row);
    Q_INVOKABLE void runOnFiles(int row);

signals:
    void isEmptyChanged();

private:
    enum Roles {
        TitleRole = Qt::UserRole + 1,
        IsBuiltinRole
    };

    bool isRowValid(int row) const;
    void reload();
    void openEditor(const std::string& name);
    std::optional<QString> requestMacroName(const muse::UriQuery& uri) const;

    MacroList m_macros;
    RunOnFilesLauncher m_runOnFilesLauncher;
};
}
