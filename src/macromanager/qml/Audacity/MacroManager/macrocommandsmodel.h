/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <functional>

#include <QAbstractListModel>
#include <QtQml/qqmlregistration.h>

#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"
#include "framework/rcommand/icommanddispatcher.h"
#include "framework/rcommand/icommandsregister.h"
#include "macromanager/imacrorunner.h"
#include "macromanager/imacrosrepository.h"

#include "macromanager/macromanagertypes.h"
#include "macroactions.h"
#include "runonfileslauncher.h"

namespace au::macromanager {
class MacroCommandsModel : public QAbstractListModel, public muse::Contextable, public muse::async::Asyncable
{
    Q_OBJECT
    QML_ELEMENT;

    Q_PROPERTY(QString name READ name NOTIFY nameChanged FINAL)
    Q_PROPERTY(bool isBuiltin READ isBuiltin NOTIFY nameChanged FINAL)
    Q_PROPERTY(bool isEmpty READ isEmpty NOTIFY isEmptyChanged FINAL)
    Q_PROPERTY(bool isRunning READ isRunning NOTIFY isRunningChanged FINAL)

    muse::GlobalInject<IMacrosRepository> repository;
    muse::GlobalInject<muse::rcommand::ICommandsRegister> commandsRegister;

    muse::ContextInject<IMacroRunner> runner = { this };
    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher = { this };
    muse::ContextInject<muse::IInteractive> interactive = { this };

public:
    explicit MacroCommandsModel(QObject* parent = nullptr);

    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    QString name() const;
    bool isBuiltin() const;
    bool isEmpty() const;
    bool isRunning() const;

    Q_INVOKABLE void load(const QString& macroName);
    Q_INVOKABLE void add(const QString& code);
    Q_INVOKABLE void insert(int row, const QString& code);
    Q_INVOKABLE void addMacro(const QString& name);
    Q_INVOKABLE void insertMacro(int row, const QString& name);
    Q_INVOKABLE void move(int from, int to);
    Q_INVOKABLE void remove(int row);
    Q_INVOKABLE void removeAllSteps();
    Q_INVOKABLE void editParams(int row);
    Q_INVOKABLE bool save();
    Q_INVOKABLE void run();
    Q_INVOKABLE void runOnFiles();
    Q_INVOKABLE void rename();
    Q_INVOKABLE void exportMacro();
    Q_INVOKABLE void deleteMacro();

signals:
    void nameChanged();
    void isEmptyChanged();
    void isRunningChanged();
    void macroDeleted();

private:
    enum Roles {
        StepRole = Qt::UserRole + 1,
        CodeRole,
        TitleRole,
        HasParamsRole,
        IsMacroRole
    };

    using OnParams = std::function<void (const muse::rcommand::Params&)>;

    bool isRowValid(int row) const;
    QString titleOf(const muse::rcommand::Command& command) const;
    bool hasParams(const muse::rcommand::Command& command) const;
    void notifyStepsChanged(int fromRow, int toRow);
    void insertStep(int row, const MacroItem& item);
    void setStepParams(int row, const muse::rcommand::Params& params);
    void requestParams(const muse::rcommand::Command& command, const muse::rcommand::Params& current, const OnParams& onParams);
    void requestEffectParams(const muse::rcommand::Command& command, const muse::rcommand::Params& current, const OnParams& onParams);
    void requestSchemaParams(const muse::rcommand::Command& command, const muse::rcommand::Params& current, const OnParams& onParams);

    Macro m_macro;
    MacroActions m_actions;
    RunOnFilesLauncher m_runOnFilesLauncher;
};
}
