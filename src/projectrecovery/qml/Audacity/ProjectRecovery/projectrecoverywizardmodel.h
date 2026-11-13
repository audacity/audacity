/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QFutureWatcher>
#include <QObject>
#include <QVariant>
#include <qqmlintegration.h>

#include <optional>

#include "framework/global/io/ifilesystem.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/types/retval.h"
#include "framework/multiwindows/imultiwindowsprovider.h"
#include "framework/rcommand/icommanddispatcher.h"

#include "project/iprojectconfiguration.h"

#include "projectrecovery/iprojectrecoveryservice.h"

namespace au::projectrecovery {
//! The recovery wizard: select a file, analyze it, choose a method, run it. The original file is never written.
class ProjectRecoveryWizardModel : public QObject, public muse::Contextable
{
    Q_OBJECT
    QML_ELEMENT

    Q_PROPERTY(QString projectPath READ projectPath WRITE setProjectPath NOTIFY projectPathChanged)
    Q_PROPERTY(QString fileName READ fileName NOTIFY projectPathChanged)
    Q_PROPERTY(QString projectsDir READ projectsDir CONSTANT)

    Q_PROPERTY(Stage stage READ stage NOTIFY stageChanged)
    Q_PROPERTY(int step READ step NOTIFY stageChanged)
    Q_PROPERTY(int stepCount READ stepCount CONSTANT)
    Q_PROPERTY(bool canGoNext READ canGoNext NOTIFY stageChanged)
    Q_PROPERTY(bool projectOpen READ projectOpen NOTIFY stageChanged)
    Q_PROPERTY(QString errorText READ errorText NOTIFY stageChanged)

    Q_PROPERTY(bool hasDocument READ hasDocument NOTIFY stageChanged)
    Q_PROPERTY(QString projectVersion READ projectVersion NOTIFY stageChanged)
    Q_PROPERTY(int sampleRate READ sampleRate NOTIFY stageChanged)
    Q_PROPERTY(int trackCount READ trackCount NOTIFY stageChanged)
    Q_PROPERTY(int clipCount READ clipCount NOTIFY stageChanged)
    Q_PROPERTY(qint64 fileSize READ fileSize NOTIFY stageChanged)
    Q_PROPERTY(Integrity integrity READ integrity NOTIFY stageChanged)
    Q_PROPERTY(bool hasAutosave READ hasAutosave NOTIFY stageChanged)
    Q_PROPERTY(qint64 blockRowCount READ blockRowCount NOTIFY stageChanged)
    Q_PROPERTY(int missingBlockCount READ missingBlockCount NOTIFY stageChanged)
    Q_PROPERTY(qint64 orphanBlockCount READ orphanBlockCount NOTIFY stageChanged)
    Q_PROPERTY(QVariantList missingBlocks READ missingBlocks NOTIFY stageChanged)
    Q_PROPERTY(QVariantList availableMethods READ availableMethods NOTIFY stageChanged)
    Q_PROPERTY(Method method READ method NOTIFY stageChanged)
    Q_PROPERTY(QString outputPath READ outputPath NOTIFY stageChanged)

    muse::GlobalInject<project::IProjectConfiguration> projectConfiguration;
    muse::GlobalInject<muse::io::IFileSystem> fileSystem;
    muse::GlobalInject<muse::mi::IMultiWindowsProvider> multiwindowsProvider;
    muse::GlobalInject<IProjectRecoveryService> recoveryService;
    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher { this };

public:
    enum class Stage {
        SelectProject,
        Analyzing,
        Results,
        ChooseMethod,
        MethodDetails,
        Recovering,
        Finished,
    };
    Q_ENUM(Stage)

    enum class Method {
        Lossless,
        Lossy,
        Database,
    };
    Q_ENUM(Method)

    enum class Integrity {
        NotChecked,
        Passed,
        Failed,
    };
    Q_ENUM(Integrity)

    explicit ProjectRecoveryWizardModel(QObject* parent = nullptr);

    QString projectPath() const;
    void setProjectPath(const QString& path);
    QString fileName() const;
    QString projectsDir() const;

    Stage stage() const;
    //! 1 to stepCount; 0 on the pages that show progress
    int step() const;
    int stepCount() const;
    bool canGoNext() const;
    bool projectOpen() const;
    QString errorText() const;

    bool hasDocument() const;
    QString projectVersion() const;
    int sampleRate() const;
    int trackCount() const;
    int clipCount() const;
    qint64 fileSize() const;
    Integrity integrity() const;
    bool hasAutosave() const;
    qint64 blockRowCount() const;
    int missingBlockCount() const;
    qint64 orphanBlockCount() const;
    //! Each item has track, clip, start and end in seconds
    QVariantList missingBlocks() const;
    QVariantList availableMethods() const;
    Method method() const;
    QString outputPath() const;

    Q_INVOKABLE void next();
    Q_INVOKABLE void back();
    Q_INVOKABLE void selectMethod(int method);
    Q_INVOKABLE void runRecovery();
    Q_INVOKABLE void openResult();

signals:
    void projectPathChanged();
    void stageChanged();

private:
    void onInspected();
    void onRepaired();
    void setStage(Stage stage);
    bool canRepair() const;
    const DocumentInfo* activeDocument() const;
    muse::io::path_t makeOutputPath() const;

    QString m_projectPath;
    Stage m_stage = Stage::SelectProject;
    bool m_projectOpen = false;
    Method m_method = Method::Lossy;
    QString m_error;
    QString m_outputPath;
    std::optional<Inspection> m_inspection;

    // A result that comes after the dialog closes goes nowhere: the watchers go with the model
    QFutureWatcher<muse::RetVal<Inspection> > m_inspectionWatcher;
    QFutureWatcher<muse::RetVal<RepairReport> > m_repairWatcher;
};
}
