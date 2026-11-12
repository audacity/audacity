/*
 * Audacity: A Digital Audio Editor
 */
#include "projectrecoverywizardmodel.h"

#include <cmath>

#include "framework/global/translation.h"
#include "framework/global/types/val.h"

#include "project/projectcommands.h"
#include "project/types/projecttypes.h"

using namespace au::projectrecovery;

namespace {
constexpr int STEP_COUNT = 5;
}

ProjectRecoveryWizardModel::ProjectRecoveryWizardModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
    connect(&m_inspectionWatcher, &QFutureWatcherBase::finished, this, &ProjectRecoveryWizardModel::onInspected);
    connect(&m_repairWatcher, &QFutureWatcherBase::finished, this, &ProjectRecoveryWizardModel::onRepaired);
}

QString ProjectRecoveryWizardModel::projectPath() const
{
    return m_projectPath;
}

void ProjectRecoveryWizardModel::setProjectPath(const QString& path)
{
    if (m_projectPath == path) {
        return;
    }
    m_projectPath = path;
    m_projectOpen = false;
    emit projectPathChanged();
    emit stageChanged();
}

QString ProjectRecoveryWizardModel::fileName() const
{
    return muse::io::filename(muse::io::path_t(m_projectPath)).toQString();
}

QString ProjectRecoveryWizardModel::projectsDir() const
{
    muse::io::path_t dir = projectConfiguration()->lastOpenedProjectsPath();
    if (dir.empty()) {
        dir = projectConfiguration()->userProjectsPath();
    }
    return dir.toQString();
}

ProjectRecoveryWizardModel::Stage ProjectRecoveryWizardModel::stage() const
{
    return m_stage;
}

int ProjectRecoveryWizardModel::step() const
{
    switch (m_stage) {
    case Stage::SelectProject:
        return 1;
    case Stage::Results:
        return 2;
    case Stage::ChooseMethod:
        return 3;
    case Stage::MethodDetails:
        return 4;
    case Stage::Finished:
        return 5;
    case Stage::Analyzing:
    case Stage::Recovering:
        break;
    }
    return 0;
}

int ProjectRecoveryWizardModel::stepCount() const
{
    return STEP_COUNT;
}

bool ProjectRecoveryWizardModel::canGoNext() const
{
    switch (m_stage) {
    case Stage::SelectProject:
        return !m_projectPath.isEmpty();
    case Stage::Results:
        return m_inspection.has_value();
    default:
        return false;
    }
}

bool ProjectRecoveryWizardModel::projectOpen() const
{
    return m_projectOpen;
}

QString ProjectRecoveryWizardModel::errorText() const
{
    return m_error;
}

const DocumentInfo* ProjectRecoveryWizardModel::activeDocument() const
{
    if (!m_inspection) {
        return nullptr;
    }
    for (const DocumentInfo& document : m_inspection->documents) {
        if (document.source == m_inspection->activeDocument && document.readable) {
            return &document;
        }
    }
    return nullptr;
}

bool ProjectRecoveryWizardModel::hasDocument() const
{
    return activeDocument() != nullptr;
}

QString ProjectRecoveryWizardModel::projectVersion() const
{
    const DocumentInfo* document = activeDocument();
    return document ? QString::fromStdString(document->audacityVersion) : QString();
}

int ProjectRecoveryWizardModel::sampleRate() const
{
    const DocumentInfo* document = activeDocument();
    return document ? static_cast<int>(std::llround(document->rate)) : 0;
}

int ProjectRecoveryWizardModel::trackCount() const
{
    const DocumentInfo* document = activeDocument();
    return document ? static_cast<int>(document->waveTracks) : 0;
}

int ProjectRecoveryWizardModel::clipCount() const
{
    const DocumentInfo* document = activeDocument();
    return document ? static_cast<int>(document->clips) : 0;
}

qint64 ProjectRecoveryWizardModel::fileSize() const
{
    return m_inspection ? static_cast<qint64>(m_inspection->database.fileSize) : 0;
}

ProjectRecoveryWizardModel::Integrity ProjectRecoveryWizardModel::integrity() const
{
    if (!m_inspection || !m_inspection->database.integrityChecked) {
        return Integrity::NotChecked;
    }
    return m_inspection->database.integrityMessages.empty() ? Integrity::Passed : Integrity::Failed;
}

bool ProjectRecoveryWizardModel::hasAutosave() const
{
    if (!m_inspection) {
        return false;
    }
    for (const DocumentInfo& document : m_inspection->documents) {
        if (document.source == "autosave") {
            return true;
        }
    }
    return false;
}

qint64 ProjectRecoveryWizardModel::blockRowCount() const
{
    return m_inspection ? static_cast<qint64>(m_inspection->database.blockRows) : 0;
}

int ProjectRecoveryWizardModel::missingBlockCount() const
{
    const DocumentInfo* document = activeDocument();
    return document ? static_cast<int>(document->missingBlocks) : 0;
}

qint64 ProjectRecoveryWizardModel::orphanBlockCount() const
{
    return m_inspection ? static_cast<qint64>(m_inspection->database.orphanBlocks) : 0;
}

QVariantList ProjectRecoveryWizardModel::missingBlocks() const
{
    QVariantList blocks;
    if (!m_inspection) {
        return blocks;
    }
    for (const MissingBlock& block : m_inspection->missingBlocks) {
        blocks << QVariantMap { { "track", QString::fromStdString(block.track) }, { "clip", QString::fromStdString(block.clip) },
            { "start", block.startSeconds }, { "end", block.endSeconds } };
    }
    return blocks;
}

QVariantList ProjectRecoveryWizardModel::availableMethods() const
{
    QVariantList methods;
    if (canRepair()) {
        methods << static_cast<int>(Method::Lossy);
    }
    return methods;
}

ProjectRecoveryWizardModel::Method ProjectRecoveryWizardModel::method() const
{
    return m_method;
}

QString ProjectRecoveryWizardModel::outputPath() const
{
    return m_outputPath;
}

bool ProjectRecoveryWizardModel::canRepair() const
{
    if (!m_inspection) {
        return false;
    }
    const std::vector<Problem>& problems = m_inspection->problems;
    return problems.size() == 1 && problems.front() == Problem::MissingBlocks;
}

void ProjectRecoveryWizardModel::next()
{
    if (!canGoNext()) {
        return;
    }
    if (m_stage == Stage::Results) {
        setStage(Stage::ChooseMethod);
        return;
    }

    // Double check that the project is not open
    m_projectOpen = multiwindowsProvider()->isProjectAlreadyOpened(muse::io::path_t(m_projectPath));
    if (m_projectOpen) {
        emit stageChanged();
        return;
    }

    m_inspection.reset();
    m_error.clear();
    setStage(Stage::Analyzing);
    const muse::io::path_t path(m_projectPath);
    m_inspectionWatcher.setFuture(recoveryService()->inspect(path));
}

void ProjectRecoveryWizardModel::back()
{
    switch (m_stage) {
    case Stage::Results:
        setStage(Stage::SelectProject);
        break;
    case Stage::ChooseMethod:
        setStage(Stage::Results);
        break;
    case Stage::MethodDetails:
        setStage(Stage::ChooseMethod);
        break;
    default:
        break;
    }
}

void ProjectRecoveryWizardModel::selectMethod(int method)
{
    if (m_stage != Stage::ChooseMethod || static_cast<Method>(method) != Method::Lossy || !canRepair()) {
        return;
    }
    m_method = Method::Lossy;
    setStage(Stage::MethodDetails);
}

void ProjectRecoveryWizardModel::onInspected()
{
    const muse::RetVal<Inspection> inspection = m_inspectionWatcher.result();
    if (inspection.ret) {
        m_inspection = inspection.val;
    } else {
        m_error = QString::fromStdString(inspection.ret.text());
    }
    setStage(Stage::Results);
}

void ProjectRecoveryWizardModel::runRecovery()
{
    if (m_stage != Stage::MethodDetails || !canRepair()) {
        return;
    }
    const muse::io::path_t original(m_projectPath);
    const muse::io::path_t output = makeOutputPath();
    m_outputPath = output.toQString();
    m_error.clear();
    setStage(Stage::Recovering);
    m_repairWatcher.setFuture(recoveryService()->repair(original, output));
}

void ProjectRecoveryWizardModel::onRepaired()
{
    const muse::RetVal<RepairReport> report = m_repairWatcher.result();
    if (!report.ret) {
        m_error = QString::fromStdString(report.ret.text());
    }
    setStage(Stage::Finished);
}

void ProjectRecoveryWizardModel::openResult()
{
    if (m_stage != Stage::Finished || !m_error.isEmpty()) {
        return;
    }
    const muse::io::path_t output(m_outputPath);
    commandDispatcher()->dispatch(project::PROJECT_OPEN_COMMAND, { { project::PROJECT_URL_PARAM, muse::Val(output.toQUrl().toString()) } });
}

void ProjectRecoveryWizardModel::setStage(Stage stage)
{
    m_stage = stage;
    emit stageChanged();
}

muse::io::path_t ProjectRecoveryWizardModel::makeOutputPath() const
{
    const muse::io::path_t original(m_projectPath);
    // An unsaved project lives in a folder that Audacity cleans up
    const bool unsaved = project::isAudacityUnsavedFile(original);
    muse::io::path_t folder = muse::io::dirpath(original);
    if (unsaved || !fileSystem()->isWritable(folder)) {
        folder = projectConfiguration()->userProjectsPath();
    }
    if (folder.empty()) {
        folder = projectConfiguration()->defaultUserProjectsPath();
    }
    const std::string suffix = unsaved ? std::string("aup4") : muse::io::suffix(original);
    const std::string base = muse::io::completeBasename(original).toStdString();
    const std::string recovered = muse::trc("projectrecovery", "Recovered");
    for (int number = 1;; ++number) {
        const std::string name = base + " (" + recovered + (number > 1 ? " " + std::to_string(number) : "") + ")." + suffix;
        const muse::io::path_t candidate = folder.appendingComponent(name);
        // The repair refuses an output that has a -wal or -shm file next to it
        if (!fileSystem()->exists(candidate) && !fileSystem()->exists(candidate + "-wal") && !fileSystem()->exists(candidate + "-shm")) {
            return candidate;
        }
    }
}
