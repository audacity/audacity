/*
* Audacity: A Digital Audio Editor
*/

#include "importerconfiguration.h"

#include "global/settings.h"
#include "framework/global/stringutils.h"

using namespace au::importexport;

static const std::string module_name("importer");

static const muse::Settings::Key TEMPO_DETECTION_WORKSPACES(module_name, "importer/tempoDetectionWorkspaces");
static const muse::Settings::Key EMPTY_PROJECT_LOOP_ACTION(module_name, "importer/emptyProjectLoopAction");
static const muse::Settings::Key SUBSEQUENT_IMPORT_LOOP_ACTION(module_name, "importer/subsequentImportLoopAction");

void ImporterConfiguration::init()
{
    muse::settings()->setDefaultValue(TEMPO_DETECTION_WORKSPACES, muse::Val(std::string("Music")));
    muse::settings()->valueChanged(TEMPO_DETECTION_WORKSPACES).onReceive(nullptr, [this](const muse::Val&) {
        m_tempoDetectionWorkspacesChanged.notify();
    });

    muse::settings()->setDefaultValue(EMPTY_PROJECT_LOOP_ACTION, muse::Val(static_cast<int>(LoopAction::Ask)));
    muse::settings()->setDefaultValue(SUBSEQUENT_IMPORT_LOOP_ACTION, muse::Val(static_cast<int>(LoopAction::Ask)));
}

std::vector<std::string> ImporterConfiguration::tempoDetectionWorkspaces() const
{
    std::vector<std::string> result;
    std::string combinedString = muse::settings()->value(TEMPO_DETECTION_WORKSPACES).toString();
    muse::strings::split(combinedString, result, "|");
    return result;
}

void ImporterConfiguration::setTempoDetectionWorkspaces(const std::vector<std::string>& workspaces)
{
    if (tempoDetectionWorkspaces() == workspaces) {
        return;
    }
    std::string result = muse::strings::join(workspaces, "|");
    muse::settings()->setSharedValue(TEMPO_DETECTION_WORKSPACES, muse::Val(result));
}

muse::async::Notification ImporterConfiguration::tempoDetectionWorkspacesChanged() const
{
    return m_tempoDetectionWorkspacesChanged;
}

LoopAction ImporterConfiguration::emptyProjectLoopAction() const
{
    return static_cast<LoopAction>(muse::settings()->value(EMPTY_PROJECT_LOOP_ACTION).toInt());
}

void ImporterConfiguration::setEmptyProjectLoopAction(LoopAction action)
{
    muse::settings()->setSharedValue(EMPTY_PROJECT_LOOP_ACTION, muse::Val(static_cast<int>(action)));
}

LoopAction ImporterConfiguration::subsequentImportLoopAction() const
{
    return static_cast<LoopAction>(muse::settings()->value(SUBSEQUENT_IMPORT_LOOP_ACTION).toInt());
}

void ImporterConfiguration::setSubsequentImportLoopAction(LoopAction action)
{
    muse::settings()->setSharedValue(SUBSEQUENT_IMPORT_LOOP_ACTION, muse::Val(static_cast<int>(action)));
}
