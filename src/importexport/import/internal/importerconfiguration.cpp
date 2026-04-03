/*
* Audacity: A Digital Audio Editor
*/

#include "importerconfiguration.h"

#include "global/settings.h"
#include "framework/global/stringutils.h"

using namespace au::importexport;

static const std::string module_name("importer");

static const muse::Settings::Key TEMPO_DETECTION_PREF(module_name, "importer/tempoDetectionPref");
static const muse::Settings::Key TEMPO_DETECTION_WORKSPACES(module_name, "importer/tempoDetectionWorkspaces");
static const muse::Settings::Key SUBSEQUENT_IMPORT_LOOP_ACTION(module_name, "importer/subsequentImportLoopAction");

void ImporterConfiguration::init()
{
    muse::settings()->setDefaultValue(TEMPO_DETECTION_PREF,
                                      muse::Val(static_cast<int>(TempoDetectionPref::TempoDetection::WORKSPACE_DEPENDENT)));
    muse::settings()->valueChanged(TEMPO_DETECTION_PREF).onReceive(nullptr, [this](const muse::Val&) {
        m_tempoDetectionPrefChanged.notify();
    });

    muse::settings()->setDefaultValue(TEMPO_DETECTION_WORKSPACES, muse::Val(std::string("Music")));
    muse::settings()->valueChanged(TEMPO_DETECTION_WORKSPACES).onReceive(nullptr, [this](const muse::Val&) {
        m_tempoDetectionWorkspacesChanged.notify();
    });

    muse::settings()->setDefaultValue(SUBSEQUENT_IMPORT_LOOP_ACTION, muse::Val(static_cast<int>(LoopAction::Ask)));
    muse::settings()->valueChanged(SUBSEQUENT_IMPORT_LOOP_ACTION).onReceive(nullptr, [this](const muse::Val&) {
        m_subsequentImportLoopActionChanged.notify();
    });
}

TempoDetectionPref::TempoDetection ImporterConfiguration::tempoDetectionPref() const
{
    return muse::settings()->value(TEMPO_DETECTION_PREF).toEnum<TempoDetectionPref::TempoDetection>();
}

void ImporterConfiguration::setTempoDetectionPref(TempoDetectionPref::TempoDetection pref)
{
    if (tempoDetectionPref() == pref) {
        return;
    }
    muse::settings()->setSharedValue(TEMPO_DETECTION_PREF, muse::Val(static_cast<int>(pref)));
}

muse::async::Notification ImporterConfiguration::tempoDetectionPrefChanged() const
{
    return m_tempoDetectionPrefChanged;
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

LoopAction ImporterConfiguration::subsequentImportLoopAction() const
{
    return muse::settings()->value(SUBSEQUENT_IMPORT_LOOP_ACTION).toEnum<LoopAction>();
}

void ImporterConfiguration::setSubsequentImportLoopAction(LoopAction action)
{
    muse::settings()->setSharedValue(SUBSEQUENT_IMPORT_LOOP_ACTION, muse::Val(static_cast<int>(action)));
}

muse::async::Notification ImporterConfiguration::subsequentImportLoopActionChanged() const
{
    return m_subsequentImportLoopActionChanged;
}
