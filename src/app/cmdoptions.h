/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <optional>
#include <string>

#include <QString>
#include <QUrl>

#include "global/io/path.h"
#include "global/logger.h"
#include "global/internal/cmdoptions.h"

namespace au::app {
struct AudacityCmdOptions : public muse::CmdOptions {
    struct {
        std::optional<bool> revertToFactorySettings;
    } app;

    struct {
        std::optional<std::string> type;
        std::optional<QUrl> projectUrl;
        std::optional<QString> projectDisplayNameOverride;
        std::optional<QString> cloudProjectId;
        muse::io::paths_t mediaFiles;
    } startup;

    struct Autobot {
        QString testCaseNameOrFile;
        QString testCaseContextNameOrFile;
        QString testCaseContextValue;
        QString testCaseFunc;
        QString testCaseFuncArgs;
    } autobot;

    struct AudioPluginRegistration {
        muse::io::path_t pluginPath;
        bool failedPlugin = false;
        int failCode = 0;
        bool selfTest = false;
    } audioPluginRegistration;
};
}
