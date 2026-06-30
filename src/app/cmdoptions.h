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
        bool memoryLeakReport = false;
        bool version = false;
        bool longVersion = false;
    } app;

    struct CloudProject {
        QString id;
        std::optional<QString> snapshotId;
    };

    struct {
        std::optional<std::string> type;
        std::optional<QUrl> projectUrl;
        std::optional<QString> projectDisplayNameOverride;
        std::optional<CloudProject> cloudProject;
        std::optional<QString> startupUrl;
        muse::io::paths_t mediaFiles;
        bool removeMediaFilesAfterImport = false;
    } startup;

    struct Testflow {
        QString testCaseNameOrFile;
        QString testCaseContextNameOrFile;
        QString testCaseContextValue;
        QString testCaseFunc;
        QString testCaseFuncArgs;
    } testflow;

    struct AudioPluginRegistration {
        muse::io::path_t pluginPath;
        muse::io::path_t outputPath;
        bool selfTest = false;
    } audioPluginRegistration;
};
}
