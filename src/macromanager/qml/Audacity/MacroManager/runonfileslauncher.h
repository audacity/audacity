/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/ioc.h"
#include "framework/global/iapplication.h"
#include "framework/interactive/iinteractive.h"
#include "importexport/import/iimporter.h"
#include "project/iprojectconfiguration.h"
#include "macromanager/irunonfilesrequests.h"

namespace au::macromanager {
class RunOnFilesLauncher : public muse::Contextable
{
    muse::GlobalInject<project::IProjectConfiguration> projectConfiguration;
    muse::GlobalInject<muse::IApplication> application;
    muse::GlobalInject<IRunOnFilesRequests> requests;

    muse::ContextInject<muse::IInteractive> interactive{ this };
    muse::ContextInject<importexport::IImporter> importer{ this };

public:
    explicit RunOnFilesLauncher(const muse::Contextable* owner)
        : muse::Contextable(owner) {}

    void launch(const Macro& macro);

private:
    muse::io::paths_t selectFiles() const;
};
}
