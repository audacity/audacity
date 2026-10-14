/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"
#include "context/iglobalcontext.h"
#include "importexport/import/iimporter.h"
#include "project/iprojectconfiguration.h"
#include "macromanager/imacrorunner.h"

namespace au::macromanager {
class RunOnFilesLauncher : public muse::Contextable
{
    muse::GlobalInject<project::IProjectConfiguration> projectConfiguration;

    muse::ContextInject<IMacroRunner> runner{ this };
    muse::ContextInject<muse::IInteractive> interactive{ this };
    muse::ContextInject<context::IGlobalContext> globalContext{ this };
    muse::ContextInject<importexport::IImporter> importer{ this };

public:
    explicit RunOnFilesLauncher(const muse::Contextable* owner)
        : muse::Contextable(owner) {}

    void launch(const Macro& macro);

private:
    muse::io::paths_t selectFiles() const;
};
}
