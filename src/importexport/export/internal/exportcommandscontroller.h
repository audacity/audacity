/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>
#include <string>

#include "framework/rcommand/commandable.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/io/ifilesystem.h"
#include "framework/rcommand/icommanddispatcher.h"
#include "context/iglobalcontext.h"
#include "../iexportconfiguration.h"
#include "../iexporter.h"
#include "exportcommandoptions.h"

namespace au::importexport {
class ExportCommandsController : public muse::rcommand::Commandable, public muse::Contextable
{
    muse::GlobalInject<IExportConfiguration> exportConfiguration;
    muse::GlobalInject<muse::io::IFileSystem> fileSystem;

    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher{ this };
    muse::ContextInject<IExporter> exporter{ this };
    muse::ContextInject<context::IGlobalContext> globalContext{ this };

public:
    explicit ExportCommandsController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

private:
    muse::Ret exportAudio(const ExportFormatSettings& settings, const std::string& path, int sampleRate, ExportChannelLayout channels,
                          const std::string& name);

    std::optional<std::string> formatForExtension(const std::string& extension) const;
    std::string outputBaseName() const;
};
}
