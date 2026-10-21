/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>
#include <string>

#include "framework/global/modularity/ioc.h"
#include "framework/global/iglobalconfiguration.h"
#include "framework/interactive/iinteractive.h"
#include "macromanager/imacrosrepository.h"

#include "macromanager/macromanagertypes.h"

namespace au::macromanager {
class MacroActions : public muse::Contextable
{
    muse::GlobalInject<IMacrosRepository> repository;
    muse::GlobalInject<muse::IGlobalConfiguration> globalConfiguration;

    muse::ContextInject<muse::IInteractive> interactive{ this };

public:
    explicit MacroActions(const muse::Contextable* owner)
        : muse::Contextable(owner) {}

    std::optional<std::string> requestNewName() const;
    std::optional<std::string> rename(const std::string& name) const;
    bool remove(const std::string& name) const;
    void exportMacro(const Macro& macro) const;
    muse::io::path_t selectImportFile() const;

private:
    std::optional<std::string> requestName(muse::UriQuery uri) const;
    std::vector<std::string> macroFileFilter() const;
};
}
