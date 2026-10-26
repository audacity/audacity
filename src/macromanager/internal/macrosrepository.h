/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/types/config.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/iglobalconfiguration.h"
#include "framework/global/io/ifilesystem.h"

#include "macromanager/imacrosrepository.h"

namespace au::macromanager {
class MacrosRepository : public IMacrosRepository
{
    muse::GlobalInject<muse::IGlobalConfiguration> globalConfiguration;
    muse::GlobalInject<muse::io::IFileSystem> fileSystem;

public:
    MacrosRepository() = default;

    void init();

    const MacroList& macros() const override;
    std::optional<Macro> macro(const std::string& name) const override;
    std::vector<std::string> dependentMacros(const std::string& name) const override;
    muse::Ret validateReferences(const Macro& macro) const override;

    muse::Ret save(const Macro& macro) override;
    muse::Ret remove(const std::string& name) override;
    muse::Ret rename(const std::string& oldName, const std::string& newName) override;

    muse::RetVal<Macro> readMacroFile(const muse::io::path_t& path) const override;
    muse::Ret writeMacroFile(const Macro& macro, const muse::io::path_t& path) const override;

    muse::async::Notification macrosChanged() const override;

private:
    muse::io::path_t userMacrosPath() const;
    muse::io::path_t userMacroFilePath(const std::string& name) const;
    muse::io::paths_t builtinMacroFilePaths() const;

    void reload();
    void loadMacroFile(const muse::io::path_t& path, bool builtin);
    muse::Ret visitReferences(const Macro& macro, std::vector<std::string>& path) const;

    muse::Config m_config;
    MacroList m_macros;
    muse::async::Notification m_macrosChanged;
};
}
