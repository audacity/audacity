/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>
#include <string>
#include <vector>

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/async/notification.h"
#include "framework/global/io/path.h"
#include "framework/global/types/ret.h"
#include "framework/global/types/retval.h"

#include "macromanagertypes.h"

namespace au::macromanager {
class IMacrosRepository : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IMacrosRepository)

public:
    virtual ~IMacrosRepository() = default;

    virtual const MacroList& macros() const = 0;
    virtual std::optional<Macro> macro(const std::string& name) const = 0;
    virtual std::vector<std::string> dependentMacros(const std::string& name) const = 0;
    virtual std::vector<std::string> referencingMacros(const std::string& name) const = 0;
    virtual muse::Ret validateReferences(const Macro& macro) const = 0;

    virtual muse::Ret save(const Macro& macro) = 0;
    virtual muse::Ret remove(const std::string& name) = 0;
    virtual muse::Ret rename(const std::string& oldName, const std::string& newName) = 0;

    virtual muse::RetVal<Macro> readMacroFile(const muse::io::path_t& path) const = 0;
    virtual muse::Ret writeMacroFile(const Macro& macro, const muse::io::path_t& path) const = 0;

    virtual muse::async::Notification macrosChanged() const = 0;
};
}
