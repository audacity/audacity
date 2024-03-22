/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#ifndef MU_GLOBAL_GLOBALMODULE_H
#define MU_GLOBAL_GLOBALMODULE_H

#include <memory>
#include <optional>

#include "logger.h"

#include "modularity/imodulesetup.h"
#include "modularity/ioc.h"
#include "io/ifilesystem.h"

namespace mu {
class SystemInfo;
class Invoker;
class GlobalConfiguration;
class GlobalModule : public modularity::IModuleSetup
{
    Inject<io::IFileSystem> fileSystem;

public:

    std::string moduleName() const override;
    void registerExports() override;
    void registerApi() override;
    void onPreInit(const IApplication::RunMode& mode) override;
    void onInit(const IApplication::RunMode& mode) override;
    void onDeinit() override;

    static void invokeQueuedCalls();

    void setLoggerLevel(const mu::logger::Level& level);

private:
    std::shared_ptr<GlobalConfiguration> m_configuration;
    std::shared_ptr<SystemInfo> m_systemInfo;

    std::optional<mu::logger::Level> m_loggerLevel;

    static std::shared_ptr<Invoker> s_asyncInvoker;
};
}

#endif // MU_GLOBAL_GLOBALMODULE_H
