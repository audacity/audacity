/*
MIT License

Copyright (c) 2020 Igor Korsukov

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
#ifndef KORS_MODULARITY_MODULESIOC_H
#define KORS_MODULARITY_MODULESIOC_H

#include <memory>
#include <map>
#include <string>
#include <cassert>
#include <iostream>

#include "imoduleinterface.h"

namespace kors::modularity {
class ModulesIoC
{
public:

    static ModulesIoC* instance();

    // Register Export
    template<class I>
    void registerExportCreator(const std::string& module, IModuleCreator* c)
    {
        if (!c) {
            assert(c);
            return;
        }
        registerService(module, I::interfaceInfo(), std::shared_ptr<IModuleInterface>(), c);
    }

    template<class I>
    void registerExport(const std::string& module, I* p)
    {
        if (!p) {
            assert(p);
            return;
        }
        registerExport<I>(module, std::shared_ptr<I>(p));
    }

    template<class I>
    void registerExportNoDelete(const std::string& module, I* p)
    {
        if (!p) {
            assert(p);
            return;
        }
        registerExport<I>(module, std::shared_ptr<I>(p, [](I*) {}));
    }

    template<class I>
    void registerExport(const std::string& module, std::shared_ptr<I> p)
    {
        if (!p) {
            assert(p);
            return;
        }
        registerService(module, I::interfaceInfo(), std::static_pointer_cast<IModuleInterface>(p), nullptr);
    }

    // Register Internal
    template<class I>
    void registerInternalCreator(const std::string& module, IModuleCreator* c)
    {
        if (!c) {
            assert(c);
            return;
        }
        registerService(module, I::interfaceInfo(), std::shared_ptr<IModuleInterface>(), c);
    }

    template<class I>
    void registerInternal(const std::string& module, I* p)
    {
        if (!p) {
            assert(p);
            return;
        }
        registerInternal<I>(module, std::shared_ptr<I>(p));
    }

    template<class I>
    void registerInternalNoDelete(const std::string& module, I* p)
    {
        if (!p) {
            assert(p);
            return;
        }
        registerInternal<I>(module, std::shared_ptr<I>(p, [](I*) {}));
    }

    template<class I>
    void registerInternal(const std::string& module, std::shared_ptr<I> p)
    {
        if (!p) {
            assert(p);
            return;
        }
        registerService(module, I::interfaceInfo(), std::static_pointer_cast<IModuleInterface>(p), nullptr);
    }

    // Unregister
    template<class I>
    void unregister(const std::string& /*module*/)
    {
        unregisterService(I::interfaceInfo());
    }

    template<class I>
    void unregisterIfRegistered(const std::string& module, std::shared_ptr<I> p)
    {
        if (resolve<I>(module, std::string_view()) == p) {
            unregister<I>(module);
        }
    }

    // Resolve
    template<class I>
    std::shared_ptr<I> resolve(const std::string_view& module, const std::string_view& callInfo = std::string_view())
    {
        std::shared_ptr<IModuleInterface> p = doResolvePtrByInfo(module, I::interfaceInfo(), callInfo);
#ifndef NDEBUG
        return std::dynamic_pointer_cast<I>(p);
#else
        return std::static_pointer_cast<I>(p);
#endif
    }

    template<class I>
    std::shared_ptr<I> resolveRequiredImport(const std::string& module)
    {
        std::shared_ptr<IModuleInterface> p = doResolvePtrByInfo(module, I::interfaceInfo(), std::string_view());
        if (!p) {
            std::cerr << "not found implementation for interface: " << I::interfaceInfo().id << std::endl;
            assert(false);
        }
#ifndef NDEBUG
        return std::dynamic_pointer_cast<I>(p);
#else
        return std::static_pointer_cast<I>(p);
#endif
    }

    void reset()
    {
        m_map.clear();
    }

private:

    ModulesIoC() = default;

    void unregisterService(const InterfaceInfo& info)
    {
        m_map.erase(info.id);
    }

    void registerService(const std::string& module,
                         const InterfaceInfo& info,
                         std::shared_ptr<IModuleInterface> p,
                         IModuleCreator* c)
    {
        auto foundIt = m_map.find(info.id);
        if (foundIt != m_map.end()) {
            std::cerr << module << ": double register:"
                      << info.id << ", first register in" << m_map[info.id].sourceModule << std::endl;
            assert(false);
            return;
        }

        Service inj;
        inj.sourceModule = module;
        inj.c = c;
        inj.p = p;
        m_map[info.id] = inj;
    }

    std::shared_ptr<IModuleInterface> doResolvePtrByInfo(const std::string_view& usageModule,
                                                         const InterfaceInfo& info,
                                                         const std::string_view& callInfo)
    {
        //! TODO add statistics collection / monitoring, who resolves what

        if (info.internal) {
            if (usageModule != info.module) {
                std::cerr << "Assertion failed!! Interface '" << info.id << "' is internal"
                          << ", usage module: '" << usageModule << "'"
                          << ", interface module: '" << info.module << "'"
                          << ", called from: " << (callInfo.empty() ? std::string_view("unknown") : callInfo)
                          << std::endl;

                #ifndef NDEBUG
                std::abort();
                #endif
            }
        }

        auto it = m_map.find(info.id);
        if (it == m_map.end()) {
            return nullptr;
        }

        Service& inj = it->second;
        if (inj.p) {
            return inj.p;
        }

        if (inj.c) {
            return inj.c->create();
        }

        return nullptr;
    }

    struct Service {
        IModuleCreator* c = nullptr;
        std::string sourceModule;
        std::shared_ptr<IModuleInterface> p;
    };

    std::map<std::string_view, Service > m_map;
};

template<class T>
struct Creator : MODULE_EXPORT_CREATOR
{
    std::shared_ptr<IModuleInterface> create() { return std::make_shared<T>(); }
};
}

#endif // KORS_MODULARITY_MODULESIOC_H
