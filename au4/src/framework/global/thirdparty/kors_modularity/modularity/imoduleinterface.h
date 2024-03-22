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
#ifndef KORS_MODULARITY_IMODULEINTERFACE_H
#define KORS_MODULARITY_IMODULEINTERFACE_H

#include <memory>
#include <string_view>

#include "funcinfo.h"

namespace kors::modularity {
class IModuleInterface
{
public:
    virtual ~IModuleInterface() = default;
};

class IModuleExportInterface : public IModuleInterface
{
public:
    virtual ~IModuleExportInterface() = default;

    static constexpr bool isInternalInterface() { return false; }
};

class IModuleInternalInterface : public IModuleInterface
{
public:
    virtual ~IModuleInternalInterface() = default;

    static constexpr bool isInternalInterface() { return true; }
};

class IModuleCreator
{
public:
    virtual ~IModuleCreator() = default;
    virtual std::shared_ptr<IModuleInterface> create() = 0;
};

struct IModuleExportCreator : public IModuleCreator {
    virtual ~IModuleExportCreator() = default;
    static constexpr bool isInternalInterface() { return false; }
};

struct IModuleInternalCreator : public IModuleCreator {
    virtual ~IModuleInternalCreator() = default;
    static constexpr bool isInternalInterface() { return true; }
};

struct InterfaceInfo {
    std::string_view id;
    std::string_view module;
    bool internal = false;
    constexpr InterfaceInfo(std::string_view i, std::string_view m, bool intr)
        : id(i), module(m), internal(intr) {}
};
}

#ifndef _KORS_NO_STRINGVIEW_CONSTEXPR_METHODS
#define INTERFACE_ID(id)                                                \
public:                                                                 \
    static constexpr kors::modularity::InterfaceInfo interfaceInfo() {    \
        constexpr kors::modularity::InterfaceInfo info(#id, MODULENAME, isInternalInterface());    \
        return info;                                                    \
    }                                                                   \
private:                                                                \

#else
#define INTERFACE_ID(id)                                                \
public:                                                                 \
    static const kors::modularity::InterfaceInfo& interfaceInfo() {       \
        static const kors::modularity::InterfaceInfo info(#id, MODULENAME, isInternalInterface());    \
        return info;                                                    \
    }                                                                   \
private:                                                                \

#endif

#define MODULE_EXPORT_INTERFACE public kors::modularity::IModuleExportInterface
#define MODULE_EXPORT_CREATOR public kors::modularity::IModuleExportCreator

#define MODULE_INTERNAL_INTERFACE public kors::modularity::IModuleInternalInterface
#define MODULE_INTERNAL_CREATOR public kors::modularity::IModuleInternalCreator

#endif // KORS_MODULARITY_IMODULEINTERFACE_H
