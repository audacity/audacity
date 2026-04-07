/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>
#include <memory>

#include <vector>
#include <cstdint>

#include "global/io/path.h"
#include "global/types/ret.h"
#include "global/async/notification.h"
#include "modularity/imoduleinterface.h"
#include "framework/global/modularity/ioc.h"

namespace au::au3 {
//! NOTE It's exactly IAu3Project, not just IProject
class IAu3Project : public muse::Contextable
{
public:
    IAu3Project(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    virtual ~IAu3Project() = default;

    [[nodiscard]] virtual muse::Ret open() = 0;
    [[nodiscard]] virtual muse::Ret load(const muse::io::path_t& filePath, bool ignoreAutosave = false) = 0;
    virtual bool save(const muse::io::path_t& fileName) = 0;
    virtual void saveThumbnail(std::vector<uint8_t> pngData) = 0;
    virtual void close() = 0;

    virtual std::string title() const = 0;
    [[nodiscard]] virtual muse::io::path_t getFileName() const = 0;

    // Save status management
    [[nodiscard]] virtual bool hasUnsavedChanges() const = 0;
    virtual void markAsSaved() = 0;
    [[nodiscard]] virtual bool isRecovered() const = 0;
    [[nodiscard]] virtual bool isTemporary() const = 0;

    // Autosave management
    [[nodiscard]] virtual bool hasAutosaveData() const = 0;
    [[nodiscard]] virtual muse::Ret removeAutosaveData() = 0;

    virtual muse::async::Notification projectChanged() const = 0;

    // internal
    virtual uintptr_t au3ProjectPtr() const = 0;
};

class IAu3ProjectCreator : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IAu3ProjectCreator)
public:
    virtual ~IAu3ProjectCreator() = default;

    virtual std::shared_ptr<IAu3Project> create(const muse::modularity::ContextPtr& ctx) const = 0;

    [[nodiscard]] virtual muse::Ret removeUnsavedData(const muse::io::path_t& projectPath) const = 0;
};
}
