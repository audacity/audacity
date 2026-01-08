/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "io/path.h"
#include "modularity/imoduleinterface.h"

namespace au::importexport {
class IFFmpegOptionsAccessor : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IFFmpegOptionsAccessor)

public:
    virtual ~IFFmpegOptionsAccessor() = default;

    virtual std::vector<std::string> formatList() const = 0;
    virtual void fetchCompatibleFormatList(const std::string& format, const std::string& codec) = 0;
    virtual void fetchAllFormats() = 0;

    virtual std::vector<std::string> codecList() const = 0;
    virtual void fetchCompatibleCodecList(const std::string& format, const std::string& codec) = 0;
    virtual void fetchAllCodecs() = 0;

    virtual std::vector<std::string> profileList() const = 0;
    virtual std::vector<std::string> predictionOrderMethodList() const = 0;

    virtual std::string ffmpegVersion() const = 0;
    virtual std::string ffmpegLibraryPath() const = 0;
    virtual bool setFFmpegLibraryPath(const muse::io::path_t& path) = 0;
};
}
