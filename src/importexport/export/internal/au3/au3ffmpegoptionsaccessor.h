/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "mod-ffmpeg/ExportFFmpegOptions.h"

#include "iffmpegoptionsaccessor.h"

namespace au::importexport {
class Au3FFmpegOptionsAccessor : public IFFmpegOptionsAccessor
{
public:
    void init();

    std::vector<std::string> formatList() const override;
    void fetchCompatibleFormatList(const std::string& format, const std::string& codec) override;
    void fetchAllFormats() override;

    std::vector<std::string> codecList() const override;
    void fetchCompatibleCodecList(const std::string& format, const std::string& codec) override;
    void fetchAllCodecs() override;

    std::vector<std::string> profileList() const override;
    std::vector<std::string> predictionOrderMethodList() const override;

private:
    std::unique_ptr<ExportFFmpegOptions> m_options;
};
}
