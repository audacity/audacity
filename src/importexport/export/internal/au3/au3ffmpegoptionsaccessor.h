/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "ExportFFmpegOptions.h"

#include "iffmpegoptionsaccessor.h"

namespace au::importexport {
class Au3FFmpegOptionsAccessor : public IFFmpegOptionsAccessor
{
public:
    void init() override;

    std::vector<std::string> formatList() const override;
    void fetchCompatibleFormatList(const std::string& format, const std::string& codec) override;
    void showAllFormats() override;

    std::vector<std::string> codecList() const override;
    void fetchCompatibleCodecList(const std::string& format, const std::string& codec) override;
    void showAllCodecs() override;

    std::vector<std::string> profileList() const override;
    std::vector<std::string> predictionOrderMethodList() const override;

private:
    ExportFFmpegOptions m_options;
};
}
