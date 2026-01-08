/*
* Audacity: A Digital Audio Editor
*/

#include "mod-ffmpeg/FFmpeg.h"
#include "mod-ffmpeg/ExportFFmpegOptions.h"
#include "mod-ffmpeg/lib-ffmpeg-support/FFmpegFunctions.h"

#include "au3ffmpegoptionsaccessor.h"

using namespace au::importexport;

void Au3FFmpegOptionsAccessor::init()
{
    m_options = std::make_unique<ExportFFmpegOptions>();
}

std::vector<std::string> Au3FFmpegOptionsAccessor::formatList() const
{
    return m_options->GetFormatNames();
}

void Au3FFmpegOptionsAccessor::fetchCompatibleFormatList(const std::string& format,
                                                         const std::string& codec)
{
    m_options->FetchCompatibleFormatList(format, codec);
}

void Au3FFmpegOptionsAccessor::fetchAllFormats()
{
    m_options->FetchFormatList();
}

std::vector<std::string> Au3FFmpegOptionsAccessor::codecList() const
{
    return m_options->GetCodecNames();
}

void Au3FFmpegOptionsAccessor::fetchCompatibleCodecList(const std::string& format,
                                                        const std::string& codec)
{
    m_options->FetchCompatibleCodecList(format, codec);
}

void Au3FFmpegOptionsAccessor::fetchAllCodecs()
{
    m_options->FetchCodecList();
}

std::vector<std::string> Au3FFmpegOptionsAccessor::predictionOrderMethodList() const
{
    return m_options->GetPredictionOrderMethods();
}

std::string Au3FFmpegOptionsAccessor::ffmpegVersion() const
{
    return GetFFmpegVersion().Translation().ToStdString();
}

std::string Au3FFmpegOptionsAccessor::ffmpegLibraryPath() const
{
    auto ffmpeg = FFmpegFunctions::Load();
    if (!ffmpeg) {
        return {};
    }
    return ffmpeg->GetLoadedAVFormatPath().ToStdString();
}

bool Au3FFmpegOptionsAccessor::setFFmpegLibraryPath(const muse::io::path_t& path)
{
    return SetFFmpegPath(path.toStdString());
}

std::vector<std::string> Au3FFmpegOptionsAccessor::profileList() const
{
    return m_options->GetProfiles();
}
