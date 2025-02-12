/**********************************************************************

  Audacity: A Digital Audio Editor

  FFmpegAPIResolver.cpp

  Dmitry Vedenko

**********************************************************************/

#include "FFmpegAPIResolver.h"

namespace avformat_55 {
extern void Register();
}
namespace avformat_57 {
extern void Register();
}
namespace avformat_58 {
extern void Register();
}
namespace avformat_59 {
extern void Register();
}
namespace avformat_60 {
extern void Register();
}
namespace avformat_61 {
extern void Register();
}

namespace avutil_52 {
extern void Register();
}
namespace avutil_55 {
extern void Register();
}
namespace avutil_56 {
extern void Register();
}
namespace avutil_57 {
extern void Register();
}
namespace avutil_58 {
extern void Register();
}
namespace avutil_59 {
extern void Register();
}

namespace avcodec_55 {
extern void Register();
extern void RegisterId();
}
namespace avcodec_57 {
extern void Register();
extern void RegisterId();
}
namespace avcodec_58 {
extern void Register();
extern void RegisterId();
}
namespace avcodec_59 {
extern void Register();
extern void RegisterId();
}
namespace avcodec_60 {
extern void Register();
extern void RegisterId();
}
namespace avcodec_61 {
extern void Register();
extern void RegisterId();
}

// without this method all supported ffmpeg versions are not initialized
void Register()
{
    avformat_55::Register();
    avformat_57::Register();
    avformat_58::Register();
    avformat_59::Register();
    avformat_60::Register();
    avformat_61::Register();

    avutil_52::Register();
    avutil_55::Register();
    avutil_56::Register();
    avutil_57::Register();
    avutil_58::Register();
    avutil_59::Register();

    avcodec_55::Register();
    avcodec_57::Register();
    avcodec_58::Register();
    avcodec_59::Register();
    avcodec_60::Register();
    avcodec_61::Register();

    avcodec_55::RegisterId();
    avcodec_57::RegisterId();
    avcodec_58::RegisterId();
    avcodec_59::RegisterId();
    avcodec_60::RegisterId();
    avcodec_61::RegisterId();
}

FFmpegAPIResolver& FFmpegAPIResolver::Get()
{
    static FFmpegAPIResolver instance;
    return instance;
}

bool FFmpegAPIResolver::GetAVCodecIDResolver(int avCodecVersion, AVCodecIDResolver& resolver) const
{
    const auto it = mAVCodecIDResolvers.find(avCodecVersion);

    if (it == mAVCodecIDResolvers.end()) {
        return false;
    }

    resolver = it->second;

    return true;
}

bool FFmpegAPIResolver::GetAVCodecFactories(int avCodecVersion, AVCodecFactories& factories) const
{
    const auto it = mAVCodecFactories.find(avCodecVersion);

    if (it == mAVCodecFactories.end()) {
        return false;
    }

    factories = it->second;

    return true;
}

bool FFmpegAPIResolver::GetAVFormatFactories(int avFormatVersion, AVFormatFactories& factories) const
{
    const auto it = mAVFormatFactories.find(avFormatVersion);

    if (it == mAVFormatFactories.end()) {
        return false;
    }

    factories = it->second;

    return true;
}

bool FFmpegAPIResolver::GetAVUtilFactories(int avUtilVersion, AVUtilFactories& factories) const
{
    const auto it = mAVUtilFactories.find(avUtilVersion);

    if (it == mAVUtilFactories.end()) {
        return false;
    }

    factories = it->second;

    return true;
}

void FFmpegAPIResolver::AddAVCodecIDResolver(int avCodecVersion, const AVCodecIDResolver& resolver)
{
    mAVCodecIDResolvers.emplace(avCodecVersion, resolver);
}

void FFmpegAPIResolver::AddAVCodecFactories(int avCodecVersion, const AVCodecFactories& factories)
{
    mAVCodecFactories.emplace(avCodecVersion, factories);
}

void FFmpegAPIResolver::AddAVFormatFactories(int avFormatVersion, const AVFormatFactories& factories)
{
    mAVFormatFactories.emplace(avFormatVersion, factories);
}

void FFmpegAPIResolver::AddAVUtilFactories(int avUtilVersion, const AVUtilFactories& factories)
{
    mAVUtilFactories.emplace(avUtilVersion, factories);
}

std::vector<int> FFmpegAPIResolver::GetSuportedAVFormatVersions() const
{
    std::vector<int> result;
    result.reserve(mAVFormatFactories.size());

    for (auto it = mAVFormatFactories.rbegin(), end = mAVFormatFactories.rend();
         it != end; ++it) {
        result.emplace_back(it->first);
    }

    return result;
}
