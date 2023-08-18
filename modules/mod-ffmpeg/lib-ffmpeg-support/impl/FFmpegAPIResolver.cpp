/**********************************************************************

  Audacity: A Digital Audio Editor

  FFmpegAPIResolver.cpp

  Dmitry Vedenko

**********************************************************************/

#include "FFmpegAPIResolver.h"

FFmpegAPIResolver& FFmpegAPIResolver::Get()
{
   static FFmpegAPIResolver instance;
   return instance;
}

bool FFmpegAPIResolver::GetAVCodecIDResolver(int avCodecVersion, AVCodecIDResolver& resolver) const
{
   const auto it = mAVCodecIDResolvers.find(avCodecVersion);

   if (it == mAVCodecIDResolvers.end())
      return false;

   resolver = it->second;

   return true;
}

bool FFmpegAPIResolver::GetAVCodecFactories(int avCodecVersion, AVCodecFactories& factories) const
{
   const auto it = mAVCodecFactories.find(avCodecVersion);

   if (it == mAVCodecFactories.end())
      return false;

   factories = it->second;

   return true;
}

bool FFmpegAPIResolver::GetAVFormatFactories(int avFormatVersion, AVFormatFactories& factories) const
{
   const auto it = mAVFormatFactories.find(avFormatVersion);

   if (it == mAVFormatFactories.end())
      return false;

   factories = it->second;

   return true;
}

bool FFmpegAPIResolver::GetAVUtilFactories(int avUtilVersion, AVUtilFactories& factories) const
{
   const auto it = mAVUtilFactories.find(avUtilVersion);

   if (it == mAVUtilFactories.end())
      return false;

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
        it != end; ++it)
   {
      result.emplace_back(it->first);
   }

   return result;
}
