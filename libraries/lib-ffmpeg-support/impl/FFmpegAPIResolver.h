/**********************************************************************

  Audacity: A Digital Audio Editor

  FFmpegAPIResolver.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>
#include <vector>
#include <map>

#include "FFmpegTypes.h"
#include "AVCodecID.h"

struct FFmpegFunctions;
class FFmpegLog;

class AVCodecContextWrapper;
class AVCodecWrapper;
class AVPacketWrapper;

struct AVCodecIDResolver final
{
   AVCodecIDFwd (*GetAVCodecID)(AudacityAVCodecID);
   AudacityAVCodecID (*GetAudacityCodecID)(AVCodecIDFwd);
};

struct AVCodecFactories final
{
   std::unique_ptr<AVCodecContextWrapper> (*CreateAVCodecContextWrapper)(const FFmpegFunctions&, AVCodecContext*) = nullptr;
   std::unique_ptr<AVCodecContextWrapper> (*CreateAVCodecContextWrapperFromCodec)(const FFmpegFunctions&, std::unique_ptr<AVCodecWrapper>) = nullptr;
   std::unique_ptr<AVCodecWrapper> (*CreateAVCodecWrapper) (const AVCodec*) = nullptr;

   //! @post return value is not null
   std::unique_ptr<AVPacketWrapper> (*CreateAVPacketWrapper) (const FFmpegFunctions&) = nullptr;
};

class AVFormatContextWrapper;
class AVInputFormatWrapper;
class AVIOContextWrapper;
class AVOutputFormatWrapper;
class AVStreamWrapper;

struct AVFormatFactories final
{
   std::unique_ptr<AVFormatContextWrapper> (*CreateAVFormatContextWrapper) (const FFmpegFunctions&) = nullptr;
   std::unique_ptr<AVInputFormatWrapper> (*CreateAVInputFormatWrapper) (AVInputFormat*) = nullptr;
   std::unique_ptr<AVIOContextWrapper> (*CreateAVIOContextWrapper) (const FFmpegFunctions&) = nullptr;
   std::unique_ptr<AVOutputFormatWrapper> (*CreateAVOutputFormatWrapper) (const AVOutputFormat*) = nullptr;
   std::unique_ptr<AVStreamWrapper> (*CreateAVStreamWrapper) (const FFmpegFunctions&, AVStream*, bool) = nullptr;
};

class AVFrameWrapper;

struct AVUtilFactories final
{
   //! @post return value is not null
   std::unique_ptr<AVFrameWrapper> (*CreateAVFrameWrapper)(const FFmpegFunctions&) = nullptr;
   std::unique_ptr<FFmpegLog> (*CreateLogCallbackSetter)(const FFmpegFunctions&) = nullptr;
};

class FFmpegAPIResolver final
{
   FFmpegAPIResolver() = default;

public:
   static FFmpegAPIResolver& Get();

   bool GetAVCodecIDResolver(int avCodecVersion, AVCodecIDResolver& resolver) const;
   bool GetAVCodecFactories(int avCodecVersion, AVCodecFactories& factories) const;
   bool GetAVFormatFactories(int avFormatVersion, AVFormatFactories& factories) const;
   bool GetAVUtilFactories(int avUtilVersion, AVUtilFactories& factories) const;

   void AddAVCodecIDResolver(int avCodecVersion, const AVCodecIDResolver& resolver);
   void AddAVCodecFactories(int avCodecVersion, const AVCodecFactories& factories);
   void AddAVFormatFactories(int avFormatVersion, const AVFormatFactories& factories);
   void AddAVUtilFactories(int avUtilVersion, const AVUtilFactories& factories);

   //! Compatible library versions to be sought at load time, ordered by
   //! decreasing preference (that is, newest version first)
   std::vector<int> GetSuportedAVFormatVersions() const;

private:
   std::map<int, AVCodecIDResolver> mAVCodecIDResolvers;
   std::map<int, AVCodecFactories> mAVCodecFactories;
   std::map<int, AVFormatFactories> mAVFormatFactories;
   std::map<int, AVUtilFactories> mAVUtilFactories;
};
