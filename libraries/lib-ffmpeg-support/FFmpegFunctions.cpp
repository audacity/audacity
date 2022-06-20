/**********************************************************************

  Audacity: A Digital Audio Editor

  FFmpegFunctions.cpp

  Dmitry Vedenko

**********************************************************************/

#include "FFmpegFunctions.h"

#include <wx/string.h>
#include <wx/dynlib.h>
#include <wx/log.h>
#include <wx/utils.h>

#if defined(__WXMSW__)
#  include <wx/buffer.h>
#  include <wx/msw/registry.h>

#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>
#  include <psapi.h>
#else
#  include <dlfcn.h>
#endif

#include "Prefs.h"
#include "FileNames.h"

#include "impl/avutil/AVUtilFunctionsLoader.h"
#include "impl/avcodec/AVCodecFunctionsLoader.h"
#include "impl/avformat/AVFormatFunctionsLoader.h"

#include "impl/FFmpegAPIResolver.h"
#include "impl/FFmpegLog.h"

void* GetSymbolFromProcess(const char* name)
{
#if defined(__WXMSW__)
   std::vector<HMODULE> handles(256);

   DWORD neededMemory;

   if (!EnumProcessModules(
          GetCurrentProcess(), handles.data(), sizeof(HMODULE) * handles.size(),
          &neededMemory))
   {
      return nullptr;
   }

   const int modulesCount = neededMemory / sizeof(HMODULE);

   if (modulesCount > handles.size())
   {
      handles.resize(modulesCount);

      if (!EnumProcessModules(
             GetCurrentProcess(), handles.data(),
             sizeof(HMODULE) * handles.size(), &neededMemory))
      {
         return nullptr;
      }
   }

   for (HMODULE handle : handles)
   {
      void* addr = GetProcAddress(handle, name);

      if (addr != nullptr)
         return addr;
   }

   return nullptr;
#else
   return dlsym(RTLD_DEFAULT, name);
#endif
}

struct EnvSetter final
{
   static const wxString VariableName;
   static const wxString Separator;

   explicit EnvSetter(bool fromUserPathOnly)
   {
      ValueExisted = wxGetEnv(VariableName, &OldValue);

      wxString value;

      for (const wxString& path : FFmpegFunctions::GetSearchPaths(fromUserPathOnly))
      {
         if (!value.empty())
            value += Separator;

         value += path;
      }

      wxSetEnv(VariableName, value);
   };

   ~EnvSetter()
   {
      if (ValueExisted)
         wxSetEnv(VariableName, OldValue);
      else
         wxUnsetEnv(VariableName);
   }

   wxString OldValue;
   bool ValueExisted;
};

#if defined(__WXMSW__)
const wxString EnvSetter::VariableName("PATH");
const wxString EnvSetter::Separator(";");
#elif defined(__WXMAC__)
const wxString EnvSetter::VariableName("DYLD_LIBRARY_PATH");
const wxString EnvSetter::Separator(":");
#else
const wxString EnvSetter::VariableName("LD_LIBRARY_PATH");
const wxString EnvSetter::Separator(":");
#endif

std::vector<wxString> BuildAVFormatPaths(int version)
{
   return {
#if defined(__WXMSW__)
      wxString::Format("avformat-%d.dll", version),
#elif defined(__WXMAC__)
      wxString::Format("ffmpeg.%d.64bit.dylib", version),
      wxString::Format("libavformat.%d.dylib", version),
#else
      wxString::Format("libavformat.so.%d", version)
#endif
};
}

struct FFmpegFunctions::Private final
{
   std::shared_ptr<wxDynamicLibrary> AVFormatLibrary;
   std::shared_ptr<wxDynamicLibrary> AVCodecLibrary;
   std::shared_ptr<wxDynamicLibrary> AVUtilLibrary;

   std::unique_ptr<FFmpegLog> FFmpegLogCallbackSetter;

   AVFormatFactories FormatFactories;
   AVCodecFactories  CodecFactories;
   AVUtilFactories   UtilFactories;

   std::shared_ptr<wxDynamicLibrary> LibraryWithSymbol(const char* symbol, bool fromUserPathOnly) const
   {
      if (AVFormatLibrary->HasSymbol(symbol))
         return AVFormatLibrary;

      void* addr = GetSymbolFromProcess(symbol);

      if (addr == nullptr)
         return nullptr;

      const wxString path = FileNames::PathFromAddr(addr);

      if (path.empty())
         return nullptr;

      return LoadLibrary(wxFileNameFromPath(path), fromUserPathOnly);
   }

   bool Load(FFmpegFunctions& functions, const wxString& path, bool fromUserPathOnly)
   {
      // We start by loading AVFormat
      AVFormatLibrary = LoadLibrary(path, fromUserPathOnly);

      if (AVFormatLibrary == nullptr)
         return false;

      if ((AVCodecLibrary = LibraryWithSymbol("avcodec_version", fromUserPathOnly)) == nullptr)
         return false;

      if ((AVUtilLibrary = LibraryWithSymbol("avutil_version", fromUserPathOnly)) == nullptr)
         return false;

      if (
         !LoadAVFormatFunctions(*AVFormatLibrary, functions) ||
         !LoadAVCodecFunctions(*AVCodecLibrary, functions) ||
         !LoadAVUtilFunctions(*AVUtilLibrary, functions))
         return false;

      if (!FFmpegAPIResolver::Get().GetAVFormatFactories(
             functions.AVFormatVersion.Major, FormatFactories))
         return false;

      if (!FFmpegAPIResolver::Get().GetAVCodecFactories(
             functions.AVCodecVersion.Major, CodecFactories))
         return false;

      AVCodecIDResolver codecResolvers;

      if (!FFmpegAPIResolver::Get().GetAVCodecIDResolver(
             functions.AVCodecVersion.Major, codecResolvers))
         return false;

      functions.GetAVCodecID = codecResolvers.GetAVCodecID;
      functions.GetAudacityCodecID = codecResolvers.GetAudacityCodecID;

      if (!FFmpegAPIResolver::Get().GetAVUtilFactories(
             functions.AVUtilVersion.Major, UtilFactories))
         return false;

      if (functions.avcodec_register_all)
         functions.avcodec_register_all();

      if (functions.av_register_all)
         functions.av_register_all();

      FFmpegLogCallbackSetter =
         UtilFactories.CreateLogCallbackSetter(functions);

      return true;
   }

   std::shared_ptr<wxDynamicLibrary> LoadLibrary(const wxString& libraryName, bool fromUserPathOnly) const
   {
#if defined(__WXMAC__)
      // On macOS dyld reads environment only when application starts.
      // Let's emulate the process manually
      for (const wxString& path : FFmpegFunctions::GetSearchPaths(fromUserPathOnly))
      {
         const wxString fullName = wxFileName(path, libraryName).GetFullPath();

         auto library = std::make_shared<wxDynamicLibrary>(fullName);

         if (library->IsLoaded())
            return library;
      }
#endif
      auto library = std::make_shared<wxDynamicLibrary> (libraryName);

      if (library->IsLoaded())
         return library;

      // Loading has failed.
      // wxLogSysError doesn't report errors correctly on *NIX
#if defined(_WIN32)
      wxLogSysError("Failed to load %s", libraryName.c_str());
#else
      const char* errorString = dlerror();
      wxLogError("Failed to load %s (%s)", libraryName.c_str(), errorString);
#endif
      return {};
   }
};

FFmpegFunctions::FFmpegFunctions()
    : mPrivate(std::make_unique<Private>())
{
}

FFmpegFunctions::~FFmpegFunctions()
{
}

std::shared_ptr<FFmpegFunctions> FFmpegFunctions::Load(bool fromUserPathOnly)
{
   static std::weak_ptr<FFmpegFunctions> weakFunctions;

   auto functions = weakFunctions.lock();

   if (functions)
      return functions;

   std::shared_ptr<FFmpegFunctions> ffmpeg =
      std::make_shared<FFmpegFunctions>();

   const auto supportedVersions =
      FFmpegAPIResolver::Get().GetSuportedAVFormatVersions();

#if !defined(__WXMAC__)
   EnvSetter envSetter(fromUserPathOnly);
#endif

   for (int version : supportedVersions)
   {
      for (const wxString& path : BuildAVFormatPaths(version))
      {
         if (ffmpeg->mPrivate->Load(*ffmpeg, path, fromUserPathOnly))
         {
            weakFunctions = ffmpeg;
            return ffmpeg;
         }
      }
   }

   return {};
}

StringSetting AVFormatPath { L"/FFmpeg/FFmpegLibPath", L"" };

std::vector<wxString> FFmpegFunctions::GetSearchPaths(bool fromUserPathOnly)
{
   std::vector<wxString> paths;

   const wxString userAVFormatFullPath = AVFormatPath.Read();

   if (!userAVFormatFullPath.empty())
   {
      // For some directories, wxPathOnly will fail.
      // For example, if path is `c:\ffmpeg-4.4`
      // wxPathOnly will return `c:\`
      if (wxDirExists(userAVFormatFullPath))
         paths.emplace_back(userAVFormatFullPath);
      else
         paths.emplace_back(wxPathOnly(userAVFormatFullPath));
   }

   if (fromUserPathOnly)
      return paths;

#if defined(__WXMSW__)
   wxRegKey reg(wxT("HKEY_LOCAL_MACHINE\\Software\\FFmpeg for Audacity"));
   wxString path;

   if (reg.Exists())
      reg.QueryValue(wxT("InstallPath"), path);

   if (!path.empty())
      paths.emplace_back(path);

#elif defined(__WXMAC__)
   paths.emplace_back(wxT("/Library/Application Support/audacity/libs"));
   paths.emplace_back(wxT("/usr/local/lib/audacity"));
#endif

   return paths;
}

std::unique_ptr<AVIOContextWrapper> FFmpegFunctions::CreateAVIOContext() const
{
   return mPrivate->FormatFactories.CreateAVIOContextWrapper(*this);
}

std::unique_ptr<AVFormatContextWrapper>
FFmpegFunctions::CreateAVFormatContext() const
{
   return mPrivate->FormatFactories.CreateAVFormatContextWrapper(*this);
}

std::unique_ptr<AVStreamWrapper>
FFmpegFunctions::CreateAVStreamWrapper(AVStream* stream, bool forEncoding) const
{
   return mPrivate->FormatFactories.CreateAVStreamWrapper(*this, stream, forEncoding);
}

std::unique_ptr<AVPacketWrapper> FFmpegFunctions::CreateAVPacketWrapper() const
{
   return mPrivate->CodecFactories.CreateAVPacketWrapper(*this);
}

std::unique_ptr<AVFrameWrapper> FFmpegFunctions::CreateAVFrameWrapper() const
{
   return mPrivate->UtilFactories.CreateAVFrameWrapper(*this);
}

std::unique_ptr<AVInputFormatWrapper>
FFmpegFunctions::CreateAVInputFormatWrapper(
   AVInputFormat* inputFormat) const
{
   return mPrivate->FormatFactories.CreateAVInputFormatWrapper(inputFormat);
}

std::unique_ptr<AVOutputFormatWrapper> FFmpegFunctions::GuessOutputFormat(
   const char* short_name, const char* filename, const char* mime_type)
{
   AVOutputFormat* outputFormat =
      av_guess_format(short_name, filename, mime_type);

   return mPrivate->FormatFactories.CreateAVOutputFormatWrapper(outputFormat);
}

std::unique_ptr<AVOutputFormatWrapper>
FFmpegFunctions::CreateAVOutputFormatWrapper(
   const AVOutputFormat* outputFormat) const
{
   return mPrivate->FormatFactories.CreateAVOutputFormatWrapper(outputFormat);
}

std::unique_ptr<AVCodecWrapper>
FFmpegFunctions::CreateDecoder(AVCodecIDFwd codecID) const
{
   AVCodec* codec = avcodec_find_decoder(codecID);

   if (codec == nullptr)
      return {};

   return mPrivate->CodecFactories.CreateAVCodecWrapper(codec);
}

std::unique_ptr<AVCodecWrapper>
FFmpegFunctions::CreateEncoder(AVCodecIDFwd codecID) const
{
   auto codec = avcodec_find_encoder(codecID);

   if (codec == nullptr)
      return {};

   return mPrivate->CodecFactories.CreateAVCodecWrapper(codec);
}

std::unique_ptr<AVCodecWrapper>
FFmpegFunctions::CreateEncoder(const char* name) const
{
   auto codec = avcodec_find_encoder_by_name(name);

   if (codec == nullptr)
      return {};

   return mPrivate->CodecFactories.CreateAVCodecWrapper(codec);
}

std::unique_ptr<AVCodecContextWrapper>
FFmpegFunctions::CreateAVCodecContextWrapper(AVCodecContext* context) const
{
   return mPrivate->CodecFactories.CreateAVCodecContextWrapper(
      *this, context);
}

std::unique_ptr<AVCodecContextWrapper>
FFmpegFunctions::CreateAVCodecContextWrapperFromCodec(
   std::unique_ptr<AVCodecWrapper> codec) const
{
   if (codec == nullptr)
      return {};

   return mPrivate->CodecFactories.CreateAVCodecContextWrapperFromCodec(
      *this, std::move(codec));
}

const std::vector<const AVOutputFormatWrapper*>&
FFmpegFunctions::GetOutputFormats() const
{
   if (mOutputFormats.empty())
      const_cast<FFmpegFunctions*>(this)->FillOuptutFormatsList();

   return mOutputFormatPointers;
}

const std::vector<const AVCodecWrapper*>& FFmpegFunctions::GetCodecs() const
{
   if (mCodecs.empty())
      const_cast<FFmpegFunctions*>(this)->FillCodecsList();

   return mCodecPointers;
}

std::unique_ptr<AVFifoBufferWrapper>
FFmpegFunctions::CreateFifoBuffer(int size) const
{
   return std::make_unique<AVFifoBufferWrapper>(*this, size);
}

void FFmpegFunctions::FillCodecsList()
{
   mCodecs.clear();
   mCodecPointers.clear();

   if (av_codec_iterate != nullptr)
   {
      const AVCodec* currentCodec = nullptr;
      void* i = 0;
      
      while ((currentCodec = av_codec_iterate(&i)))
      {
         mCodecs.emplace_back(
            mPrivate->CodecFactories.CreateAVCodecWrapper(currentCodec));
      }
   }
   else if (av_codec_next != nullptr)
   {
      AVCodec* currentCodec = nullptr;

      while ((currentCodec = av_codec_next(currentCodec)) != nullptr)
      {
         mCodecs.emplace_back(
            mPrivate->CodecFactories.CreateAVCodecWrapper(currentCodec));
      }
   }

   mCodecPointers.reserve(mCodecs.size());

   for (const auto& codec : mCodecs)
      mCodecPointers.emplace_back(codec.get());
}

void FFmpegFunctions::FillOuptutFormatsList()
{
   mOutputFormats.clear();
   mOutputFormatPointers.clear();

   if (av_muxer_iterate != nullptr)
   {
      const AVOutputFormat* currentFormat = nullptr;
      void* i = 0;

      while ((currentFormat = av_muxer_iterate(&i)))
      {
         mOutputFormats.emplace_back(
            mPrivate->FormatFactories.CreateAVOutputFormatWrapper(
               currentFormat));
      }
   }
   else if (av_oformat_next != nullptr)
   {
      AVOutputFormat* currentFormat = nullptr;

      while ((currentFormat = av_oformat_next(currentFormat)) != nullptr)
      {
         mOutputFormats.emplace_back(
            mPrivate->FormatFactories.CreateAVOutputFormatWrapper(currentFormat));
      }
   }

   mOutputFormatPointers.reserve(mOutputFormats.size());

   for (const auto& format : mOutputFormats)
      mOutputFormatPointers.emplace_back(format.get());
}
