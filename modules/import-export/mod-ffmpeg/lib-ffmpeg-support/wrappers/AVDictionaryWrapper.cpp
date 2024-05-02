/**********************************************************************

  Audacity: A Digital Audio Editor

  AVDictionaryWrapper.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AVDictionaryWrapper.h"

#include "FFmpegFunctions.h"
#include "FFmpegTypes.h"

#include <utility>

AVDictionaryWrapper::AVDictionaryWrapper(
   const FFmpegFunctions& ffmpeg) noexcept
    : mFFmpeg(ffmpeg)
{
}

AVDictionaryWrapper::AVDictionaryWrapper(
   const FFmpegFunctions& ffmpeg, AVDictionary* rhs) noexcept
    : mFFmpeg(ffmpeg)
{
   if (rhs != nullptr)
      mFFmpeg.av_dict_copy(&mAVDictionary, rhs, 0);
}

AVDictionaryWrapper::AVDictionaryWrapper(
   const AVDictionaryWrapper& rhs) noexcept
    : AVDictionaryWrapper(rhs.mFFmpeg, rhs.mAVDictionary)
{
}

AVDictionaryWrapper::AVDictionaryWrapper(
   AVDictionaryWrapper&& rhs) noexcept
    : mFFmpeg(rhs.mFFmpeg)
{
   *this = std::move(rhs);
}

AVDictionaryWrapper&
AVDictionaryWrapper::operator=(const AVDictionaryWrapper& rhs) noexcept
{
   assert(&mFFmpeg == &rhs.mFFmpeg);

   if (rhs.mAVDictionary != nullptr)
      mFFmpeg.av_dict_copy(&mAVDictionary, rhs.mAVDictionary, 0);

   return *this;
}

AVDictionaryWrapper&
AVDictionaryWrapper::operator=(AVDictionaryWrapper&& rhs) noexcept
{
   assert(&mFFmpeg == &rhs.mFFmpeg);

   std::swap(mAVDictionary, rhs.mAVDictionary);

   return *this;
}

 AVDictionary* AVDictionaryWrapper::GetWrappedValue() noexcept
{
   return mAVDictionary;
}

const AVDictionary* AVDictionaryWrapper::GetWrappedValue() const noexcept
{
   return mAVDictionary;
}

AVDictionaryWrapper::~AVDictionaryWrapper()
{
   mFFmpeg.av_dict_free(&mAVDictionary);
}

void AVDictionaryWrapper::Set(
   const std::string_view& key, const std::string& value,
   int flags) noexcept
{
   mFFmpeg.av_dict_set(&mAVDictionary, key.data(), value.data(), flags);
}

void AVDictionaryWrapper::Set(
   const std::string_view& key, const wxString& value, int flags) noexcept
{
   mFFmpeg.av_dict_set(&mAVDictionary, key.data(), value.ToUTF8().data(), flags);
}

void AVDictionaryWrapper::Set(
   const std::string_view& key, const char* value, int flags) noexcept
{
   mFFmpeg.av_dict_set(
      &mAVDictionary, key.data(), value, flags);
}

std::string_view AVDictionaryWrapper::Get(
   const std::string_view& key, const std::string_view& defaultValue,
   int flags) const
{
   if (mAVDictionary == nullptr)
      return defaultValue;

   AudacityAVDictionaryEntry* entry =
      mFFmpeg.av_dict_get(mAVDictionary, key.data(), nullptr, flags);

   if (entry != nullptr)
      return entry->value;

   return defaultValue;
}

bool AVDictionaryWrapper::HasValue(
   const std::string_view& key, int flags) const noexcept
{
   if (mAVDictionary == nullptr)
      return false;

   AudacityAVDictionaryEntry* entry =
      mFFmpeg.av_dict_get(mAVDictionary, key.data(), nullptr, flags);

   return entry != nullptr;
}

AVDictionary* AVDictionaryWrapper::Release() noexcept
{
   auto temp = mAVDictionary;
   mAVDictionary = nullptr;

   return temp;
}
