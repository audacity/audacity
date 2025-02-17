/**********************************************************************

  Audacity: A Digital Audio Editor

  AVDictionaryWrapper.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <string_view>
#include <string>
#include <wx/string.h>

struct FFmpegFunctions;
typedef struct AVDictionary AVDictionary;

#define DICT_MATCH_CASE 1
#define DICT_IGNORE_SUFFIX 2

class FFMPEG_SUPPORT_API AVDictionaryWrapper
{
public:
    //! Unlike the other FFmpeg wrapper classes, this one is copyable.
    AVDictionaryWrapper(const AVDictionaryWrapper& rhs) noexcept;
    AVDictionaryWrapper(AVDictionaryWrapper&& rhs) noexcept;

    // We expect that &mFFmpeg == rhs.mFFmpeg
    AVDictionaryWrapper& operator=(const AVDictionaryWrapper& rhs) noexcept;
    AVDictionaryWrapper& operator=(AVDictionaryWrapper&& rhs) noexcept;

    explicit AVDictionaryWrapper(const FFmpegFunctions& ffmpeg) noexcept;
    explicit AVDictionaryWrapper(const FFmpegFunctions& ffmpeg, AVDictionary* rhs) noexcept;

    AVDictionary* GetWrappedValue() noexcept;
    const AVDictionary* GetWrappedValue() const noexcept;

    virtual ~AVDictionaryWrapper();

    void Set(const std::string_view& key, const std::string& value, int flags = 0) noexcept;
    void Set(const std::string_view& key, const wxString& value, int flags = 0) noexcept;
    void Set(const std::string_view& key, const char* value, int flags = 0) noexcept;

    template<typename T>
    void Set(const std::string_view& key, const T& value, int flags = 0) noexcept
    {
        Set(key, std::to_string(value), flags);
    }

    std::string_view Get(const std::string_view& key, const std::string_view& defaultValue, int flags = 0) const;
    bool HasValue(const std::string_view& key, int flags = 0) const noexcept;

    AVDictionary* Release() noexcept;
protected:
    const FFmpegFunctions& mFFmpeg;
    AVDictionary* mAVDictionary { nullptr };
};
