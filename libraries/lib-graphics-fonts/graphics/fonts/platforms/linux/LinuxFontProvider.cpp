/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  LinuxFontProvider.h

  Dmitry Vedenko

**********************************************************************/
#include "LinuxFontProvider.h"

#include <cstdint>
#include <cstdio>
#include <string>

#include <fontconfig/fontconfig.h>

#include "graphics/fonts/FontLibrary.h"

#include "MemoryX.h"

namespace graphics::fonts::platforms::linux_like
{
namespace
{
auto val = ([]() { FontLibrary::SetFontProvider(std::make_unique<LinuxFontProvider>()); return true; })();

class FileFontStream : public FontStream
{
public:
   FileFontStream(const char* fileName, long faceIndex)
   {
      mFile = fopen(fileName, "rb");
   }

   ~FileFontStream()
   {
      if (mFile != nullptr)
         fclose(mFile);
   }

   bool IsOk()
   {
      return mFile != nullptr;
   }

   unsigned long StreamRead(
      unsigned long offset, unsigned char* buffer, unsigned long count) override
   {
      if (count == 0)
         return fseek(mFile, offset, SEEK_SET);

      if (fseek(mFile, offset, SEEK_SET))
         return 0;

      return fread(buffer, 1, count, mFile);
   }

   long GetFaceIndex() const override
   {
      return mFaceIndex;
   }
private:
   FILE* mFile { nullptr };
   long mFaceIndex { 0 };
};

class FontconfigPattern final
{
public:
   FontconfigPattern()
       : mPattern(FcPatternCreate())
   {
   }

   explicit FontconfigPattern(FcPattern* pattern)
       : mPattern(pattern)
   {
   }

   ~FontconfigPattern()
   {
      FcPatternDestroy(mPattern);
   }

   FontconfigPattern(const FontconfigPattern&) = delete;
   FontconfigPattern& operator=(const FontconfigPattern&) = delete;

   FontconfigPattern(FontconfigPattern&& rhs) noexcept
   {
      std::swap(mPattern, rhs.mPattern);
   }

   FontconfigPattern& operator=(FontconfigPattern&& rhs) noexcept
   {
      std::swap(mPattern, rhs.mPattern);
      return *this;
   }

   bool IsOk() const noexcept
   {
      return mPattern != nullptr;
   }

   FontconfigPattern Match(FcConfig* config) const
   {
      if (mPattern == nullptr)
         return FontconfigPattern(nullptr);

      FcConfigSubstitute(config, mPattern, FcMatchPattern);
      FcDefaultSubstitute(mPattern);

      FcResult matchResult;
      auto result = FcFontMatch(config, mPattern, &matchResult);

      if (result == nullptr)
         return FontconfigPattern(nullptr);

      return FontconfigPattern(result);
   }

   FontconfigPattern& Add(const char* object, const char* value)
   {
      if (mPattern == nullptr)
         return *this;

      FcValue fcval {};

      fcval.type = FcTypeString;
      fcval.u.s = reinterpret_cast<const FcChar8*>(value);

      FcPatternAdd(mPattern, object, fcval, FcTrue);
      return *this;
   }

   FontconfigPattern& Add(const char* object, int value)
   {
      if (mPattern == nullptr)
         return *this;

      FcValue fcval {};

      fcval.type = FcTypeInteger;
      fcval.u.i = value;

      FcPatternAdd(mPattern, object, fcval, FcTrue);
      return *this;
   }

   std::string GetString(const char* object) const
   {
      if (mPattern == nullptr)
         return {};

      FcValue fcval {};

      if (
         FcPatternGet(mPattern, object, 0, &fcval) != FcResultMatch ||
         fcval.type != FcTypeString)
         return {};

      return { reinterpret_cast<const char*>(fcval.u.s) };
   }

   int GetInt(const char* object) const
   {
      if (mPattern == nullptr)
         return {};

      FcValue fcval {};

      if (
         FcPatternGet(mPattern, object, 0, &fcval) != FcResultMatch ||
         fcval.type != FcTypeInteger)
         return {};

      return fcval.u.i;
   }

private:
   FcPattern* mPattern { nullptr };
};

int GetFcWeight(FontWeight weight) noexcept
{
   switch (weight)
   {
   case FontWeight::Thin:
      return FC_WEIGHT_THIN;
   case FontWeight::ExtraLight:
      return FC_WEIGHT_ULTRALIGHT;
   case FontWeight::Light:
      return FC_WEIGHT_LIGHT;
   case FontWeight::Normal:
      return FC_WEIGHT_NORMAL;
   case FontWeight::Medium:
      return FC_WEIGHT_MEDIUM;
   case FontWeight::SemiBold:
      return FC_WEIGHT_SEMIBOLD;
   case FontWeight::Bold:
      return FC_WEIGHT_BOLD;
   case FontWeight::ExtraBold:
      return FC_WEIGHT_ULTRABOLD;
   case FontWeight::Heavy:
      return FC_WEIGHT_HEAVY;
   case FontWeight::ExtraBlack:
      return FC_WEIGHT_EXTRABLACK;
   }

   return FC_WEIGHT_NORMAL;
}

int GetFcSlant(FontStyle style) noexcept
{
   switch (style)
   {
   case FontStyle::Normal:
      return FC_SLANT_ROMAN;
   case FontStyle::Italic:
      return FC_SLANT_ITALIC;
   case FontStyle::Oblique:
      return FC_SLANT_OBLIQUE;
   }

   return FC_SLANT_ROMAN;
}

int GetFcWidth(FontStretch width) noexcept
{
   switch (width)
   {
   case FontStretch::UltraCondensed:
      return FC_WIDTH_ULTRACONDENSED;
   case FontStretch::ExtraCondensed:
      return FC_WIDTH_EXTRACONDENSED;
   case FontStretch::Condensed:
      return FC_WIDTH_CONDENSED;
   case FontStretch::SemiCondensed:
      return FC_WIDTH_SEMICONDENSED;
   case FontStretch::Normal:
      return FC_WIDTH_NORMAL;
   case FontStretch::SemiExpanded:
      return FC_WIDTH_SEMIEXPANDED;
   case FontStretch::Expanded:
      return FC_WIDTH_EXPANDED;
   case FontStretch::ExtraExpanded:
      return FC_WIDTH_EXTRAEXPANDED;
   case FontStretch::UltraExpanded:
      return FC_WIDTH_ULTRAEXPANDED;
   }

   return FC_WIDTH_NORMAL;
}

} // namespace

class LinuxFontProvider::Fontconfig final
{
public:
   Fontconfig()
   {
      FcInit();
      mFcConfig = FcInitLoadConfigAndFonts();
   }

   ~Fontconfig()
   {
      FcConfigDestroy(mFcConfig);
      // It is not safe to call FcFini() here, because wxWidgets may still be
      // using fontconfig.
      // FcFini();
   }

   std::unique_ptr<FontStream> GetFontStream(const FontInfo& fontInfo)
   {
      FontconfigPattern matchedFont =
         FontconfigPattern()
            .Add(FC_FAMILY, fontInfo.GetFaceName().data())
            .Add(FC_SLANT, GetFcSlant(fontInfo.GetFontStyle()))
            .Add(FC_WEIGHT, GetFcWeight(fontInfo.GetFontWeight()))
            .Add(FC_WIDTH, GetFcWidth(fontInfo.GetFontStretch()))
            .Match(mFcConfig);

      if(!matchedFont.IsOk())
         return {};

      auto fontFile = matchedFont.GetString(FC_FILE);
      auto faceIndex = matchedFont.GetInt(FC_INDEX);

      return std::make_unique<FileFontStream>(fontFile.c_str(), faceIndex);
   }

private:
   FcConfig* mFcConfig;
};

LinuxFontProvider::LinuxFontProvider()
    : mFontConfig(std::make_unique<Fontconfig>())
{
}

LinuxFontProvider::~LinuxFontProvider()
{
}

std::unique_ptr<FontStream>
LinuxFontProvider::GetFontStream(const FontInfo& fontInfo)
{
   return mFontConfig->GetFontStream(fontInfo);
}

} // namespace graphics::fonts::platforms::linux_like
