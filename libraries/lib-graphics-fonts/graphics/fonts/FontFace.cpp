/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  FontFace.cpp

  Dmitry Vedenko

**********************************************************************/
#include "FontFace.h"

#include <unordered_map>

#include <ft2build.h>
#include <freetype/freetype.h>
#include <freetype/ftbbox.h>
#include <freetype/ftbitmap.h>
#include <freetype/ftglyph.h>

#include <hb.h>
#include <hb-ft.h>

#include "FontProvider.h"
#include "TextLayout.h"
#include "MemoryX.h"

namespace graphics::fonts
{
namespace
{
template<size_t Divisor, typename T>
T GetPosition(T value) noexcept
{
   return value / Divisor;
}

struct FreeTypeLibrary final
{
   FT_Library library;

   FreeTypeLibrary()
   {
      FT_Init_FreeType(&library);
   }

   ~FreeTypeLibrary()
   {
      FT_Done_FreeType(library);
   }
};

FreeTypeLibrary libraryInstance;

class HarfbuzzFont final
{
public:
   explicit HarfbuzzFont(FT_Face face, uint32_t pixelSize)
       : mFreeTypeFace(face)
       , mPixelSize(pixelSize)
   {
      if (FT_Set_Pixel_Sizes(face, 0, pixelSize))
         return;

      mHeight = face->size->metrics.height / 64;
      mMaxSymbolWidth = face->size->metrics.max_advance / 64;

      mFont = hb_ft_font_create_referenced(face);
   }

   HarfbuzzFont(const HarfbuzzFont&) = delete;

   HarfbuzzFont(HarfbuzzFont&& rhs) noexcept
   {
      *this = std::move(rhs);
   }

   HarfbuzzFont& operator=(const HarfbuzzFont&) = delete;

   HarfbuzzFont& operator=(HarfbuzzFont&& rhs) noexcept
   {
      std::swap(mFreeTypeFace, rhs.mFreeTypeFace);
      std::swap(mFont, rhs.mFont);
      std::swap(mPixelSize, rhs.mPixelSize);
      std::swap(mHeight, rhs.mHeight);
      std::swap(mMaxSymbolWidth, rhs.mMaxSymbolWidth);

      return *this;
   }

   ~HarfbuzzFont()
   {
      if (mFont != nullptr)
         hb_font_destroy(mFont);
   }

   bool IsOk() const noexcept
   {
      return mFont != nullptr;
   }

   std::shared_ptr<TextLayout> CreateTextLayout(std::string_view text, bool hinted) const
   {
      if (text.empty())
      {
         return std::make_shared<TextLayout>(
            std::vector<TextLayoutSymbol>(), 0, 0);
      }

      if (FT_Set_Pixel_Sizes(mFreeTypeFace, 0, mPixelSize))
         return {};

      auto buffer = hb_buffer_create();

      if (buffer == nullptr)
         return nullptr;

      auto bufferDeleter = finally([buffer]() { hb_buffer_destroy(buffer); });

      hb_buffer_add_utf8(buffer, text.data(), text.size(), 0, text.size());
      hb_buffer_guess_segment_properties(buffer);

      hb_shape(mFont, buffer, nullptr, 0);

      const uint32_t glyphLength = hb_buffer_get_length(buffer);
      auto info = hb_buffer_get_glyph_infos(buffer, nullptr);

      std::vector<TextLayoutSymbol> symbols;
      symbols.reserve(glyphLength);

      int32_t currentX = 0;
      int32_t currentY = 0;

      for (uint32_t symbolIndex = 0; symbolIndex < glyphLength; ++symbolIndex)
      {
         const auto metrics = GetCachedMetrics(info[symbolIndex].codepoint, hinted);

         symbols.emplace_back(TextLayoutSymbol {
            info[symbolIndex].codepoint, info[symbolIndex].cluster, currentX, currentY });

         if (symbolIndex != (glyphLength - 1))
         {
            FT_Vector kerning {};

            if (FT_HAS_KERNING(mFreeTypeFace))
            {
               FT_Get_Kerning(
                  mFreeTypeFace, info[symbolIndex].codepoint,
                  info[symbolIndex + 1].codepoint, FT_KERNING_DEFAULT,
                  &kerning);
            }

            const auto secondSymbolMetrics = GetCachedMetrics(info[symbolIndex + 1].codepoint, hinted);

            const int delta = metrics.rsb - secondSymbolMetrics.lsb;

            if (delta >= 32)
            {
               kerning.x = -64;
            }
            else if (delta < -32)
            {
               kerning.x = 64;
            }

            currentX += kerning.x / 64;
            currentY += kerning.y / 64;
         }

         currentX += metrics.advance.x;
         currentY += metrics.advance.y;
      }

      if (currentY == 0)
      {
         currentX = currentX;
         currentY = mHeight;
      }
      else if (currentX == 0)
      {
         currentX = mMaxSymbolWidth;
         currentY = currentY;
      }

      return std::make_shared<TextLayout>(
         std::move(symbols), currentX, currentY);
   }

private:
   struct GlyphMetrics final
   {
      PointType<int32_t> advance { 0, 0 };
      int32_t lsb { 0 };
      int32_t rsb { 0 };
   };

   GlyphMetrics GetCachedMetrics(hb_codepoint_t codePoint, bool hinted) const
   {
      auto& metricsCache = mGlyphMetrics[hinted ? 0 : 1];
      
      auto it = metricsCache.find(codePoint);

      if (it != metricsCache.end())
         return it->second;

      auto metrics = ComputeMetrics(codePoint, hinted);

      metricsCache.emplace(codePoint, metrics);

      return metrics;
   }

   GlyphMetrics ComputeMetrics(hb_codepoint_t codePoint, bool hinted) const noexcept
   {
      if (codePoint == 0)
         return {};

      FT_Int32 flags = FT_LOAD_RENDER;

      if (hinted)
         flags |= FT_LOAD_FORCE_AUTOHINT;
      else
         flags |= FT_LOAD_NO_HINTING | FT_LOAD_NO_AUTOHINT;

      if (FT_Load_Glyph(mFreeTypeFace, codePoint, flags))
         return {};

      return { { int(mFreeTypeFace->glyph->advance.x >> 6),
                 int(mFreeTypeFace->glyph->advance.y >> 6) },
               int(mFreeTypeFace->glyph->lsb_delta),
               int(mFreeTypeFace->glyph->rsb_delta) };
   }

   FT_Face mFreeTypeFace { nullptr };
   hb_font_t* mFont { nullptr };
   uint32_t mPixelSize { 0 };

   uint32_t mHeight { 0 };
   uint32_t mMaxSymbolWidth { 0 };


   mutable std::unordered_map<hb_codepoint_t, GlyphMetrics> mGlyphMetrics[2];
};
} // namespace

class FontFace::FreeTypeFace final
{
public:
   FreeTypeFace(std::unique_ptr<FontStream> fontStream)
       : mFontStream(std::move(fontStream))
   {
      mFreeTypeStream.descriptor.pointer = this;
      mFreeTypeStream.size = 0x7FFFFFFF;
      mFreeTypeStream.read = &FreeTypeFace::StreamRead;
      mFreeTypeStream.close = &FreeTypeFace::StreamClose;

      mFreeTypeOpenArgs.flags = FT_OPEN_STREAM;
      mFreeTypeOpenArgs.stream = &mFreeTypeStream;

      if (FT_Open_Face(libraryInstance.library, &mFreeTypeOpenArgs, 0, &mFreeTypeFace))
         return;
   }

   ~FreeTypeFace()
   {
      mHarfbuzzFonts.clear();

      if (mFreeTypeFace != nullptr)
         FT_Done_Face(mFreeTypeFace);
   }

   const PainterFont::Metrics& GetMetrics(FontSize fontSize)
   {
      const auto pixelSize = GetPixelSize(fontSize);

      auto it = mMetricsCache.find(pixelSize);

      if (it != mMetricsCache.end())
         return it->second;

      if (FT_Set_Pixel_Sizes(mFreeTypeFace, 0, pixelSize))
         return mDefaultMetrics;

      auto metrics = mFreeTypeFace->size->metrics;

      PainterFont::Metrics result {
         static_cast<float>(metrics.ascender / 64),
         static_cast<float>((-metrics.descender) / 64),
         static_cast<float>((metrics.height - metrics.ascender + metrics.descender) / 64),
         static_cast<float>(metrics.height / 64)
      };

      return mMetricsCache.emplace(pixelSize, result).first->second;
   }

   bool IsOk () const noexcept
   {
      return mFontStream != nullptr;
   }

   const HarfbuzzFont* GetHarfbuzzfont(uint32_t pixelSize)
   {
      auto it = mHarfbuzzFonts.find(pixelSize);

      if (it != mHarfbuzzFonts.end())
         return &it->second;

      HarfbuzzFont font(mFreeTypeFace, pixelSize);

      if (!font.IsOk())
         return nullptr;

      return &mHarfbuzzFonts.emplace(pixelSize, std::move(font)).first->second;
   }

   FontSymbol GetFontSymbol(uint32_t pixelSize, uint32_t codepoint, bool hinted) const
   {
      if (FT_Set_Pixel_Sizes(mFreeTypeFace, 0, pixelSize))
         return {};

      const auto charIndex = codepoint;

      if (charIndex == 0)
         return {};

      FT_Int32 flags = FT_LOAD_RENDER;

      if (hinted)
         flags |= FT_LOAD_FORCE_AUTOHINT;
      else
         flags |= FT_LOAD_NO_HINTING | FT_LOAD_NO_AUTOHINT;

      if (FT_Load_Glyph(mFreeTypeFace, charIndex, flags))
         return {};

      FT_Glyph glyph;

      if (FT_Get_Glyph(mFreeTypeFace->glyph, &glyph))
         return {};

      auto doneGlyph = finally([glyph]() { FT_Done_Glyph(glyph); });

      if (mFreeTypeFace->glyph->format != FT_GLYPH_FORMAT_BITMAP)
      {
         if (FT_Glyph_To_Bitmap(&glyph, FT_RENDER_MODE_NORMAL, nullptr, true))
            return {};
      }

      auto bitmapGlyph = reinterpret_cast<FT_BitmapGlyph>(glyph);

      const auto srcWidth = bitmapGlyph->bitmap.width;
      const auto srcHeight = bitmapGlyph->bitmap.rows;
      const auto tgtWidth = srcWidth + 2;
      const auto tgtHeight = srcHeight + 2;

      FontSymbol symbol { std::vector<Color>(tgtWidth * tgtHeight),
                          tgtWidth, tgtHeight,
                          bitmapGlyph->left - 1,
                          bitmapGlyph->top + 1 };

      auto src_ptr = bitmapGlyph->bitmap.buffer;

      if (bitmapGlyph->bitmap.pixel_mode == FT_PIXEL_MODE_GRAY)
      {
         for (uint32_t row = 0; row < srcHeight; row++)
         {
            for (uint32_t col = 0; col < srcWidth; ++col)
               SetColor(symbol, col, row, Color(255, 255, 255, src_ptr[col]));

            src_ptr += bitmapGlyph->bitmap.pitch;
         }
      }
      else if (bitmapGlyph->bitmap.pixel_mode == FT_PIXEL_MODE_MONO)
      {
         for (uint32_t row = 0; row < srcHeight; row++)
         {
            for (uint32_t col = 0; col < srcWidth; ++col)
            {
               const auto srcByte = src_ptr[col / 8];
               const auto srcBit = col % 8;

               const auto alpha =
                  (srcByte & (1 << (7 - srcBit))) == 0 ? 0 : 255;

               SetColor(symbol, col, row, Color(255, 255, 255, alpha));
            }

            src_ptr += bitmapGlyph->bitmap.pitch;
         }
      }
      else if (bitmapGlyph->bitmap.pixel_mode == FT_PIXEL_MODE_BGRA)
      {
         for (uint32_t row = 0; row < srcHeight; row++)
         {
            for (uint32_t col = 0; col < srcWidth; ++col)
            {
               SetColor(
                  symbol, col, row,
                  Color(
                     src_ptr[4 * col + 2], src_ptr[4 * col + 1],
                     src_ptr[4 * col + 0], src_ptr[4 * col + 3]));
            }

            src_ptr += bitmapGlyph->bitmap.pitch;
         }
      }
      else
      {
         return {};
      }

      return symbol;
   }



private:
   void SetColor(FontSymbol& symbol, uint32_t col, uint32_t row, Color color)
      const noexcept
   {
      symbol.bitmap[(row + 1) * symbol.width + (col + 1)] = color;
   }

   static unsigned long StreamRead(
      FT_Stream stream, unsigned long offset, unsigned char* buffer,
      unsigned long size)
   {
      auto face = static_cast<FreeTypeFace*>(stream->descriptor.pointer);
      return face->mFontStream->StreamRead(offset, buffer, size);
   }

   static void StreamClose(FT_Stream stream)
   {
      auto face = static_cast<FreeTypeFace*>(stream->descriptor.pointer);
      face->mFontStream = {};
   }

   FT_Face mFreeTypeFace { nullptr };

   using HarfbuzzFonts = std::unordered_map<uint32_t, HarfbuzzFont>;
   HarfbuzzFonts mHarfbuzzFonts;

   std::unique_ptr<FontStream> mFontStream;

   FT_StreamRec mFreeTypeStream {};
   FT_Open_Args mFreeTypeOpenArgs {};

   using MetricsCache = std::unordered_map<uint32_t, PainterFont::Metrics>;
   MetricsCache mMetricsCache;
   PainterFont::Metrics mDefaultMetrics;
};

FontFace::FontFace(
   FontProvider& provider, const FontInfo& info, size_t libraryIndex)
    : mFontInfo(info)
    , mLibraryIndex(libraryIndex)
{
   auto stream = provider.GetFontStream(info);

   if (stream == nullptr)
      return;

   mFreetypeFace = std::make_unique<FreeTypeFace>(std::move(stream));

   if (!mFreetypeFace->IsOk())
      mFreetypeFace = {};
}

FontFace::~FontFace()
{
}

std::shared_ptr<TextLayout>
FontFace::CreateTextLayout(FontSize fontSize, std::string_view text, bool hinted) const
{
   if (mFreetypeFace == nullptr)
      return nullptr;

   auto hbfont = mFreetypeFace->GetHarfbuzzfont(GetPixelSize(fontSize));

   if (hbfont == nullptr)
      return nullptr;

   return hbfont->CreateTextLayout(text, hinted);
}

FontSymbol FontFace::GetFontSymbol(uint32_t pixelSize, uint32_t codepoint, bool hinted) const
{
   return mFreetypeFace->GetFontSymbol(pixelSize, codepoint, hinted);
}

float FontFace::GetPointSize() const noexcept
{
   return mFontInfo.GetPointSize();
}

const PainterFont::Metrics& FontFace::GetMetrics(FontSize size) const noexcept
{
   return mFreetypeFace->GetMetrics(size);
}

const FontInfo& FontFace::GetFontInfo() const noexcept
{
   return mFontInfo;
}

bool FontFace::IsOk() const noexcept
{
   return mFreetypeFace != nullptr;
}

size_t FontFace::GetLibraryIndex() const noexcept
{
   return mLibraryIndex;
}

uint32_t FontFace::GetPixelSize(FontSize size) noexcept
{
   return std::floor(size.pointSize * size.dpi / 72.0);
}

uint32_t FontFace::BaseDPI() noexcept
{
   return 96;
}

} // namespace graphics::fonts
