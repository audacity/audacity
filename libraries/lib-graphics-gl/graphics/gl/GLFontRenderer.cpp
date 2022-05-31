/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  GLFontRenderer.cpp

  Dmitry Vedenko

**********************************************************************/

#include "GLFontRenderer.h"

#include <cstdint>
#include <unordered_map>

#include "graphics/fonts/Font.h"
#include "graphics/fonts/FontFace.h"
#include "graphics/fonts/TextLayout.h"

#include "Context.h"
#include "PaintTarget.h"
#include "Texture.h"
#include "GLRenderer.h"

#include "MemoryX.h"


namespace graphics::gl
{
namespace
{

struct SymbolKey final
{
   size_t faceIndex;
   uint32_t pixelSize;

   uint32_t codePoint;

   bool hinted;
};

struct CachedSymbol final
{
   Texture* texture { nullptr };

   uint32_t width { 0 };
   uint32_t height { 0};

   int32_t left { 0 };
   int32_t top { 0 };

   Texture::TextureCoords uv {};
};

struct SymbolKeyHash final
{
   size_t operator()(const SymbolKey& key) const noexcept
   {
      return HashCombiner {} (key.faceIndex, key.pixelSize, key.codePoint, key.hinted);
   }
};

bool operator==(const SymbolKey& lhs, const SymbolKey& rhs) noexcept
{
   return lhs.faceIndex == rhs.faceIndex &&
          lhs.pixelSize == rhs.pixelSize &&
          lhs.codePoint == rhs.codePoint &&
          lhs.hinted == rhs.hinted;
}

constexpr uint32_t textureSize = 256;

struct TextureCacheItem final
{
   TextureCacheItem(GLRenderer& renderer)
       : texture(std::make_shared<Texture>(renderer, textureSize, textureSize, PainterImageFormat::RGBA8888, false))
   {
   }

   CachedSymbol TryUpload(const fonts::FontSymbol& symbol)
   {
      if (currentRow + symbol.height > textureSize)
         return {};
      
      uint32_t col = nextColumn + symbol.width;

      if (col > textureSize)
      {
         nextColumn = 0;
         col = symbol.width;

         if (nextRow + symbol.height > textureSize)
            return {};

         currentRow = nextRow;
      }

      auto coords = texture->Update(
         { { nextColumn, currentRow }, { symbol.width, symbol.height } },
         symbol.bitmap.data());

      if (coords.left == coords.right)
         return {};

      CachedSymbol cachedSymbol { texture.get(), symbol.width, symbol.height,
                                  symbol.left,   symbol.top,   coords };

      nextColumn = col;
      nextRow = std::max(nextRow, currentRow + symbol.height);

      return cachedSymbol;
   }

   std::shared_ptr<Texture> texture;

   uint32_t nextColumn { 0 };
   
   uint32_t currentRow { 0 };
   uint32_t nextRow { 0 };
};

struct TextureCache final
{
   TextureCache(GLRenderer& _renderer)
       : renderer(_renderer)
   {
   }
   
   CachedSymbol Upload(const fonts::FontSymbol& symbol)
   {
      if (textures.empty())
         textures.emplace_back(renderer);
      
      auto cachedSymbol = textures.back().TryUpload(symbol);

      if (cachedSymbol.texture != nullptr)
         return cachedSymbol;

      textures.emplace_back(renderer);

      return textures.back().TryUpload(symbol);
   }
   
   void UpdateGPUCache()
   {
      for (auto& texture : textures)
      {
         if (texture.texture->IsDirty())
            texture.texture->PerformUpdate(renderer.GetResourceContext());
      }
   }
   
   GLRenderer& renderer;
   std::vector<TextureCacheItem> textures;
};

} // namespace

class GLFontRenderer::SymbolsCache final
{
public:
   explicit SymbolsCache(GLRenderer& renderer)
       : mRenderer(renderer)
       , mTextureCache(renderer)
   {
   }

   CachedSymbol GetSymbol(const fonts::FontFace& face, uint32_t pixelSize, uint32_t codePoint, bool hinted)
   {
      auto key =
         SymbolKey { face.GetLibraryIndex(), pixelSize, codePoint, hinted };
      
      auto it = mSymbols.find(key);
      if (it != mSymbols.end())
         return it->second;

      auto symbol = CreateSymbol(face, pixelSize, codePoint, hinted);
      mSymbols.emplace(key, symbol);
      return symbol;
   }

   void UpdateGPUCache()
   {
      mTextureCache.UpdateGPUCache();
   }

private:
   CachedSymbol CreateSymbol(
      const fonts::FontFace& face, uint32_t pixelSize, uint32_t codePoint,
      bool hinted)
   {
      auto fontSymbol = face.GetFontSymbol(pixelSize, codePoint, hinted);

      if (fontSymbol.bitmap.empty())
         return {};

      return mTextureCache.Upload(fontSymbol);
   }
   
   GLRenderer& mRenderer;

   using Symbols = std::unordered_map<SymbolKey, CachedSymbol, SymbolKeyHash>;
   Symbols mSymbols;
   
   TextureCache mTextureCache;
};

GLFontRenderer::GLFontRenderer(GLRenderer& renderer)
    : mRenderer(renderer)
    , mSymbolsCache(std::make_unique<SymbolsCache>(renderer))
{
}

GLFontRenderer::~GLFontRenderer()
{
}

void GLFontRenderer::SetCurrentPaintTarget(PaintTarget& paintTarget, Context& context)
{
   mCurrentPaintTarget = &paintTarget;
   mContext = &context;
}

void GLFontRenderer::ResetPaintTarget()
{
   mCurrentPaintTarget = nullptr;
   mContext = nullptr;
}

uint32_t GLFontRenderer::GetDPI() const noexcept
{
   return mContext != nullptr ? mContext->GetDPI() : 96;
}

void GLFontRenderer::Draw(
   const fonts::Font& font, const fonts::TextLayout& layout, Color textColor)
{
   if (mCurrentPaintTarget == nullptr)
      return;

   auto& symbols = layout.GetSymbols();

   auto& fontFace = font.GetFontFace();

   const fonts::FontSize fontSize { font.GetFontSize(), mContext->GetDPI() };

   const auto pixelSize =
      fontFace.GetPixelSize(fontSize);

   Texture* lastTexture = nullptr;

   const auto metrics = fontFace.GetMetrics(fontSize);
   const auto yoffset = metrics.Ascent;

   const float scaleFactor = mContext->GetScaleFactor();

   for (auto symbol : symbols)
   {
      auto cachedSymbol = mSymbolsCache->GetSymbol(
         fontFace, pixelSize, symbol.codepoint, mHintingEnabled);

      if (cachedSymbol.texture == nullptr)
         continue;

      if (lastTexture != cachedSymbol.texture)
      {
         lastTexture = cachedSymbol.texture;
         mContext->BindTexture(lastTexture->shared_from_this(), 0);
      }

      const Point topLeft { static_cast<float>(symbol.x + cachedSymbol.left),
                            static_cast<float>(symbol.y + yoffset - cachedSymbol.top) };

      const Point topRight =
         topLeft + Point { static_cast<float>(cachedSymbol.width), 0.0f };

      const Point bottomRight =
         topLeft + Point { static_cast<float>(cachedSymbol.width),
                           static_cast<float>(cachedSymbol.height) };

      const Point bottomLeft =
         topLeft + Point { 0.0f, static_cast<float>(cachedSymbol.height) };

      const Vertex vertices[] = {
         { scaleFactor * topLeft,     { cachedSymbol.uv.left,  cachedSymbol.uv.top    }, textColor, Colors::Black.WithAlpha(0) },
         { scaleFactor * topRight,    { cachedSymbol.uv.right, cachedSymbol.uv.top    }, textColor, Colors::Black.WithAlpha(0) },
         { scaleFactor * bottomRight, { cachedSymbol.uv.right, cachedSymbol.uv.bottom }, textColor, Colors::Black.WithAlpha(0) },
         { scaleFactor * bottomLeft,  { cachedSymbol.uv.left,  cachedSymbol.uv.bottom }, textColor, Colors::Black.WithAlpha(0) },
      };

      const uint16_t indices[] = { 0, 1, 2, 2, 3, 0 };

      mCurrentPaintTarget->Append(GLenum::TRIANGLES, vertices, 4, indices, 6);
   }
}

void GLFontRenderer::UpdateGPUCache()
{
   mSymbolsCache->UpdateGPUCache();
}

void GLFontRenderer::SetScale(float scale) noexcept
{
   mScale = scale;
   SetHintingEnabled(mHintingEnabled);
}

void GLFontRenderer::SetHintingEnabled(bool hinting) noexcept
{
   mHintingEnabled = hinting && std::abs(mScale - 1.0f) <
                                   std::numeric_limits<float>::epsilon();
}

} // namespace graphics::gl
