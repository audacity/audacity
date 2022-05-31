/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  MacosFontProvider.h

  Implementation is base on https://gist.github.com/Jyczeal/1892760
  which is based on some legacy code from Skia.

  Dmitry Vedenko

**********************************************************************/
#include "MacosFontProvider.h"

#include <cassert>
#include <cstdint>
#include <string>

#include <CoreGraphics/CoreGraphics.h>
#include <CoreText/CoreText.h>
#include <Cocoa/Cocoa.h>

#include "graphics/fonts/FontLibrary.h"

#include "CFResources.h"

namespace graphics::fonts::platforms::macos
{
namespace
{
auto val = ([]() { FontLibrary::SetFontProvider(std::make_unique<MacosFontProvider>()); return true; })();

struct SFNTHeader final
{
   int32_t fVersion;
   uint16_t fNumTables;
   uint16_t fSearchRange;
   uint16_t fEntrySelector;
   uint16_t fRangeShift;
};

struct SFNTTableEntry final
{
   uint32_t fTag;
   uint32_t fCheckSum;
   uint32_t fOffset;
   uint32_t fLength;
};

struct CGTableEntryHelper final
{
   CGTableEntryHelper() = default;
   CGTableEntryHelper(const CGTableEntryHelper&) = delete;
   CGTableEntryHelper(CGTableEntryHelper&&) = default;
   CGTableEntryHelper& operator=(const CGTableEntryHelper&) = delete;
   CGTableEntryHelper& operator=(CGTableEntryHelper&&) = default;

   CGTableEntryHelper(CGFontRef font, uint32_t tag)
       : entryTag(tag)
   {
      entryData.reset(CGFontCopyTableForTag(font, tag));
      size = entryData != nullptr ? CFDataGetLength(entryData.get()) : 0;
      alignedSize = (size + 3) & ~3;
   }

   uint32_t entryTag;

   CF_ptr<CFDataRef> entryData;
   CFIndex size;
   CFIndex alignedSize;
};

uint32_t
CalcTableCheckSum(const uint32_t* table, uint32_t numberOfBytesInTable) noexcept
{
   uint32_t sum = 0;

   uint32_t nLongs = (numberOfBytesInTable + 3) / 4;

   while (nLongs-- > 0)
      sum += CFSwapInt32HostToBig(*table++);

   return CFSwapInt32HostToBig(sum);
}

std::vector<uint8_t> SFNTFromCGFont(CGFontRef font)
{
   assert(font != nullptr);

   if (font == nullptr)
      return {};

   bool containsCFFTable = false;

   // Each entry in the returned array is a four-byte value that represents a
   // single TrueType or OpenType font table tag
   CF_ptr<CFArrayRef> tableTags(CGFontCopyTableTags(font));

   CFIndex tablesCount = CFArrayGetCount(tableTags.get());

   size_t totalSize = sizeof(SFNTHeader) + sizeof(SFNTTableEntry) * tablesCount;

   std::vector<CGTableEntryHelper> tableEntries;
   tableEntries.reserve(tablesCount);

   for (CFIndex tableIndex = 0; tableIndex < tablesCount; ++tableIndex)
   {
      const auto tableTag = static_cast<uint32_t>(reinterpret_cast<uintptr_t>(
         CFArrayGetValueAtIndex(tableTags.get(), tableIndex)));

      if (!containsCFFTable && tableTag == 'CFF ')
         containsCFFTable = true;

      tableEntries.emplace_back(font, tableTag);

      totalSize += tableEntries.back().alignedSize;
   }

   std::vector<uint8_t> sfntData(totalSize);

   auto sfntHeader = reinterpret_cast<SFNTHeader*>(sfntData.data());

   // fSearchRange = (max power of 2 <= num_tables) * 16.
   // fEntrySelector = Log2(fSearchRange).
   // fRangeShift = num_tables * 16 - search_range.

   uint16_t entrySelector = 0;
   uint16_t searchRange = 1;

   while (searchRange < (tablesCount / 2))
   {
      searchRange *= 2;
      entrySelector++;
   }

   searchRange *= 16;
   uint16_t rangeShift = tablesCount * 16 - searchRange;

   sfntHeader->fVersion = containsCFFTable ? 'OTTO' : CFSwapInt16HostToBig(1);
   sfntHeader->fNumTables = CFSwapInt16HostToBig(tablesCount);
   sfntHeader->fSearchRange = CFSwapInt16HostToBig(searchRange);
   sfntHeader->fEntrySelector = CFSwapInt16HostToBig(entrySelector);
   sfntHeader->fRangeShift = CFSwapInt16HostToBig(rangeShift);

   auto sfntTableEntries =
      reinterpret_cast<SFNTTableEntry*>(sfntData.data() + sizeof(SFNTHeader));

   uint32_t offset = sizeof(SFNTHeader) + sizeof(SFNTTableEntry) * tablesCount;

   for (CFIndex tableIndex = 0; tableIndex < tablesCount; ++tableIndex)
   {
      auto& tableEntry = tableEntries[tableIndex];

      sfntTableEntries[tableIndex].fTag = CFSwapInt32HostToBig(tableEntry.entryTag);
      sfntTableEntries[tableIndex].fCheckSum = CalcTableCheckSum(reinterpret_cast<const uint32_t*>(
         CFDataGetBytePtr(tableEntry.entryData.get())), tableEntry.size);
      sfntTableEntries[tableIndex].fOffset = CFSwapInt32HostToBig(offset);
      sfntTableEntries[tableIndex].fLength =
         CFSwapInt32HostToBig(tableEntry.size);

      std::memcpy(
         sfntData.data() + offset, CFDataGetBytePtr(tableEntry.entryData.get()),
         tableEntry.size);

      offset += tableEntry.alignedSize;
   }

   return sfntData;
}

CGFloat GetCTWeight(const FontInfo& fontInfo) noexcept
{
   switch (fontInfo.GetFontWeight())
   {
   case FontWeight::Thin:
      return NSFontWeightUltraLight;
   case FontWeight::ExtraLight:
      return NSFontWeightThin;
   case FontWeight::Light:
      return NSFontWeightLight;
   case FontWeight::SemiLight:
      return NSFontWeightLight;
   case FontWeight::Normal:
      return NSFontWeightRegular;
   case FontWeight::Medium:
      return NSFontWeightMedium;
   case FontWeight::SemiBold:
      return NSFontWeightSemibold;
   case FontWeight::Bold:
      return NSFontWeightBold;
   case FontWeight::ExtraBold:
      return NSFontWeightHeavy;
   case FontWeight::Heavy:
      return NSFontWeightHeavy;
   case FontWeight::ExtraBlack:
      return NSFontWeightBlack;
   default:
      return NSFontWeightRegular;
   }
}

CGFloat GetCTWidth(const FontInfo& fontInfo) noexcept
{
   auto width = static_cast<CGFloat>(fontInfo.GetFontStretch());

   return (width - static_cast<CGFloat>(FontStretch::Normal)) /
          (static_cast<CGFloat>(FontStretch::UltraExpanded) -
           static_cast<CGFloat>(FontStretch::Normal));
}

} // namespace

std::unique_ptr<graphics::fonts::FontStream>
MacosFontProvider::GetFontStream(const FontInfo& fontInfo)
{
   uint32_t fontTraits = fontInfo.GetFontStyle() != FontStyle::Normal
                             ? kCTFontItalicTrait
                             : 0;

   fontTraits |= fontInfo.GetFontWeight() > FontWeight::Normal ? kCTFontBoldTrait : 0;

   CF_ptr<CFStringRef> faceName (CFStringCreateWithBytes( kCFAllocatorDefault,
            reinterpret_cast<const UInt8*>(fontInfo.GetFaceName().data()), fontInfo.GetFaceName().size(), kCFStringEncodingUTF8, false /* no BOM */ ));

   NSDictionary* dict = @{
      (__bridge NSString*)kCTFontFamilyNameAttribute : (__bridge NSString*)faceName.get(),
      (__bridge NSString*)kCTFontTraitsAttribute : @{
         (__bridge NSString*)kCTFontSymbolicTrait : @(fontTraits),
         (__bridge NSString*)kCTFontWeightTrait : @(GetCTWeight(fontInfo)),
         (__bridge NSString*)kCTFontWidthTrait : @(GetCTWidth(fontInfo)),
      }
   };

   CF_ptr<CTFontDescriptorRef> descriptor(CTFontDescriptorCreateWithAttributes(
      (__bridge CFDictionaryRef)dict));

   CF_ptr<CTFontRef> font(CTFontCreateWithFontDescriptor(descriptor.get(),
      0.0, nullptr));

   if (!font)
      return {};

   CF_ptr<CGFontRef> cgFont(CTFontCopyGraphicsFont(font.get(), nullptr));

   return std::make_unique<MemoryFontStream>(SFNTFromCGFont(cgFont.get()));
}

} // namespace graphics::fonts::platforms::macos
