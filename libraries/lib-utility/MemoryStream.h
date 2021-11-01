/**********************************************************************

  Audacity: A Digital Audio Editor

  MemoryStream.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <array>
#include <cstdint>
#include <list>
#include <vector>

/*!
 * @brief A low overhead memory stream with O(1) append, low heap fragmentation and a linear memory view.
 *
 * wxMemoryBuffer always appends 1Kb to the end of the buffer, causing severe performance issues
 * and significant heap fragmentation. There is no possibility to controll the increment value.
 *
 * std::vector doubles it's memory size which can be problematic for large projects as well.
 * Not as bad as wxMemoryBuffer though.
 * 
 */
class UTILITY_API MemoryStream final
{
public:
   using StreamData = std::vector<uint8_t>;

   void Clear();

   void AppendByte(char data);
   void AppendData(const void* data, const size_t length);

   // This function possibly has O(size) complexity as it may
   // require copying bytes to a linear chunk
   const void* GetData() const;
   const size_t GetSize() const;
private:
   static constexpr size_t ChunkSize = 1024 * 1024;

   using AppendDataView = std::pair<const void*, size_t>;

   struct Chunk final
   {
      std::array<uint8_t, ChunkSize> Data;
      size_t BytesUsed { 0 };

      // Returns data size left to append
      size_t Append(AppendDataView& dataView);
   };

   using ChunksList = std::list<Chunk>;

   // This structures are lazily updated by get data
   mutable ChunksList mChunks;
   mutable StreamData mLinearData;

   size_t mDataSize { 0 };
};
