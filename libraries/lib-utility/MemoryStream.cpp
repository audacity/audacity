/**********************************************************************

  Audacity: A Digital Audio Editor

  MemoryStream.cpp

  Dmitry Vedenko

**********************************************************************/

#include "MemoryStream.h"

#include <algorithm>

void MemoryStream::Clear()
{
   mChunks = {};
   mLinearData = {};
   mDataSize = 0;
}

void MemoryStream::AppendByte(char data)
{
   AppendData(&data, 1);
}

void MemoryStream::AppendData(const void* data, const size_t length)
{
   if (mChunks.empty())
      mChunks.emplace_back();

   AppendDataView dataView = { data, length };

   while (mChunks.back().Append(dataView) > 0)
      mChunks.emplace_back();

   mDataSize += length;
}

const void* MemoryStream::GetData() const
{
   if (!mChunks.empty())
   {
      const size_t desiredSize = GetSize();

      mLinearData.reserve(desiredSize);

      for (const Chunk& chunk : mChunks)
      {
         auto begin = chunk.Data.begin();
         auto end = begin + chunk.BytesUsed;

         mLinearData.insert(mLinearData.end(), begin, end);
      }

      mChunks = {};
   }

   return mLinearData.data();
}

const size_t MemoryStream::GetSize() const
{
   return mDataSize;
}

size_t MemoryStream::Chunk::Append(AppendDataView& dataView)
{
   const size_t dataSize = dataView.second;

   const size_t bytesToWrite = std::min(ChunkSize - BytesUsed, dataSize);
   const size_t bytesLeft = dataSize - bytesToWrite;

   const uint8_t* beginData = static_cast<const uint8_t*>(dataView.first);
   const uint8_t* endData = beginData + bytesToWrite;

   // Some extreme micro optimization for MSVC (at least)
   // std::copy will generate a call to memmove in any case
   // which will be slower than just writing the byte
   if (bytesToWrite == 1)
   {
      Data[BytesUsed] = *beginData;
   }
   else
   {
      // There is a chance for unaligned access, so we do no handle
      // types as int32_t separately. Unaligned access is slow on x86
      // and fails horribly on ARM
      std::copy(beginData, endData, Data.begin() + BytesUsed);
   }

   dataView.first = endData;
   dataView.second = bytesLeft;

   BytesUsed += bytesToWrite;

   return bytesLeft;
}
