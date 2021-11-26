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

   StreamChunk dataView = { data, length };

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

const size_t MemoryStream::GetSize() const noexcept
{
   return mDataSize;
}

size_t MemoryStream::Chunk::Append(StreamChunk& dataView)
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

bool MemoryStream::IsEmpty() const noexcept
{
   return mDataSize == 0;
}

MemoryStream::Iterator MemoryStream::begin() const
{
   return Iterator(this, true);
}

MemoryStream::Iterator MemoryStream::end() const
{
   return Iterator(this, false);
}

MemoryStream::Iterator::Iterator(const MemoryStream* stream, bool isBegin)
    : mStream(stream)
    , mListIterator(isBegin ? mStream->mChunks.cbegin() : mStream->mChunks.cend())
    , mShowLinearPart(isBegin && mStream->mLinearData.size() > 0)
{
}

MemoryStream::Iterator& MemoryStream::Iterator::operator++()
{
   if (mShowLinearPart)
      mShowLinearPart = false;
   else
      ++mListIterator;

   return *this;
}

MemoryStream::Iterator MemoryStream::Iterator::operator++(int)
{
   Iterator result { *this };
   this->operator++();
   return result;
}

MemoryStream::StreamChunk MemoryStream::Iterator::operator*() const
{
   if (mShowLinearPart)
      return { mStream->mLinearData.data(), mStream->mLinearData.size() };

   return { mListIterator->Data.data(), mListIterator->BytesUsed };
}

MemoryStream::StreamChunk MemoryStream::Iterator::operator->() const
{
   return this->operator*();
}

bool MemoryStream::Iterator::operator!=(const Iterator& rhs) const noexcept
{
   return !(*this == rhs);
}

bool MemoryStream::Iterator::operator==(const Iterator& rhs) const noexcept
{
   return mStream == rhs.mStream && mListIterator == rhs.mListIterator &&
          mShowLinearPart == rhs.mShowLinearPart;
}
