/**********************************************************************

  Audacity: A Digital Audio Editor

  BufferedStreamReader.cpp

  Dmitry Vedenko

**********************************************************************/

#include "BufferedStreamReader.h"

#include <algorithm>
#include <memory>
#include <cstring>

BufferedStreamReader::BufferedStreamReader(size_t bufferSize)
    : mBufferSize(std::max(bufferSize, RequiredAlignment))
{
   mBufferData.resize(mBufferSize + RequiredAlignment);

   auto ptr = static_cast<void*>(mBufferData.data());
   auto space = mBufferData.size();

   std::align(RequiredAlignment, mBufferSize, ptr, space);

   mBufferStart = static_cast<uint8_t*>(ptr);
}

size_t BufferedStreamReader::Read(void* buffer, size_t maxBytes)
{
   size_t bytesWritten = 0;

   while (maxBytes > 0)
   {
      if (mCurrentBytes == mCurrentIndex)
      {
         if (!HandleUnderflow())
            return bytesWritten;
      }

      const size_t availableBytes = mCurrentBytes - mCurrentIndex;
      const size_t bytesToRead = std::min(maxBytes, availableBytes);

      std::memcpy(static_cast<uint8_t*>(buffer) + bytesWritten, mBufferStart + mCurrentIndex, bytesToRead);

      maxBytes -= bytesToRead;
      bytesWritten += bytesToRead;
      mCurrentIndex += bytesToRead;
   }

   return bytesWritten;
}

bool BufferedStreamReader::Eof() const
{
   return mCurrentBytes == mCurrentIndex && !HasMoreData();
}

int BufferedStreamReader::GetC()
{
   uint8_t value = 0;

   if (ReadValue(value))
      return value;

   return -1;
}

bool BufferedStreamReader::HandleUnderflow()
{
   if (!HasMoreData())
      return false;

   mCurrentBytes = ReadData(mBufferStart, mBufferSize);
   mCurrentIndex = 0;

   return true;
}
