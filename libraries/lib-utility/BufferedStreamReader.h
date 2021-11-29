/**********************************************************************

  Audacity: A Digital Audio Editor

  BufferedStreamReader.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <vector>
#include <cstdint>
#include <cstddef>
#include <type_traits>

/*!
 * \brief A facade-like class, that implements buffered reading from the underlying data stream.
 *
 * BufferedStreamReader provides optimal read performance for the built-in types of up to 8 bytes long.
 */
class UTILITY_API BufferedStreamReader /* not final */
{
public:
   static constexpr size_t RequiredAlignment = 8;

   explicit BufferedStreamReader(size_t bufferSize = 4096);

   //! Read up to maxBytes into the buffer. Returns the number of bytes read.
   size_t Read(void* buffer, size_t maxBytes);

   //! Read a single value of ValueType, where sizeof(ValueType) <= 8 and value is aligned to the size boundary
   template<typename ValueType>
   std::enable_if_t<sizeof(ValueType) <= RequiredAlignment, bool>
   ReadValue(ValueType& value) /* ReadData may throw */
   {
      constexpr size_t valueSize = sizeof(ValueType);
      const size_t availableBytes = mCurrentBytes - mCurrentIndex;

      // In case of underflow - just fall back to general Read routine
      if (availableBytes < valueSize)
         return valueSize == Read(&value, valueSize);

      // Special case for one bytes reads
      if constexpr (valueSize == 1)
         value = mBufferStart[mCurrentIndex];
      else
         value = UncheckedRead<ValueType>();

      mCurrentIndex += valueSize;

      return true;
   }

   //! Returns true if there is no more data available
   bool Eof() const;

   //! Read a single byte from the stream. Return -1 on failure. 
   int GetC();

protected:
   //! Should return true, if underlying stream has more data
   virtual bool HasMoreData() const = 0;
   //! Read up to maxBytes into the buffer. Should return the number of bytes read.
   virtual size_t ReadData(void* buffer, size_t maxBytes) = 0;

private:
   //! Reads the data from the stream if there is not enough data buffered
   bool HandleUnderflow();

   /*! Reads a value of type T from the stream.
    * @pre mCurrentIndex + sizeof(T) <= mCurrentBytes
    */
   template<typename T>
   T UncheckedRead() noexcept
   {
      T result;

      // We know, that mBufferStart is always at least RequiredAlignment aligned
      // Only mCurrentIndex alignment matters here

      if ((mCurrentIndex % sizeof(T)) == 0)
      {
         // If the result is aligned - just read it from the memory
         const void* ptr = mBufferStart + mCurrentIndex;
         result = *static_cast<const T*>(ptr);
      }
      else
      {
         // Fallback to std::copy otherwise
         const uint8_t* begin = mBufferStart + mCurrentIndex;
         const uint8_t* end = begin + sizeof(T);
         void* out = &result;

         std::copy(begin, end, static_cast<uint8_t*>(out));
      }

      return result;
   }

   // Cache storage
   std::vector<uint8_t> mBufferData;

   // mBufferStart is aligned to RequiredAlignment bytes boundary.
   // Both values below are initialized in the constructor.
   uint8_t* mBufferStart;
   size_t mBufferSize;

   size_t mCurrentIndex { 0 };
   size_t mCurrentBytes { 0 };
};
