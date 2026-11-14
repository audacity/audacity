/**********************************************************************

  Audacity: A Digital Audio Editor

  MultiFormatReader.h

  Philipp Sibler

**********************************************************************/

#ifndef __AUDACITY_MULTIFORMATREADER_H__
#define __AUDACITY_MULTIFORMATREADER_H__

#include <stdio.h>
#include <stdint.h>

class MachineEndianness
{
public:
    typedef enum
    {
        Little = 0,
        Big
    } EndiannessT;

    MachineEndianness();
    ~MachineEndianness()
    {}

    int IsLittle()
    {
        return (mFlag == MachineEndianness::Little) ? 1 : 0;
    }

    int IsBig()
    {
        return (mFlag == MachineEndianness::Big) ? 1 : 0;
    }

    EndiannessT Which()
    {
        return mFlag;
    }

private:
    EndiannessT mFlag;
};

class MultiFormatReader
{
    FILE* mpFid;
    MachineEndianness mEnd;
    uint8_t mSwapBuffer[8];

public:
    typedef enum
    {
        Int8 = 0,
        Int16,
        Int32,
        Uint8,
        Uint16,
        Uint32,
        Float,
        Double
    } FormatT;

    MultiFormatReader(const char* filename);
    ~MultiFormatReader();

    void Reset();
    size_t ReadSamples(void* buffer, size_t len, MultiFormatReader::FormatT format, MachineEndianness::EndiannessT end);
    size_t ReadSamples(void* buffer, size_t len, size_t stride, MultiFormatReader::FormatT format, MachineEndianness::EndiannessT end);

private:
    size_t Read(void* buffer, size_t size, size_t len, size_t stride);
    void SwapBytes(void* buffer, size_t size, size_t len);
};

#endif
