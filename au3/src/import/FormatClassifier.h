/**********************************************************************

  Audacity: A Digital Audio Editor

  FormatClassifier.h

  Philipp Sibler

**********************************************************************/

#ifndef __AUDACITY_FORMATCLASSIFIER_H_
#define __AUDACITY_FORMATCLASSIFIER_H_

#include <vector>
#include "MultiFormatReader.h"
#include "SpecPowerMeter.h"

#include <sndfile.h>

#ifndef SNDFILE_1
#error Requires libsndfile 1.0.3 or higher
#endif

// #define FORMATCLASSIFIER_SIGNAL_DEBUG 1

#ifdef FORMATCLASSIFIER_SIGNAL_DEBUG

class DebugWriter
{
    FILE* mpFid;

public:
    DebugWriter(const char* filename)
    {
        mpFid = fopen(filename, "wb");
    }

    ~DebugWriter()
    {
        if (mpFid) {
            fclose(mpFid);
        }
    }

    void WriteSignal(float* buffer, size_t len)
    {
        WriteSignal(buffer, 4, len);
    }

    void WriteSignal(void* buffer, size_t size, size_t len)
    {
        fwrite(buffer, size, len, mpFid);
    }
};

#endif

class FormatClassifier
{
public:

    typedef struct
    {
        MultiFormatReader::FormatT format;
        MachineEndianness::EndiannessT endian;
    } FormatClassT;

    typedef std::vector<FormatClassT> FormatVectorT;
    typedef std::vector<FormatClassT>::iterator FormatVectorIt;

private:

    static const size_t cSiglen = 512;
    static const size_t cNumInts = 32;

    FormatVectorT mClasses;
    MultiFormatReader mReader;
    SpecPowerCalculation mMeter;

#ifdef FORMATCLASSIFIER_SIGNAL_DEBUG
    std::unique_ptr<DebugWriter> mpWriter;
#endif

    Floats mSigBuffer{ cSiglen };
    Floats mAuxBuffer{ cSiglen };
    ArrayOf<uint8_t> mRawBuffer{ cSiglen* 8 };

    Floats mMonoFeat;
    Floats mStereoFeat;

    FormatClassT mResultFormat;
    unsigned mResultChannels { 0 };

public:
    FormatClassifier(const char* filename);
    ~FormatClassifier();

    FormatClassT GetResultFormat();
    int GetResultFormatLibSndfile();
    unsigned GetResultChannels();
private:
    void Run();
    void ReadSignal(FormatClassT format, size_t stride);
    void ConvertSamples(void* in, float* out, FormatClassT format);

    void Add(float* in1, float* in2, size_t len);
    void Sub(float* in, float subt, size_t len);
    void Div(float* in, float div, size_t len);
    void Abs(float* in, float* out, size_t len);
    float Mean(float* in, size_t len);
    float Max(float* in, size_t len);
    float Max(float* in, size_t len, size_t* maxidx);

    template<class T> void ToFloat(T* in, float* out, size_t len);
};

#endif
