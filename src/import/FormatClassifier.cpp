/**********************************************************************

  Audacity: A Digital Audio Editor

  FormatClassifier.cpp

  Philipp Sibler

******************************************************************//**

\class FormatClassifier
\brief FormatClassifier classifies the sample format and endianness of
raw audio files.

The classifier operates in the frequency domain and exploits 
the low-pass-like spectral behaviour of natural audio signals 
for classification of the sample format and the used endianness.

*//*******************************************************************/
#include "FormatClassifier.h"

#include <stdint.h>
#include <cmath>
#include <cfloat>
#include <vector>
#include <cstdio>

#include <wx/defs.h>

#include "MultiFormatReader.h"
#include "sndfile.h"

FormatClassifier::FormatClassifier(const char* filename) :
   mReader(filename),
   mMeter(cSiglen)
{
   FormatClassT fClass;

   // Build buffers
   mSigBuffer = new float[cSiglen];
   mAuxBuffer = new float[cSiglen];   
   mRawBuffer = new uint8_t[cSiglen * 8];

   // Define the classification classes
   fClass.endian = MachineEndianness::Little;
   fClass.format = MultiFormatReader::Int8;
   mClasses.push_back(fClass);
   fClass.format = MultiFormatReader::Int16;
   mClasses.push_back(fClass);
   fClass.format = MultiFormatReader::Int32;
   mClasses.push_back(fClass);
   fClass.format = MultiFormatReader::Uint8;
   mClasses.push_back(fClass);
   fClass.format = MultiFormatReader::Float;
   mClasses.push_back(fClass);   
   fClass.format = MultiFormatReader::Double;
   mClasses.push_back(fClass);

   fClass.endian = MachineEndianness::Big;
   fClass.format = MultiFormatReader::Int8;
   mClasses.push_back(fClass);
   fClass.format = MultiFormatReader::Int16;
   mClasses.push_back(fClass);
   fClass.format = MultiFormatReader::Int32;
   mClasses.push_back(fClass);
   fClass.format = MultiFormatReader::Uint8;
   mClasses.push_back(fClass);
   fClass.format = MultiFormatReader::Float;
   mClasses.push_back(fClass);   
   fClass.format = MultiFormatReader::Double;
   mClasses.push_back(fClass);

   // Build feature vectors
   mMonoFeat = new float[mClasses.size()];
   mStereoFeat = new float[mClasses.size()];
   
#ifdef FORMATCLASSIFIER_SIGNAL_DEBUG
   // Build a debug writer
   char dfile [1024];
   sprintf(dfile, "%s.sig", filename);
   mpWriter = std::make_unique<DebugWriter>(dfile);
#endif

   // Run it
   Run();
   
#ifdef FORMATCLASSIFIER_SIGNAL_DEBUG
   for (unsigned int n = 0; n < mClasses.size(); n++)
   {
      printf("Class [%i] Machine [%i]: Mono: %3.7f Stereo: %3.7f\n", mClasses[n].format, mClasses[n].endian, mMonoFeat[n], mStereoFeat[n]);
   }
#endif

}

FormatClassifier::~FormatClassifier()
{
   delete[] mSigBuffer;
   delete[] mAuxBuffer;
   delete[] mRawBuffer;

   delete[] mMonoFeat;
   delete[] mStereoFeat;
}

FormatClassifier::FormatClassT FormatClassifier::GetResultFormat()
{
   return mResultFormat;
}

int FormatClassifier::GetResultFormatLibSndfile()
{
   int format = SF_FORMAT_RAW;
   
   switch(mResultFormat.format)
   {
      case MultiFormatReader::Int8:
         format |= SF_FORMAT_PCM_S8;
         break;
      case MultiFormatReader::Int16:
         format |= SF_FORMAT_PCM_16;
         break;
      case MultiFormatReader::Int32:
         format |= SF_FORMAT_PCM_32;
         break;
      case MultiFormatReader::Uint8:
         format |= SF_FORMAT_PCM_U8;
         break;
      case MultiFormatReader::Float:
         format |= SF_FORMAT_FLOAT;
         break;
      case MultiFormatReader::Double:
         format |= SF_FORMAT_DOUBLE;
         break;
      default:
         format |= SF_FORMAT_PCM_16;
         break;
   }
   
   switch(mResultFormat.endian)
   {
      case MachineEndianness::Little:
         format |= SF_ENDIAN_LITTLE;
         break;
      case MachineEndianness::Big:
         format |= SF_ENDIAN_BIG;
         break;
   }
   
   return format;
}

unsigned FormatClassifier::GetResultChannels()
{
   return mResultChannels;
}

void FormatClassifier::Run()
{
   // Calc the mono feature vector
   for (unsigned int n = 0; n < mClasses.size(); n++)
   {
      // Read the signal
      ReadSignal(mClasses[n], 1);
#ifdef FORMATCLASSIFIER_SIGNAL_DEBUG
      mpWriter->WriteSignal(mSigBuffer, cSiglen);
#endif

      // Do some simple preprocessing
      // Remove DC offset
      float smean = Mean(mSigBuffer, cSiglen);
      Sub(mSigBuffer, smean, cSiglen);
      // Normalize to +- 1.0
      Abs(mSigBuffer, mAuxBuffer, cSiglen);
      float smax = Max(mAuxBuffer, cSiglen);
      Div(mSigBuffer, smax, cSiglen);

      // Now actually fill the feature vector
      // Low to high band power ratio
      float pLo = mMeter.CalcPower(mSigBuffer, 0.15f, 0.3f);
      float pHi = mMeter.CalcPower(mSigBuffer, 0.45f, 0.1f); 
      mMonoFeat[n] = pLo / pHi;
   }

   // Calc the stereo feature vector
   for (unsigned int n = 0; n < mClasses.size(); n++)
   {
      // Read the signal
      ReadSignal(mClasses[n], 2);
#ifdef FORMATCLASSIFIER_SIGNAL_DEBUG
      mpWriter->WriteSignal(mSigBuffer, cSiglen);
#endif

      // Do some simple preprocessing
      // Remove DC offset
      float smean = Mean(mSigBuffer, cSiglen);
      Sub(mSigBuffer, smean, cSiglen);
      // Normalize to +- 1.0
      Abs(mSigBuffer, mAuxBuffer, cSiglen);
      float smax = Max(mAuxBuffer, cSiglen);
      Div(mSigBuffer, smax, cSiglen);

      // Now actually fill the feature vector
      // Low to high band power ratio
      float pLo = mMeter.CalcPower(mSigBuffer, 0.15f, 0.3f);
      float pHi = mMeter.CalcPower(mSigBuffer, 0.45f, 0.1f); 
      mStereoFeat[n] = pLo / pHi;
   }

   // Get the results
   size_t midx, sidx;
   float monoMax = Max(mMonoFeat, mClasses.size(), &midx);
   float stereoMax = Max(mStereoFeat, mClasses.size(), &sidx);

   if (monoMax > stereoMax)
   {
      mResultChannels = 1;
      mResultFormat = mClasses[midx];
   }
   else
   {
      mResultChannels = 2;
      mResultFormat = mClasses[sidx];
   }

}

void FormatClassifier::ReadSignal(FormatClassT format, size_t stride)
{
   size_t actRead = 0;
   unsigned int n = 0;

   mReader.Reset();

   // Do a dummy read of 1024 bytes to skip potential header information
   mReader.ReadSamples(mRawBuffer, 1024, MultiFormatReader::Uint8, MachineEndianness::Little);

   do
   {
      actRead = mReader.ReadSamples(mRawBuffer, cSiglen, stride, format.format, format.endian);

      if (n == 0)
      {
         ConvertSamples(mRawBuffer, mSigBuffer, format);
      }
      else
      {
         if (actRead == cSiglen)
         {
            ConvertSamples(mRawBuffer, mAuxBuffer, format);

            // Integrate signals
            Add(mSigBuffer, mAuxBuffer, cSiglen);

            // Do some dummy reads to break signal coherence
            mReader.ReadSamples(mRawBuffer, n + 1, stride, format.format, format.endian);
         }
      }

      n++;

   } while ((n < cNumInts) && (actRead == cSiglen));

}

void FormatClassifier::ConvertSamples(void* in, float* out, FormatClassT format)
{
   switch(format.format)
   {
      case MultiFormatReader::Int8:
         ToFloat((int8_t*) in, out, cSiglen);
         break;
      case MultiFormatReader::Int16:
         ToFloat((int16_t*) in, out, cSiglen);
         break;
      case MultiFormatReader::Int32:
         ToFloat((int32_t*) in, out, cSiglen);
         break;
      case MultiFormatReader::Uint8:
         ToFloat((uint8_t*) in, out, cSiglen);
         break;
      case MultiFormatReader::Uint16:
         ToFloat((uint16_t*) in, out, cSiglen);
         break;
      case MultiFormatReader::Uint32:
         ToFloat((uint32_t*) in, out, cSiglen);
        break;
      case MultiFormatReader::Float:
         ToFloat((float*) in, out, cSiglen);
         break;
      case MultiFormatReader::Double:
         ToFloat((double*) in, out, cSiglen);
         break;
   }
}

void FormatClassifier::Add(float* in1, float* in2, size_t len)
{
   for (unsigned int n = 0; n < len; n++)
   {
      in1[n] += in2[n];
   }
}

void FormatClassifier::Sub(float* in, float subt, size_t len)
{
   for (unsigned int n = 0; n < len; n++)
   {
      in[n] -= subt;
   }
}

void FormatClassifier::Div(float* in, float div, size_t len)
{
   for (unsigned int n = 0; n < len; n++)
   {
      in[n] /= div;
   }
}


void FormatClassifier::Abs(float* in, float* out, size_t len)
{
   for (unsigned int n = 0; n < len; n++)
   {
      if (in[n] < 0.0f)
      {
         out[n] = -in[n];
      }
      else
      {
         out[n] = in[n];
      }
   }
}

float FormatClassifier::Mean(float* in, size_t len)
{
   float mean = 0.0f;

   for (unsigned int n = 0; n < len; n++)
   {
      mean += in[n];
   }

   mean /= len;
   
   return mean;
}

float FormatClassifier::Max(float* in, size_t len)
{
   size_t dummyidx;
   return Max(in, len, &dummyidx);
}

float FormatClassifier::Max(float* in, size_t len, size_t* maxidx)
{
   float max = -FLT_MAX;
   *maxidx = 0;
   
   for (unsigned int n = 0; n < len; n++)
   {
      if (in[n] > max)
      {
         max = in[n];
         *maxidx = n;
      }
   }

   return max;
}

template<class T> void FormatClassifier::ToFloat(T* in, float* out, size_t len)
{
   for(unsigned int n = 0; n < len; n++)
   {
      out[n] = (float) in[n];
   }
}
