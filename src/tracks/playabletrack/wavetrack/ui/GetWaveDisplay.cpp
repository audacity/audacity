/**********************************************************************

  Audacity: A Digital Audio Editor

  @file GetWaveDisplay.cpp

  Paul Licameli split from Sequence.cpp

**********************************************************************/

#include "GetWaveDisplay.h"

#include <algorithm>
#include <cmath>
#include <float.h>
#include <wx/debug.h>
#include "SampleBlock.h"
#include "SampleCount.h"
#include "Sequence.h"

namespace {

struct MinMaxSumsq
{
   MinMaxSumsq(const float *pv, int count, int divisor)
   {
      min = FLT_MAX, max = -FLT_MAX, sumsq = 0.0f;
      while (count--) {
         float v;
         switch (divisor) {
         default:
         case 1:
            // array holds samples
            v = *pv++;
            if (v < min)
               min = v;
            if (v > max)
               max = v;
            sumsq += v * v;
            break;
         case 256:
         case 65536:
            // array holds triples of min, max, and rms values
            v = *pv++;
            if (v < min)
               min = v;
            v = *pv++;
            if (v > max)
               max = v;
            v = *pv++;
            sumsq += v * v;
            break;
         }
      }
   }

   float min;
   float max;
   float sumsq;
};

}

bool GetWaveDisplay(const Sequence &sequence,
   float *min, float *max, float *rms, int* bl,
   size_t len, const sampleCount *where)
{
   wxASSERT(len > 0);
   const auto s0 = std::max(sampleCount(0), where[0]);
   const auto numSamples = sequence.GetNumSamples();
   if (s0 >= numSamples)
      // None of the samples asked for are in range. Abandon.
      return false;

   // In case where[len - 1] == where[len], raise the limit by one,
   // so we load at least one pixel for column len - 1
   // ... unless the mNumSamples ceiling applies, and then there are other defenses
   const auto s1 = std::clamp(where[len], 1 + where[len - 1], numSamples);
   const auto maxSamples = sequence.GetMaxBlockSize();
   Floats temp{ maxSamples };

   decltype(len) pixel = 0;

   auto srcX = s0;
   decltype(srcX) nextSrcX = 0;
   int lastRmsDenom = 0;
   int lastDivisor = 0;
   auto whereNow = std::min(s1 - 1, where[0]);
   decltype(whereNow) whereNext = 0;
   // Loop over block files, opening and reading and closing each
   // not more than once
   const auto &blocks = sequence.GetBlockArray();
   unsigned nBlocks = blocks.size();
   const unsigned int block0 = sequence.FindBlock(s0);
   for (unsigned int b = block0; b < nBlocks; ++b) {
      if (b > block0)
         srcX = nextSrcX;
      if (srcX >= s1)
         break;

      // Find the range of sample values for this block that
      // are in the display.
      const SeqBlock &seqBlock = blocks[b];
      const auto start = seqBlock.start;
      nextSrcX = std::min(s1, start + seqBlock.sb->GetSampleCount());

      // The column for pixel p covers samples from
      // where[p] up to but excluding where[p + 1].

      // Find the range of pixels covered by the current block file
      // (Their starting samples covered by it, to be exact)
      decltype(len) nextPixel;
      if (nextSrcX >= s1)
         // last pass
         nextPixel = len;
      else {
         nextPixel = pixel;
         // Taking min with s1 - 1, here and elsewhere, is another defense
         // to be sure the last pixel column gets at least one sample
         while (nextPixel < len &&
                (whereNext = std::min(s1 - 1, where[nextPixel])) < nextSrcX)
            ++nextPixel;
      }
      if (nextPixel == pixel)
         // The entire block's samples fall within one pixel column.
         // Either it's a rare odd block at the end, or else,
         // we must be really zoomed out!
         // Omit the entire block's contents from min/max/rms
         // calculation, which is not correct, but correctness might not
         // be worth the compute time if this happens every pixel
         // column. -- PRL
         continue;
      if (nextPixel == len)
         whereNext = s1;

      // Decide the summary level
      const double samplesPerPixel =
         (whereNext - whereNow).as_double() / (nextPixel - pixel);
      const int divisor =
           (samplesPerPixel >= 65536) ? 65536
         : (samplesPerPixel >= 256) ? 256
         : 1;

      int blockStatus = b;

      // How many samples or triples are needed?

      const size_t startPosition =
         // srcX and start are in the same block
         std::max(sampleCount(0), (srcX - start) / divisor).as_size_t();
      const size_t inclusiveEndPosition =
         // nextSrcX - 1 and start are in the same block
         std::min((sampleCount(maxSamples) / divisor) - 1,
                  (nextSrcX - 1 - start) / divisor).as_size_t();
      const auto num = 1 + inclusiveEndPosition - startPosition;
      if (num <= 0) {
         // What?  There was a zero length block file?
         wxASSERT(false);
         // Do some defense against this case anyway
         while (pixel < nextPixel) {
            min[pixel] = max[pixel] = rms[pixel] = 0;
            bl[pixel] = blockStatus;//MC
            ++pixel;
         }
         continue;
      }

      // Read from the block file or its summary
      switch (divisor) {
      default:
      case 1:
         // Read samples
         // no-throw for display operations!
         sequence.Read(
            (samplePtr)temp.get(), floatSample, seqBlock, startPosition, num, false);
         break;
      case 256:
         // Read triples
         // Ignore the return value.
         // This function fills with zeroes if read fails
         seqBlock.sb->GetSummary256(temp.get(), startPosition, num);
         break;
      case 65536:
         // Read triples
         // Ignore the return value.
         // This function fills with zeroes if read fails
         seqBlock.sb->GetSummary64k(temp.get(), startPosition, num);
         break;
      }
      
      auto filePosition = startPosition;

      // The previous pixel column might straddle blocks.
      // If so, impute some of the data to it.
      if (b > block0 && pixel > 0) {
         // whereNow and start are in the same block
         auto midPosition = ((whereNow - start) / divisor).as_size_t();
         int diff(midPosition - filePosition);
         if (diff > 0) {
            MinMaxSumsq values(temp.get(), diff, divisor);
            const int lastPixel = pixel - 1;
            float &lastMin = min[lastPixel];
            lastMin = std::min(lastMin, values.min);
            float &lastMax = max[lastPixel];
            lastMax = std::max(lastMax, values.max);
            float &lastRms = rms[lastPixel];
            int lastNumSamples = lastRmsDenom * lastDivisor;
            lastRms = sqrt(
               (lastRms * lastRms * lastNumSamples + values.sumsq * divisor) /
               (lastNumSamples + diff * divisor)
            );

            filePosition = midPosition;
         }
      }

      // Loop over file positions
      int rmsDenom = 0;
      for (; filePosition <= inclusiveEndPosition;) {
         // Find range of pixel columns for this file position
         // (normally just one, but maybe more when zoomed very close)
         // and the range of positions for those columns
         // (normally one or more, for that one column)
         auto pixelX = pixel + 1;
         decltype(filePosition) positionX = 0;
         while (pixelX < nextPixel &&
            filePosition ==
               (positionX = (
                  // s1 - 1 or where[pixelX] and start are in the same block
                  (std::min(s1 - 1, where[pixelX]) - start) / divisor).as_size_t() )
         )
            ++pixelX;
         if (pixelX >= nextPixel)
            positionX = 1 + inclusiveEndPosition;

         // Find results to assign
         rmsDenom = (positionX - filePosition);
         wxASSERT(rmsDenom > 0);
         const float *const pv =
            temp.get() + (filePosition - startPosition) * (divisor == 1 ? 1 : 3);
         MinMaxSumsq values(pv, std::max(0, rmsDenom), divisor);

         // Assign results
         std::fill(&min[pixel], &min[pixelX], values.min);
         std::fill(&max[pixel], &max[pixelX], values.max);
         std::fill(&bl[pixel], &bl[pixelX], blockStatus);
         std::fill(&rms[pixel], &rms[pixelX], (float)sqrt(values.sumsq / rmsDenom));

         pixel = pixelX;
         filePosition = positionX;
      }

      wxASSERT(pixel == nextPixel);
      whereNow = whereNext;
      pixel = nextPixel;
      lastDivisor = divisor;
      lastRmsDenom = rmsDenom;
   } // for each block file

   wxASSERT(pixel == len);

   return true;
}
