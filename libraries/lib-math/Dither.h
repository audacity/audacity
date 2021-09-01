/**********************************************************************

  Audacity: A Digital Audio Editor

  Steve Harris
  Markus Meyer

**********************************************************************/

#ifndef __AUDACITY_DITHER_H__
#define __AUDACITY_DITHER_H__

#include "SampleFormat.h"

template< typename Enum > class EnumSetting;


/// These ditherers are currently available:
enum DitherType : unsigned {
   none = 0, rectangle = 1, triangle = 2, shaped = 3 };

class MATH_API Dither
{
public:
    static DitherType FastDitherChoice();
    static DitherType BestDitherChoice();

    static EnumSetting< DitherType > FastSetting;
    static EnumSetting< DitherType > BestSetting;

    /// Default constructor
    Dither();

    /// Reset state of the dither.
    void Reset();

    /// Apply the actual dithering. Expects the source sample in the
    /// 'source' variable, the destination sample in the 'dest' variable,
    /// and hints to the formats of the samples. Even if the sample formats
    /// are the same, samples are clipped, if necessary.
    void Apply(DitherType ditherType,
               constSamplePtr source, sampleFormat sourceFormat,
               samplePtr dest, sampleFormat destFormat,
               unsigned int len,
               unsigned int sourceStride = 1,
               unsigned int destStride = 1);
};

#endif /* __AUDACITY_DITHER_H__ */
