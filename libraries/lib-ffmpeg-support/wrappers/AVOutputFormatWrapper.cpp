/**********************************************************************

  Audacity: A Digital Audio Editor

  AVOutputFormatWrapper.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AVOutputFormatWrapper.h"

#include "FFmpegFunctions.h"

AVOutputFormatWrapper::AVOutputFormatWrapper(AVOutputFormat* wrapped) noexcept
    : mAVOutputFormat(wrapped)
{
}

AVOutputFormat* AVOutputFormatWrapper::GetWrappedValue() noexcept
{
   return mAVOutputFormat;
}

const AVOutputFormat* AVOutputFormatWrapper::GetWrappedValue() const noexcept
{
   return mAVOutputFormat;
}
