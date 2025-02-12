/**********************************************************************

  Audacity: A Digital Audio Editor

  AVOutputFormatWrapper.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AVOutputFormatWrapper.h"

#include "../FFmpegFunctions.h"

AVOutputFormatWrapper::AVOutputFormatWrapper(const AVOutputFormat* wrapped) noexcept
    : mAVOutputFormat(wrapped)
{
}

const AVOutputFormat* AVOutputFormatWrapper::GetWrappedValue() const noexcept
{
    return mAVOutputFormat;
}
