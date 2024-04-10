/**********************************************************************

  Audacity: A Digital Audio Editor

  AVInputFormatWrapper.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AVInputFormatWrapper.h"

#include "FFmpegFunctions.h"

AVInputFormatWrapper::AVInputFormatWrapper(AVInputFormat* wrapped) noexcept
    : mAVInputFormat(wrapped)
{
}

AVInputFormat* AVInputFormatWrapper::GetWrappedValue() noexcept
{
   return mAVInputFormat;
}

 const AVInputFormat* AVInputFormatWrapper::GetWrappedValue() const noexcept
{
   return mAVInputFormat;
}
