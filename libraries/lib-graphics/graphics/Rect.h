/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Rect.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "Point.h"
#include "Size.h"

template <typename DataType>
struct RectType final
{
   PointType<DataType> Origin;
   SizeType<DataType> Size;
};

using Rect = RectType<float>;
