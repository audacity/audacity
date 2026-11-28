/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  StretchMethod.h

**********************************************************************/
#pragma once

//! How a clip's change of speed is done.
//! Saved in project files as its number: only ever append, never reorder.
enum class StretchMethod
{
    //! Phase vocoder by StaffPad. Keeps the pitch. The only method before
    //! there was a choice, so clips saved without a method use it.
    StaffPad = 0,
    //! SoundTouch, time-domain (WSOLA). Keeps the pitch.
    SoundTouch = 1,
    //! SBSMS, subband sinusoidal modeling. Keeps the pitch.
    Sbsms = 2,
    //! Paulstretch, extreme spectral smearing.
    Paulstretch = 3,
    //! Plays the samples faster or slower, like a tape machine: the pitch
    //! follows the speed.
    Varispeed = 4,
};

constexpr bool IsValidStretchMethod(long value)
{
    return value >= static_cast<long>(StretchMethod::StaffPad)
           && value <= static_cast<long>(StretchMethod::Varispeed);
}
