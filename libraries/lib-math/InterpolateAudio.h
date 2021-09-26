/**********************************************************************

  Audacity: A Digital Audio Editor

  InterpolateAudio.h

  Dominic Mazzoni

*******************************************************************//*!

\file Matrix.h
\brief General routine to interpolate (or even extrapolate small amounts)
 audio when a few of the samples are bad.  Works great for a few
 dozen bad samples, but not so well with hundreds.  Uses the
 least-squares autoregression (LSAR) algorithm, as described in:

 Simon Godsill, Peter Rayner, and Olivier Cappe.  Digital Audio Restoration.
 Berlin: Springer, 1998.

 This is the same work used by Gnome Wave Cleaner (GWC), however this
 implementation is original.

*//*******************************************************************/

#ifndef __AUDACITY_INTERPOLATE_AUDIO__
#define __AUDACITY_INTERPOLATE_AUDIO__

#include <cstddef>

// See top of file for a description of the algorithm.  Interpolates
// the samples from buffer[firstBad] through buffer[firstBad+numBad-1],
// ignoring whatever value was there previously, and replacing them with
// values determined from the surrounding audio.  Works best if the bad
// samples are in the middle, with several times as much data on either
// side (6x the number of bad samples on either side is great).  However,
// it will work with less data, and with the bad samples on one end or
// the other.
void MATH_API InterpolateAudio(float *buffer, size_t len,
                                       size_t firstBad, size_t numBad);

#endif // __AUDACITY_INTERPOLATE_AUDIO__
