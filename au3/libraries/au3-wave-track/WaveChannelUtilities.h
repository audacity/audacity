/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveChannelUtilities.h

  Paul Licameli

  @brief Various operations on WaveChannel, needing only its public interface

**********************************************************************/
#ifndef __AUDACITY_WAVE_CHANNEL_UTILITIES__
#define __AUDACITY_WAVE_CHANNEL_UTILITIES__

class Envelope;
enum class PlaybackDirection;
enum class sampleFormat : unsigned;
class WaveChannel;
class WaveClipChannel;

#include <algorithm>
#include <functional>
#include <memory>
#include <utility>
#include <vector>

namespace WaveChannelUtilities {
using Clip = WaveClipChannel;
using ClipPointer = std::shared_ptr<Clip>;
using ClipPointers = std::vector<ClipPointer>;
using ClipConstPointer = std::shared_ptr<const Clip>;
using ClipConstPointers = std::vector<ClipConstPointer>;

WAVE_TRACK_API
bool CompareClipsByPlayStartTime(const Clip& x, const Clip& y);

inline bool CompareClipPointersByPlayStartTime(
    const ClipConstPointer x, const ClipConstPointer y)
{
    return CompareClipsByPlayStartTime(*x, *y);
}

inline bool IsSortedByPlayStartTime(const ClipPointers& clips)
{
    return is_sorted(
        clips.begin(), clips.end(), CompareClipPointersByPlayStartTime);
}

inline bool IsSortedByPlayStartTime(const ClipConstPointers& clips)
{
    return is_sorted(
        clips.begin(), clips.end(), CompareClipPointersByPlayStartTime);
}

//! Get clips sorted by play start time
WAVE_TRACK_API ClipPointers SortedClipArray(WaveChannel& channel);

/*!
 @copydoc SortedClipArray(WaveChannel &)
 */
WAVE_TRACK_API ClipConstPointers SortedClipArray(const WaveChannel& channel);

//! When the time is both the end of a clip and the start of the next clip, the
//! latter clip is returned.
WAVE_TRACK_API ClipPointer GetClipAtTime(WaveChannel& channel, double time);

/*!
 @copydoc GetClipAtTime(WaveChannel&, double)
 */
WAVE_TRACK_API
ClipConstPointer GetClipAtTime(const WaveChannel& channel, double time);

WAVE_TRACK_API Envelope* GetEnvelopeAtTime(WaveChannel& channel, double time);

/*!
 Getting high-level data for one channel for screen display and clipping
 calculations and Contrast
 */
WAVE_TRACK_API std::pair<float, float> GetMinMax(const WaveChannel& channel, double t0, double t1, bool mayThrow = true);

/*!
 @copydoc GetMinMax
 */
WAVE_TRACK_API float GetRMS(const WaveChannel& channel, double t0, double t1, bool mayThrow = true);

/*!
 @brief Gets as many samples as it can, but no more than `2 *
 numSideSamples + 1`, centered around `t`. Reads nothing if
 `GetClipAtTime(t) == nullptr`. Useful to access samples across clip
 boundaries, as it spreads the read to adjacent clips, i.e., not separated
 by silence from clip at `t`.

 @return The begin and end indices of the samples in the buffer where
 samples could actually be copied.
 */
WAVE_TRACK_API std::pair<size_t, size_t>
GetFloatsCenteredAroundTime(const WaveChannel& channel, double t, float* buffer, size_t numSideSamples, bool mayThrow);

/*!
 @return true if `GetClipAtTime(t) != nullptr`, false otherwise.
 */
WAVE_TRACK_API bool GetFloatAtTime(const WaveChannel& channel, double t, float& value, bool mayThrow);

/*!
 @copydoc GetFloatAtTime(const WaveChannel &, double, size_t, float &, bool)
 */
WAVE_TRACK_API bool GetFloatAtTime(const Clip& clip, double t, float& value, bool mayThrow);

/*!
 @brief Similar to GetFloatsCenteredAroundTime, but for writing. Sets as
 many samples as it can according to the same rules as
 GetFloatsCenteredAroundTime. Leaves the other samples untouched. @see
 GetFloatsCenteredAroundTime
 */
WAVE_TRACK_API void SetFloatsCenteredAroundTime(WaveChannel& channel, double t, const float* buffer, size_t numSideSamples,
                                                sampleFormat effectiveFormat);

/*!
 @brief Sets sample nearest to `t` to `value`. Silently fails if
 `GetClipAtTime(t) == nullptr`.
 */
WAVE_TRACK_API void SetFloatAtTime(WaveChannel& channel, double t, float value, sampleFormat effectiveFormat);

/*!
 @brief Provides a means of setting clip values as a function of time.
 Included are closest sample to t0 up to closest sample to t1, exclusively.
 If the given interval is empty, i.e., `t0 >= t1`, no action is taken.
 @param producer a function taking sample (absolute, not clip-relative)
 time and returning the desired value for the sample at that time.
 */
WAVE_TRACK_API void SetFloatsWithinTimeRange(WaveChannel& channel, double t0, double t1,
                                             const std::function<float(double sampleTime)>& producer, sampleFormat effectiveFormat);

/*!
 @brief Helper for GetFloatsCenteredAroundTime. If `direction ==
 PlaybackDirection::Backward`, fetches samples to the left of `t`,
 excluding `t`, without reversing. @see GetFloatsCenteredAroundTime

 @return The number of samples actually copied.
 */
WAVE_TRACK_API size_t GetFloatsFromTime(const WaveChannel& channel, double t, float* buffer, size_t numSamples, bool mayThrow,
                                        PlaybackDirection direction);

/*!
 @brief Similar to GetFloatsFromTime, but for writing. Sets as many samples
 as it can according to the same rules as GetFloatsFromTime. Leaves the
 other samples untouched. @see GetFloatsFromTime
 */
WAVE_TRACK_API void SetFloatsFromTime(WaveChannel& channel, double t, const float* buffer, size_t numSamples, sampleFormat effectiveFormat,
                                      PlaybackDirection direction);

/*!
 @copydoc SetFloatsFromTime(WaveChannel &, double, const float *, size_t,
    sampleFormat, PlaybackDirection)
 */
WAVE_TRACK_API void SetFloatsFromTime(Clip& channel, double t, const float* buffer, size_t numSamples, sampleFormat effectiveFormat,
                                      PlaybackDirection direction);

/*!
 @brief Similar to GetNextClip, but returns `nullptr` if the neighbour
 clip is not adjacent.
 @pre `IsSortedByPlayStartTime(clips)`
 */
WAVE_TRACK_API
ClipConstPointer GetAdjacentClip(const ClipConstPointers& clips, const Clip& clip, PlaybackDirection searchDirection);

/*!
 @copydoc GetAdjacentClip(const ClipConstPointers &,
    const Clip &, PlaybackDirection) const
 */
WAVE_TRACK_API ClipPointer GetAdjacentClip(const ClipPointers& clips, const Clip& clip, PlaybackDirection searchDirection);

/*!
 @brief Returns clips next to `clip` in the given direction, or `nullptr`
 * if there is none.
 */
WAVE_TRACK_API ClipConstPointer GetNextClip(const ClipConstPointers& clips, const Clip& clip, PlaybackDirection searchDirection);

/*!
 @copydoc GetNextClip(const ClipConstPointers &, PlaybackDirection) const
 */
WAVE_TRACK_API ClipPointer GetNextClip(const ClipPointers& clips, const Clip& clip, PlaybackDirection searchDirection);

WAVE_TRACK_API ClipPointer GetIntervalAtTime(WaveChannel& channel, double t);
}

#endif
