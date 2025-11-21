/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

Audacity: A Digital Audio Editor

GetMeterUsingTatumQuantizationFit.h

Matthieu Hodgkinson

A method to classify audio recordings in loops and non-loops, with a
confidence score, together with a BPM estimate.

The method evaluates the assumption that the given audio is a loop. Based on
this assumption, and finite possible tempi and time signatures, a set of
hypotheses is tested. For each hypothesis, a tatum* quantization is tried,
returning an average of the normalized distance between Onset Detection Function
(ODF) peaks and the closest tatum, weighted by the ODF peak values. This yields
a single scalar that strongly correlates with the fact that the audio is a loop
or not, and that we use for loop/non-loop classification.

Besides this score, the classification stage also yields the most likely tatum
rate, which still needs disambiguation to find the beat rate. The
autocorrelation of the ODF is taken, and, for each bar division explaining the
tatum rate, is comb-filtered. The energy of the comb-filtering together with the
BPM likelihood are combined together, and the BPM with largest score is
returned.

This approach is in some aspects like existing tempo detection methods (e.g.
Percival, Graham & Tzanetakis, George (2014), implemented in the Essentia
framework at https://essentia.upf.edu/), insofar as it first derives an ODF and
then somehow correlates it with expected rhythmic patterns. However, the
quantization distance, at the core of the method, is not known by the author to
be used in other methods. Also, once the ODF is taken, the loop assumption lends
itself to a single analysis of the entire ODF, rather than performing mid-term
analyses which are then combined together. Finally, albeit restricting the use
of application, the loop assumption reduces the number of tried hypotheses,
reducing the risk of non-musical recordings to be detected as musical by sheer
luck. This increased robustness of the algorithm against false positives is
quintessential for Audacity, where non-music users should not be bothered by
wrong detections. The loop assumption is nevertheless not fundamental, and the
algorithm could be implemented without it, at the cost of a higher risk of false
positives.

Evaluation and benchmarking code can be found in
TatumQuantizationFitBenchmarking.cpp. This code takes a tolerable false-positive
rate, and outputs the corresponding loop/non-loop threshold. It also returns the
Octave Error accuracy measure, as introduced in "Schreiber, H., et al. (2020).
Music Tempo Estimation: Are We Done Yet?".

*A tatum is the smallest rhythmic unit in a musical piece. Quoting from
https://en.wikipedia.org/wiki/Tatum_(music): "The term was coined by Jeff Bilmes
(...) and is named after the influential jazz pianist Art Tatum, "whose tatum
was faster than all others""

**********************************************************************/

#pragma once

#include "MirTypes.h"

#include <functional>
#include <optional>

namespace MIR {
class MirAudioReader;

/*!
 * @brief Get the BPM of the given audio file, using the Tatum Quantization Fit
 * method.
 */
std::optional<MusicalMeter> GetMeterUsingTatumQuantizationFit(
    const MirAudioReader& audio, FalsePositiveTolerance tolerance, const std::function<void(double)>& progressCallback,
    QuantizationFitDebugOutput* debugOutput);
} // namespace MIR
