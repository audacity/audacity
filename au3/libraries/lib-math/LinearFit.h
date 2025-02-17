/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LinearFit.h

  Matthieu Hodgkinson

**********************************************************************/

#pragma once
#include <cassert>
#include <numeric>
#include <utility>
#include <vector>

/*!
 * @brief Linear least-square fit of a set of points `y` located at `x`, with
 * optional weights `w`.
 *
 * @pre x and y have equal size
 * @pre w is empty or has the same size as x and y
 */
template<typename X, typename Y, typename W = double>
std::pair<double, double> LinearFit(
    const std::vector<X>& x, const std::vector<Y>& y, std::vector<W> w = {})
{
    assert(x.size() == y.size() && (w.empty() || w.size() == y.size()));
    if (w.empty()) {
        w = std::vector<W>(y.size(), 1);
    }

    // Calculate weighted means
    const double xwMean = std::inner_product(x.begin(), x.end(), w.begin(), 0.)
                          / std::accumulate(w.begin(), w.end(), 0.);
    const double ywMean = std::inner_product(y.begin(), y.end(), w.begin(), 0.)
                          / std::accumulate(w.begin(), w.end(), 0.);

    auto n = 0;
    const double numerator = std::inner_product(
        x.begin(), x.end(), y.begin(), 0., std::plus<>(),
        [&](X xi, Y yi) { return w[n++] * (xi - xwMean) * (yi - ywMean); });

    n = 0;
    const double denominator = std::inner_product(
        x.begin(), x.end(), x.begin(), 0., std::plus<>(),
        [&](X xi, X) { return w[n++] * (xi - xwMean) * (xi - xwMean); });

    // Calculate slope (a) and intercept (b)
    const double a = numerator / denominator;
    const double b = ywMean - a * xwMean;

    return std::make_pair(a, b);
}
