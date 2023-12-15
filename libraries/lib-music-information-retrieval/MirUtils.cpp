#include "MirUtils.h"

#include <cmath>
#include <numeric>

namespace MIR
{
namespace
{
constexpr auto bpmStdDev = 29.7953;
constexpr auto pi = 3.141592653589793;

constexpr bool IsPrimeDecompositionTwoThreeOnly(int n)
{
   while (n % 2 == 0)
      n /= 2;
   while (n % 3 == 0)
      n /= 3;
   return n == 1;
}

constexpr int CountThreesInPrimeDecomposition(int n)
{
   int count = 0;
   while (n % 3 == 0)
   {
      ++count;
      n /= 3;
   }
   return count;
}

// Function to generate numbers whose prime factorization contains only twos or
// threes
std::vector<int> GetPowersOf2And3(int lower, int upper)
{
   std::vector<int> result;
   for (int i = lower; i <= upper; ++i)
      if (IsPrimeDecompositionTwoThreeOnly(i))
         result.push_back(i);
   return result;
}
} // namespace

std::vector<int> GetPossibleBarDivisors(int lower, int upper)
{
   auto result = GetPowersOf2And3(lower, upper);
   result.erase(
      std::remove_if(
         result.begin(), result.end(),
         [](int n) { return CountThreesInPrimeDecomposition(n) > 2; }),
      result.end());
   return result;
}

std::vector<int> GetPeakIndices(const std::vector<float>& odf)
{
   std::vector<int> peakIndices;
   for (auto j = 1; j < odf.size() - 1; ++j)
   {
      const auto i = j == 0 ? odf.size() - 1 : j - 1;
      const auto k = j == odf.size() - 1 ? 0 : j + 1;
      if (odf[i] < odf[j] && odf[j] > odf[k])
         peakIndices.push_back(j);
   }
   return peakIndices;
}

std::vector<float> GetNormalizedHann(int size)
{
   std::vector<float> window(size);
   for (auto n = 0; n < size; ++n)
      window[n] = .5 * (1 - std::cos(2 * pi * n / size));
   const auto windowSum = std::accumulate(window.begin(), window.end(), 0.);
   std::transform(
      window.begin(), window.end(), window.begin(),
      [windowSum](double w) { return w / windowSum; });
   return window;
}
} // namespace MIR
