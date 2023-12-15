#include <cassert>
#include <numeric>
#include <utility>
#include <vector>

template <typename T, typename U>
std::pair<double, double> LinearFit(
   const std::vector<T>& x, const std::vector<U>& y, std::vector<double> w = {})
{
   assert(x.size() == y.size() && (w.empty() || w.size() == y.size()));
   if (w.empty())
      w = std::vector<double>(y.size(), 1.0);

   // Calculate weighted means
   const double w_mean_x =
      std::inner_product(x.begin(), x.end(), w.begin(), 0.0) /
      std::accumulate(w.begin(), w.end(), 0.0);
   const double w_mean_y =
      std::inner_product(y.begin(), y.end(), w.begin(), 0.0) /
      std::accumulate(w.begin(), w.end(), 0.0);

   auto n = 0;
   const double numerator = std::inner_product(
      x.begin(), x.end(), y.begin(), 0.0, std::plus<>(),
      [&](double xi, double yi) {
         return w[n++] * (xi - w_mean_x) * (yi - w_mean_y);
      });

   n = 0;
   const double denominator = std::inner_product(
      x.begin(), x.end(), x.begin(), 0.0, std::plus<>(),
      [&](double xi, double) {
         return w[n++] * (xi - w_mean_x) * (xi - w_mean_x);
      });

   // Calculate slope (a) and intercept (b)
   const double a = numerator / denominator;
   const double b = w_mean_y - a * w_mean_x;

   return std::make_pair(a, b);
}
