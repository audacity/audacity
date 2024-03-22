#ifndef MU_GLOBAL_NUMBER_T_H
#define MU_GLOBAL_NUMBER_T_H

#include <limits>
#include <cassert>
#include <cmath>

#include "global/logstream.h"

namespace mu {
// default
inline constexpr double COMPARE_REAL_PREC(7);

inline constexpr double _pow_minus10(int prec)
{
    double result = 1.0;
    for (int i = 0; i < prec; ++i) {
        result /= 10;
    }
    return result;
}

// use
inline constexpr double _compare_real_epsilon = _pow_minus10(COMPARE_REAL_PREC);

template<typename T>
inline bool is_zero(T v)
{
    if constexpr (std::numeric_limits<T>::is_integer) {
        return v == 0;
    } else {
        return std::abs(v) <= _compare_real_epsilon;
    }
}

template<typename T>
inline bool is_equal(T v1, T v2)
{
    if constexpr (std::numeric_limits<T>::is_integer) {
        return v1 == v2;
    } else {
        return std::abs(v1 - v2) <= std::max(std::abs(v1), std::abs(v2)) * _compare_real_epsilon;
    }
}

template<typename T>
inline T check_valid(T v, T def = T())
{
    if constexpr (std::numeric_limits<T>::is_integer) {
        return v;
    } else {
        bool valid = !std::isnan(v) && !std::isinf(v);
        if (!valid) {
            assert(!std::isnan(v) && !std::isinf(v));
            return def;
        }
        return v;
    }
}

template<typename T>
class number_t
{
public:

    number_t() = default;
    number_t(T v)
        : m_val(mu::check_valid(v)) {}

    number_t<T>& operator=(T v) { m_val = mu::check_valid(v); return *this; }

    inline operator T() const {
        return m_val;
    }

    template<typename V>
    static T cast(V v) { return static_cast<T>(v); }

    inline T raw() const { return m_val; }
    inline double to_double() const { return static_cast<double>(m_val); }
    inline number_t<T> operator-() const { return number_t<T>(-m_val); }

    inline bool is_zero() const { return mu::is_zero(m_val); }
    inline bool is_negative() const { return m_val < static_cast<T>(0); }
    inline bool is_positive() const { return m_val > static_cast<T>(0); }

    inline bool operator ==(const number_t<T>& n) const { return mu::is_equal(m_val, n.m_val); }
    inline bool operator !=(const number_t<T>& n) const { return !mu::is_equal(m_val, n.m_val); }
    inline bool operator ==(T n) const { return mu::is_equal(m_val, n); }
    inline bool operator !=(T n) const { return !mu::is_equal(m_val, n); }

    inline bool operator>(const number_t<T>& n) const { return m_val > n.m_val; }
    inline bool operator<(const number_t<T>& n) const { return m_val < n.m_val; }
    inline bool operator>(T n) const { return m_val > n; }
    inline bool operator<(T n) const { return m_val < n; }

    inline bool operator>=(const number_t<T>& n) const { return m_val > n.m_val || mu::is_equal(m_val, n.m_val); }
    inline bool operator<=(const number_t<T>& n) const { return m_val < n.m_val || mu::is_equal(m_val, n.m_val); }
    inline bool operator>=(T n) const { return m_val > n || mu::is_equal(m_val, n); }
    inline bool operator<=(T n) const { return m_val < n || mu::is_equal(m_val, n); }

    inline number_t<T> operator+(const number_t<T>& n) const { return number_t<T>(m_val + n.m_val); }
    inline number_t<T> operator-(const number_t<T>& n) const { return number_t<T>(m_val - n.m_val); }
    inline number_t<T> operator+(T n) const { return number_t<T>(m_val + n); }
    inline number_t<T> operator-(T n) const { return number_t<T>(m_val - n); }

    inline number_t<T>& operator+=(const number_t<T>& n) { m_val += mu::check_valid(n.m_val); return *this; }
    inline number_t<T>& operator-=(const number_t<T>& n) { m_val -= mu::check_valid(n.m_val); return *this; }
    inline number_t<T>& operator+=(T n) { m_val += mu::check_valid(n); return *this; }
    inline number_t<T>& operator-=(T n) { m_val -= mu::check_valid(n); return *this; }

    inline number_t<T> operator/(const number_t<T>& n) const { return number_t<T>(m_val / n.m_val); }
    inline number_t<T> operator*(const number_t<T>& n) const { return number_t<T>(m_val * n.m_val); }
    inline number_t<T> operator/(T n) const { return number_t<T>(m_val / n); }
    inline number_t<T> operator*(T n) const { return number_t<T>(m_val * n); }

    inline number_t<T>& operator/=(const number_t<T>& n) { m_val /= mu::check_valid(n.m_val, T(1)); return *this; }
    inline number_t<T>& operator*=(const number_t<T>& n) { m_val *= mu::check_valid(n.m_val); return *this; }
    inline number_t<T>& operator/=(T n) { m_val /= mu::check_valid(n, T(1)); return *this; }
    inline number_t<T>& operator*=(T n) { m_val *= mu::check_valid(n); return *this; }

private:

    T m_val = T();
};

template<typename T>
inline number_t<T> operator*(T a, const number_t<T>& b) { return number_t<T>(a * b.raw()); }

using integer_t = number_t<int>;
using real_t = number_t<double>;

template<typename T>
const T& min(const T& a, const T& b) { return std::min(a, b); }
template<typename T>
const T& min(const number_t<T>& a, const T& b) { return std::min(a.raw(), b); }
template<typename T>
const T& min(const T& a, const number_t<T>& b) { return std::min(a, b.raw()); }

template<typename T>
const T& max(const T& a, const T& b) { return std::max(a, b); }
template<typename T>
const T& max(const number_t<T>& a, const T& b) { return std::max(a.raw(), b); }
template<typename T>
const T& max(const T& a, const number_t<T>& b) { return std::max(a, b.raw()); }

template<typename T>
inline T divide(const T& dividend, const T& divisor)
{
    if (is_zero(divisor)) {
        assert(!is_zero(divisor));
        return T();
    }
    return dividend / divisor;
}

#if (defined (_MSCVER) || defined (_MSC_VER))
#pragma warning(push)
#pragma warning(disable: 4723) // potential divide by 0
#endif
template<typename T>
inline T divide(const T& dividend, const T& divisor, const T& def)
{
    if (is_zero(divisor)) {
        return def;
    }
    return dividend / divisor;
}

#if (defined (_MSCVER) || defined (_MSC_VER))
#pragma warning(pop)
#endif

template<typename T>
inline number_t<T> divide(const number_t<T>& dividend, const number_t<T>& divisor, const number_t<T>& def)
{
    if (divisor.is_zero()) {
        return def;
    }
    return dividend / divisor;
}
}

template<typename T>
inline mu::logger::Stream& operator<<(mu::logger::Stream& s, const mu::number_t<T>& n)
{
    s << n.raw();
    return s;
}

#endif // MU_GLOBAL_NUMBER_T_H
