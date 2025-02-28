/**********************************************************************

 Audacity: A Digital Audio Editor

 @file IteratorX.h

 C++ standard header <iterator> with a few extensions

 Paul Licameli split from MemoryX.h

 **********************************************************************/
#ifndef __AUDACITY_ITERATOR_X__
#define __AUDACITY_ITERATOR_X__

#include <algorithm>
#include <functional>
#include <iterator>
#include <limits>

/**
  \brief A convenience for defining iterators that return rvalue types, so that
  they cooperate correctly with stl algorithms and std::reverse_iterator
 */
template< typename Value, typename Category = std::forward_iterator_tag >
struct ValueIterator {
    using iterator_category = Category;
    using value_type = Value;
    using difference_type = ptrdiff_t;
    // void pointer type so that operator -> is disabled
    using pointer = void;
    // make "reference type" really the same as the value type
    using reference = const Value;
};

/**
  \brief A convenience for use with range-for
 */
template<typename Iterator>
struct IteratorRange : public std::pair<Iterator, Iterator> {
    using iterator = Iterator;
    using reverse_iterator = std::reverse_iterator<Iterator>;

    IteratorRange (const Iterator& a, const Iterator& b)
        : std::pair<Iterator, Iterator>(a, b) {}

    IteratorRange (Iterator&& a, Iterator&& b)
        : std::pair<Iterator, Iterator>(std::move(a), std::move(b)) {}

    IteratorRange< reverse_iterator > reversal() const
    { return { this->rbegin(), this->rend() }; }

    Iterator begin() const { return this->first; }
    Iterator end() const { return this->second; }

    reverse_iterator rbegin() const { return reverse_iterator{ this->second }; }
    reverse_iterator rend() const { return reverse_iterator{ this->first }; }

    bool empty() const { return this->begin() == this->end(); }
    explicit operator bool() const {
        return !this->empty();
    }
    size_t size() const { return std::distance(this->begin(), this->end()); }

    template<typename T> iterator find(const T& t) const
    { return std::find(this->begin(), this->end(), t); }

    template<typename T> long index(const T& t) const
    {
        auto iter = this->find(t);
        if (iter == this->end()) {
            return -1;
        }
        return std::distance(this->begin(), iter);
    }

    template<typename T> bool contains(const T& t) const
    { return this->end() != this->find(t); }

    template<typename F> iterator find_if(const F& f) const
    { return std::find_if(this->begin(), this->end(), f); }

    template<typename F> long index_if(const F& f) const
    {
        auto iter = this->find_if(f);
        if (iter == this->end()) {
            return -1;
        }
        return std::distance(this->begin(), iter);
    }

    // to do: use std::all_of, any_of, none_of when available on all platforms
    template<typename F> bool all_of(const F& f) const
    {
        auto notF
            =[&](typename std::iterator_traits<Iterator>::reference v)
        { return !f(v); };
        return !this->any_of(notF);
    }

    template<typename F> bool any_of(const F& f) const
    { return this->end() != this->find_if(f); }

    template<typename F> bool none_of(const F& f) const
    { return !this->any_of(f); }

    template<typename T> struct identity
    {
        const T&& operator ()(T&& v) const
        {
            return std::forward(v);
        }
    };

    // Like std::accumulate, but the iterators implied, and with another
    // unary operation on the iterator value, pre-composed
    template<
        typename R,
        typename Binary = std::plus< R >,
        typename Unary = identity< decltype(*std::declval<Iterator>()) >
        >
    R accumulate(
        R init,
        Binary binary_op = {},
        Unary unary_op = {}) const
    {
        R result = init;
        for (auto&& v : *this) {
            result = binary_op(result, unary_op(v));
        }
        return result;
    }

    // An overload making it more convenient to use with pointers to member
    // functions
    template<
        typename R,
        typename Binary = std::plus< R >,
        typename R2, typename C
        >
    R accumulate(
        R init,
        Binary binary_op,
        R2 (C :: * pmf) () const) const
    {
        return this->accumulate(init, binary_op, std::mem_fn(pmf));
    }

    // Some accumulations frequent enough to be worth abbreviation:
    template<
        typename Unary = identity< decltype(*std::declval<Iterator>()) >,
        typename R = decltype(std::declval<Unary>()(*std::declval<Iterator>()))
        >
    R min(Unary unary_op = {}) const
    {
        return this->accumulate(
            std::numeric_limits< R >::max(),
            (const R& (*)(const R&, const R&)) std::min,
            unary_op
            );
    }

    template<
        typename R2, typename C,
        typename R = R2
        >
    R min(R2 (C :: * pmf) () const) const
    {
        return this->min(std::mem_fn(pmf));
    }

    template<
        typename Unary = identity< decltype(*std::declval<Iterator>()) >,
        typename R = decltype(std::declval<Unary>()(*std::declval<Iterator>()))
        >
    R max(Unary unary_op = {}) const
    {
        return this->accumulate(
            std::numeric_limits< R >::lowest(),
            (const R& (*)(const R&, const R&)) std::max,
            unary_op
            );
    }

    template<
        typename R2, typename C,
        typename R = R2
        >
    R max(R2 (C :: * pmf) () const) const
    {
        return this->max(std::mem_fn(pmf));
    }

    template<
        typename Unary = identity< decltype(*std::declval<Iterator>()) >,
        typename R = decltype(std::declval<Unary>()(*std::declval<Iterator>()))
        >
    R sum(Unary unary_op = {}) const
    {
        return this->accumulate(
            R { 0 },
            std::plus< R > {},
            unary_op
            );
    }

    template<
        typename R2, typename C,
        typename R = R2
        >
    R sum(R2 (C :: * pmf) () const) const
    {
        return this->sum(std::mem_fn(pmf));
    }
};

template< typename Iterator>
IteratorRange< Iterator >
make_iterator_range(const Iterator& i1, const Iterator& i2)
{
    return { i1, i2 };
}

template< typename Container >
IteratorRange< typename Container::iterator >
make_iterator_range(Container& container)
{
    return { container.begin(), container.end() };
}

template< typename Container >
IteratorRange< typename Container::const_iterator >
make_iterator_range(const Container& container)
{
    return { container.begin(), container.end() };
}

// A utility function building a container of results
template< typename Container, typename Iterator, typename Function >
Container transform_range(Iterator first, Iterator last, Function&& fn)
{
    Container result;
    std::transform(first, last, std::back_inserter(result), fn);
    return result;
}

// A utility function, often constructing a vector from another vector
template< typename OutContainer, typename InContainer, typename Function >
OutContainer transform_container(InContainer& inContainer, Function&& fn)
{
    return transform_range<OutContainer>(
        inContainer.begin(), inContainer.end(), fn);
}

template<typename Range, typename Function>
void for_each_in_range(Range&& range, Function&& fn)
{
    std::for_each(range.begin(), range.end(), std::forward<Function>(fn));
}

template<typename Integral = int>
class NumberIterator : public ValueIterator<Integral>
{
public:
    explicit NumberIterator(Integral value)
        : mValue{value} {}

    Integral operator *() const { return mValue; }
    NumberIterator& operator ++() { ++mValue; return *this; }
    NumberIterator operator ++(int) const
    { auto result = *this; ++result; return result; }

    friend bool operator ==(NumberIterator x, NumberIterator y)
    { return x.mValue == y.mValue; }
    friend bool operator !=(NumberIterator x, NumberIterator y)
    { return !(x == y); }

private:
    Integral mValue;
};

template<typename Integral> struct IotaRange : IteratorRange<NumberIterator<Integral> >
{
    IotaRange(Integral inclusiveLower, Integral exclusiveUpper)
        : IteratorRange<NumberIterator<Integral> >{
                                                   NumberIterator { inclusiveLower }, NumberIterator { exclusiveUpper }}
    {}
};

#endif // __AUDACITY_MEMORY_X_H__
