/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file Splittable.h
 
 Paul Licameli
 
 **********************************************************************/
#ifndef __AUDACITY_SPLITTABLE__
#define __AUDACITY_SPLITTABLE__

namespace Parallel {

/*!
 @page Concurrency concepts
 @section Splittable

 A `Splittable` type defines:
 
 - type `iterator_type` which models [LegacyForwardIterator](https://en.cppreference.com/w/cpp/named_req/ForwardIterator)
 - `iterator_type begin()`
 - `iterator_type end()`
 - `Splittable(Splittable &orig, iterator_type iter)`, the splitting
 constructor.
 
 `orig` is not `const`; see postconditions.  Ordinary copy and move constructors
 are not required.

 precondition: `orig.begin() <= iter && iter <= orig.end()`
 let `old_end = orig.end()`
 postcondition: `orig.end() == iter`
 postcondition on result: `result.begin() == iter && result.end() == old_end`

 This allows the result to have independent internal state, avoiding data races.
 */

//! Useful to simplify definition of Splittable types
template<typename Iterator> struct SplittableBase {
   using iterator_type = Iterator;
   iterator_type first, last;

   SplittableBase(iterator_type first, iterator_type last)
      : first{ first }, last{ last }
   {}

   iterator_type begin() { return first; }
   iterator_type end() { return last; }

   void set_begin(iterator_type first) { this->first = first; }
   void set_end(iterator_type last) { this->last = last; }

   //! Splitting constructor
   /*!
    Initialize and reassign the iterator members of `this` and orig
    consistently so that `this` is the immediately right sub-range to orig
    */
   SplittableBase(SplittableBase &orig, iterator_type iter)
      : first{ iter }
      , last{ orig.last }
   {
      orig.last = iter;
   }
};

}

#endif
