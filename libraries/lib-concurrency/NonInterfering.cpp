/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 NonInterfering.cpp
 
 Paul Licameli
 
 **********************************************************************/
#include "NonInterfering.h"
#include <algorithm>

// Make the symbol table non-empty
CONCURRENCY_API void lib_utility_dummy_symbol()
{}

constexpr auto sizeof_align_val = sizeof(std::align_val_t);

void *NonInterferingBase::operator new(std::size_t count, std::align_val_t al)
{
   using namespace std;
   // Get an allocation with sufficient extra space to remember the alignment
   // (And to do that, adjust the alignment to be not less than the alignment of
   // an alignment value!).
   // Also increase the allocation by one entire alignment.
   al = std::max( al, static_cast<align_val_t>( alignof(align_val_t) ) );
   const auto al_as_size = static_cast<size_t>(al);
   auto ptr = static_cast<char*>(
      ::operator new( count + sizeof_align_val + al_as_size ) );

   // Adjust the pointer to a properly aligned one, with a space just before it
   // to remember the adjustment
   ptr += sizeof_align_val;
   auto integer = reinterpret_cast<uintptr_t>(ptr);
   const auto partial = integer % al_as_size;
   auto adjustment = partial ? al_as_size - partial : 0;
   integer += adjustment;
   ptr = reinterpret_cast<char*>(integer);

   // Remember the adjustment
   *(reinterpret_cast<size_t*>(ptr) - 1) = adjustment;

   return ptr;
}

void NonInterferingBase::operator delete(void *ptr, std::align_val_t al)
{
   // Find the adjustment
   auto adjustment = *(reinterpret_cast<size_t*>(ptr) - 1);
   // Apply the adjustment
   auto p = reinterpret_cast<char*>(ptr) - adjustment - sizeof_align_val;
   // Call through to default operator
   ::operator delete(p);
}
