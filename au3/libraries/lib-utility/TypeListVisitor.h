/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file TypeListVisitor.h

  Paul Licameli

**********************************************************************/
#ifndef __AUDACITY_TYPELIST_VISITOR__
#define __AUDACITY_TYPELIST_VISITOR__

#include "TypeList.h"
#include <functional>
#include <variant>

namespace TypeListVisitor {
using namespace TypeList;

template<bool Const, typename TypeList> struct VariantOfReferences : Apply<std::variant,
                                                                           Map_t<Fn<std::reference_wrapper>, MapConst_t<Const, TypeList> > >
{};

//! Type of variant of reference wrappers to listed types,
//! possibly const-qualified
template<bool Const, typename TypeList> using VariantOfReferences_t
    =typename VariantOfReferences<Const, TypeList>::type;
}

#endif
