/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2Utils.h

  @brief RAII for lv2 resources

  Paul Licameli split from LV2Symbols.h

*********************************************************************/

#ifndef __AUDACITY__LV2__UTILS__
#define __AUDACITY__LV2__UTILS__

#include <memory>
#include <wx/string.h>
#include "lilv/lilv.h" // for lilv_free

//! Generate deleter classes for smart pointers to lv2 resources
template<typename Type, void(*f)(Type*)>
struct Lilv_deleter {
    void operator()(Type* p) noexcept
    {
        f(p);
    }
};

//! Generate classes of smart pointers to lv2 resources
template<typename Type, void(*f)(Type*)>
using Lilv_ptr = std::unique_ptr<Type, Lilv_deleter<Type, f> >;

static inline void free_chars(char* p) { lilv_free(p); }
using LilvCharsPtr = Lilv_ptr<char, free_chars>;

//! A smart pointer to LilvNode, which is essentially a variant type much used
//! in the lv2 API
using LilvNodePtr = Lilv_ptr<LilvNode, lilv_node_free>;

//! Use when lilv.h comments "must not be freed" or we use the node elsewhere,
//! or the node pointer is from iterating a LilvNodes collection
inline wxString LilvString(const LilvNode* node)
{
    return wxString::FromUTF8(lilv_node_as_string(node));
}

//! Use when lilv.h comments "Returned value must be freed by the caller."
//! We free it in this function.
//! Name suggests C++ move semantics applied to `node`, but only C types used
inline wxString LilvStringMove(LilvNode* node)
{
    LilvNodePtr temp{ node };
    wxString str = LilvString(node);
    return str;
}

#endif
