/**********************************************************************

 Audacity: A Digital Audio Editor

 @file wxArrayStringEx.h

 Paul Licameli split from MemoryX.h

 **********************************************************************/

#ifndef __AUDACITY_WX_ARRAY_STRING_EX__
#define __AUDACITY_WX_ARRAY_STRING_EX__

#include <wx/arrstr.h>

//! Extend wxArrayString with move operations and construction and insertion fromstd::initializer_list
class wxArrayStringEx : public wxArrayString
{
public:
    using wxArrayString::wxArrayString;
    wxArrayStringEx() = default;

    template< typename Iterator >
    wxArrayStringEx(Iterator start, Iterator finish)
    {
        this->reserve(std::distance(start, finish));
        while (start != finish) {
            this->push_back(*start++);
        }
    }

    template< typename T >
    wxArrayStringEx(std::initializer_list< T > items)
    {
        this->reserve(this->size() + items.size());
        for ( const auto& item : items ) {
            this->push_back(item);
        }
    }

    //! The move operations can take arguments of the base class wxArrayString
    wxArrayStringEx(wxArrayString&& other)
    {
        swap(other);
    }

    //! The move operations can take arguments of the base class wxArrayString
    wxArrayStringEx& operator=(wxArrayString&& other)
    {
        if (this != &other) {
            clear();
            swap(other);
        }
        return *this;
    }

    using wxArrayString::insert;

    template< typename T >
    iterator insert(const_iterator pos, std::initializer_list< T > items)
    {
        const auto index = pos - ((const wxArrayString*)this)->begin();
        this->wxArrayString::Insert({}, index, items.size());
        auto result = this->begin() + index, iter = result;
        for ( auto pItem = items.begin(), pEnd = items.end();
              pItem != pEnd;
              ++pItem, ++iter
              ) {
            *iter = *pItem;
        }
        return result;
    }
};

#endif
