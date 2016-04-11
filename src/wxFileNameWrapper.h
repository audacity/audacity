/**********************************************************************

Audacity: A Digital Audio Editor

wxFileNameWrapper.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_WXFILENAMEWRAPPER__
#define __AUDACITY_WXFILENAMEWRAPPER__

// The wxFileName does not have a move constructor.
// So add one to it, so that it passes around by value more quickly.
class wxFileNameWrapper : public wxFileName
{
public:
   explicit
      wxFileNameWrapper(const wxFileName &that)
      : wxFileName(that)
   {}

   wxFileNameWrapper() = default;
   wxFileNameWrapper(const wxFileNameWrapper &that) = default;
   wxFileNameWrapper &operator= (const wxFileNameWrapper &that) = default;

   void swap(wxFileNameWrapper &that)
   {
      if (this != &that) {
         enum : size_t { Size = sizeof(*this) };
         // Do it bitwise.
         // std::aligned_storage<Size>::type buffer;
         char buffer[Size];
         memcpy(&buffer, this, Size);
         memcpy(this, &that, Size);
         memcpy(&that, &buffer, Size);
      }
   }

   // Define move copy and assignment in terms of swap
   wxFileNameWrapper(wxFileNameWrapper &&that)
   {
      swap(that);
   }

   wxFileNameWrapper &operator= (wxFileNameWrapper &&that)
   {
      if (this != &that) {
         Clear();
         swap(that);
      }
      return *this;
   }
};

#endif

