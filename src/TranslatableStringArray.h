/**********************************************************************

Audacity: A Digital Audio Editor

TranslatableStringArray.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_TRANSLATABLE_STRING_ARRAY__
#define __AUDACITY_TRANSLATABLE_STRING_ARRAY__

#include <wx/app.h>

class wxArrayStringEx;

wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API, EVT_LANGUAGE_CHANGE, wxCommandEvent);

/*
This class can maintain a static table containing user visible strings that updates
itself properly when the language is changed in Preferences.

Typical usage is to define a derived class, override Populate(), and then
make a singleton instance of the class.

Populate() is called only as needed to fill the table on demand the first
time it is used after application startup or language change.
*/

template<typename ArrayType> class TranslatableArray  /* not final */
   : public wxEvtHandler
{
public:

   TranslatableArray()
   {
      if (wxTheApp)
         wxTheApp->Bind(EVT_LANGUAGE_CHANGE,
            &TranslatableArray::Invalidate,
            this);
   }

   const ArrayType& Get()
   {
      if (mContents.empty())
         Populate();
      return mContents;
   }

protected:
   // Override this function to fill in mContents,
   // typically by lines like
   // mContents.push_back(_("Translate me"));
   virtual void Populate() = 0;

   void Invalidate(wxCommandEvent &evt)
   {
      mContents.clear();
      evt.Skip();
   }

   ArrayType mContents;
};

typedef TranslatableArray<wxArrayStringEx> TranslatableStringArray;

#endif
