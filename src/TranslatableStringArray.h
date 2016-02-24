/**********************************************************************

Audacity: A Digital Audio Editor

TranslatableStringArray.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_TRANSLATABLE_STRING_ARRAY__
#define __AUDACITY_TRANSLATABLE_STRING_ARRAY__

#include <wx/app.h>
#include <wx/event.h>

class wxArrayString;

DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_LANGUAGE_CHANGE, -1);

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
         wxTheApp->Connect(EVT_LANGUAGE_CHANGE,
            wxCommandEventHandler(TranslatableArray::Invalidate),
            NULL,
            this);
   }

   ~TranslatableArray()
   {
      if (wxTheApp)
         wxTheApp->Disconnect(EVT_LANGUAGE_CHANGE,
            wxCommandEventHandler(TranslatableArray::Invalidate),
            NULL,
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

typedef TranslatableArray<wxArrayString> TranslatableStringArray;

#endif
