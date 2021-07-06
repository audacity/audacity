/**********************************************************************

  Sneedacity: A Digital Audio Editor

  LangChoice.h

  Dominic Mazzoni

  Pop up a language asking the user to choose a (natural)
  language for the user interface.  Generally only popped
  up once, the first time the program is run.

**********************************************************************/

#ifndef __SNEEDACITY_LANG_CHOICE__
#define __SNEEDACITY_LANG_CHOICE__

class wxString;
class wxWindow;

wxString ChooseLanguage(wxWindow *parent);

#endif // __SNEEDACITY_LANG_CHOICE__
