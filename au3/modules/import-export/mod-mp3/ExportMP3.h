/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP3.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORTMP3__
#define __AUDACITY_EXPORTMP3__

/* --------------------------------------------------------------------------*/

#include <memory>

enum MP3RateMode : unsigned {
    MODE_SET = 0,
    MODE_VBR,
    MODE_ABR,
    MODE_CBR,
};

#if defined(__WXMSW__) || defined(__WXMAC__)
#define MP3_EXPORT_BUILT_IN 1
#endif

class TranslatableString;
class wxWindow;

//----------------------------------------------------------------------------
// Get MP3 library version
//----------------------------------------------------------------------------
TranslatableString GetMP3Version(wxWindow* parent, bool prompt);

#endif
