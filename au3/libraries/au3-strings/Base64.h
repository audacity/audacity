/**********************************************************************

 Audacity: A Digital Audio Editor

 @file Base64.h

 @brief Base64 encode/decode (extracted from Audacity sources)

 **********************************************************************/

#pragma once

class wxString;

namespace Base64 {
STRINGS_API
wxString Encode(const void* in, int len);

STRINGS_API
int Decode(const wxString& in, void* out);
}
