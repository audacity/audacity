/**********************************************************************

 Audacity: A Digital Audio Editor

 @file Base64.cpp

 @brief Base64 encode/decode (extracted from Audacity sources)

 **********************************************************************/

#include "Base64.h"
#include <wx/string.h>

////////////////////////////////////////////////////////////////////////////////
// Base64 en/decoding
//
// Original routines marked as public domain and found at:
//
// http://en.wikibooks.org/wiki/Algorithm_implementation/Miscellaneous/Base64
//
////////////////////////////////////////////////////////////////////////////////

// Lookup table for encoding
const static wxChar cset[] = wxT("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/");
const static char padc = wxT('=');

wxString Base64::Encode(const void* in, int len)
{
    auto p = static_cast<const unsigned char*>(in);
    wxString out;

    unsigned long temp;
    for (int i = 0; i < len / 3; i++) {
        temp  = (*p++) << 16;//Convert to big endian
        temp += (*p++) << 8;
        temp += (*p++);
        out += cset[(temp & 0x00FC0000) >> 18];
        out += cset[(temp & 0x0003F000) >> 12];
        out += cset[(temp & 0x00000FC0) >> 6];
        out += cset[(temp & 0x0000003F)];
    }

    switch (len % 3) {
    case 1:
        temp  = (*p++) << 16;  //Convert to big endian
        out += cset[(temp & 0x00FC0000) >> 18];
        out += cset[(temp & 0x0003F000) >> 12];
        out += padc;
        out += padc;
        break;

    case 2:
        temp  = (*p++) << 16;  //Convert to big endian
        temp += (*p++) << 8;
        out += cset[(temp & 0x00FC0000) >> 18];
        out += cset[(temp & 0x0003F000) >> 12];
        out += cset[(temp & 0x00000FC0) >> 6];
        out += padc;
        break;
    }

    return out;
}

int Base64::Decode(const wxString& in, void* out)
{
    const auto len = in.length();
    auto p = static_cast<unsigned char*>(out);

    if (len % 4) { //Sanity check
        return 0;
    }

    //const char *a = in.mb_str();
    //Setup a vector to hold the result
    unsigned long temp = 0; //Holds decoded quanta
    size_t i = 0;
    while (i < len)
    {
        for (int quantumPosition = 0; quantumPosition < 4; quantumPosition++) {
            unsigned char c = in[i];
            temp <<= 6;

            if (c >= 0x41 && c <= 0x5A) {
                temp |= c - 0x41;
            } else if (c >= 0x61 && c <= 0x7A) {
                temp |= c - 0x47;
            } else if (c >= 0x30 && c <= 0x39) {
                temp |= c + 0x04;
            } else if (c == 0x2B) {
                temp |= 0x3E;
            } else if (c == 0x2F) {
                temp |= 0x3F;
            } else if (c == padc) {
                switch (len - i) {
                case 1: //One pad character
                    *p++ = (temp >> 16) & 0x000000FF;
                    *p++ = (temp >> 8) & 0x000000FF;
                    return p - static_cast<unsigned char*>(out);
                case 2: //Two pad characters
                    *p++ = (temp >> 10) & 0x000000FF;
                    return p - static_cast<unsigned char*>(out);
                }
            }
            i++;
        }
        *p++ = (temp >> 16) & 0x000000FF;
        *p++ = (temp >> 8) & 0x000000FF;
        *p++ = temp & 0x000000FF;
    }

    return p - static_cast<unsigned char*>(out);
}
