/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormats.cpp

  Dominic Mazzoni

*******************************************************************//*!

\file FileFormats.cpp
\brief Works with libsndfile to provide encoding and other file
information.

*//*******************************************************************/

#include "FileFormats.h"

#include "wxArrayStringEx.h"
#include "Internat.h"
#include "MemoryX.h"
#include "BasicUI.h"

#ifndef SNDFILE_1
#error Requires libsndfile 1.0 or higher
#endif

//
// enumerating headers
//

int sf_num_headers()
{
    int count;

    sf_command(NULL, SFC_GET_FORMAT_MAJOR_COUNT,
               &count, sizeof(count));

    return count;
}

wxString sf_header_index_name(int format)
{
    SF_FORMAT_INFO format_info;

    memset(&format_info, 0, sizeof(format_info));
    format_info.format = format;
    sf_command(NULL, SFC_GET_FORMAT_MAJOR,
               &format_info, sizeof(format_info));

    return LAT1CTOWX(format_info.name);
}

unsigned int sf_header_index_to_type(int i)
{
    SF_FORMAT_INFO format_info;

    memset(&format_info, 0, sizeof(format_info));
    format_info.format = i;
    sf_command(NULL, SFC_GET_FORMAT_MAJOR,
               &format_info, sizeof(format_info));

    return format_info.format & SF_FORMAT_TYPEMASK;
}

//
// enumerating encodings
//

int sf_num_encodings()
{
    int count;

    sf_command(NULL, SFC_GET_FORMAT_SUBTYPE_COUNT, &count, sizeof(int));

    return count;
}

wxString sf_encoding_index_name(int i)
{
    SF_FORMAT_INFO format_info;

    memset(&format_info, 0, sizeof(format_info));
    format_info.format = i;
    sf_command(NULL, SFC_GET_FORMAT_SUBTYPE,
               &format_info, sizeof(format_info));
    return sf_normalize_name(format_info.name);
}

unsigned int sf_encoding_index_to_subtype(int i)
{
    SF_FORMAT_INFO format_info;

    memset(&format_info, 0, sizeof(format_info));
    format_info.format = i;
    sf_command(NULL, SFC_GET_FORMAT_SUBTYPE,
               &format_info, sizeof(format_info));

    return format_info.format & SF_FORMAT_SUBMASK;
}

//
// getting info about an actual SF format
//

wxString sf_header_name(int format)
{
    SF_FORMAT_INFO format_info;

    memset(&format_info, 0, sizeof(format_info));
    format_info.format = (format & SF_FORMAT_TYPEMASK);
    sf_command(NULL, SFC_GET_FORMAT_INFO, &format_info, sizeof(format_info));

    return LAT1CTOWX(format_info.name);
}

wxString sf_header_shortname(int format)
{
    SF_FORMAT_INFO format_info;
    int i;
    wxString s;

    memset(&format_info, 0, sizeof(format_info));
    format_info.format = (format & SF_FORMAT_TYPEMASK);
    sf_command(NULL, SFC_GET_FORMAT_INFO, &format_info, sizeof(format_info));

    MallocString<> tmp { strdup(format_info.name) };
    i = 0;
    while (tmp[i]) {
        if (tmp[i] == ' ') {
            tmp[i] = 0;
        } else {
            i++;
        }
    }

    s = LAT1CTOWX(tmp.get());

    return s;
}

wxString sf_header_extension(int format)
{
    SF_FORMAT_INFO format_info;

    memset(&format_info, 0, sizeof(format_info));
    format_info.format = (format & SF_FORMAT_TYPEMASK);
    sf_command(NULL, SFC_GET_FORMAT_INFO, &format_info, sizeof(format_info));

    return LAT1CTOWX(format_info.extension);
}

wxString sf_encoding_name(int encoding)
{
    SF_FORMAT_INFO format_info;

    memset(&format_info, 0, sizeof(format_info));
    format_info.format = (encoding & SF_FORMAT_SUBMASK);
    sf_command(NULL, SFC_GET_FORMAT_INFO, &format_info, sizeof(format_info));

    return sf_normalize_name(format_info.name);
}

int sf_num_simple_formats()
{
    int count;

    sf_command(NULL, SFC_GET_SIMPLE_FORMAT_COUNT, &count, sizeof(int));

    return count;
}

static SF_FORMAT_INFO g_format_info;

SF_FORMAT_INFO* sf_simple_format(int i)
{
    memset(&g_format_info, 0, sizeof(g_format_info));

    g_format_info.format = i;
    sf_command(NULL, SFC_GET_SIMPLE_FORMAT,
               &g_format_info, sizeof(g_format_info));

    return &g_format_info;
}

bool sf_subtype_more_than_16_bits(unsigned int format)
{
    unsigned int subtype = format & SF_FORMAT_SUBMASK;
    return subtype == SF_FORMAT_FLOAT
           || subtype == SF_FORMAT_DOUBLE
           || subtype == SF_FORMAT_PCM_24
           || subtype == SF_FORMAT_PCM_32;
}

bool sf_subtype_is_integer(unsigned int format)
{
    unsigned int subtype = format & SF_FORMAT_SUBMASK;
    return subtype == SF_FORMAT_PCM_16
           || subtype == SF_FORMAT_PCM_24
           || subtype == SF_FORMAT_PCM_32;
}

int sf_subtype_bytes_per_sample(unsigned int format)
{
    unsigned int subtype = format & SF_FORMAT_SUBMASK;
    if (subtype == SF_FORMAT_PCM_S8) {
        return 1;
    }
    if (subtype == SF_FORMAT_PCM_U8) {
        return 1;
    }
    if (subtype == SF_FORMAT_PCM_16) {
        return 2;
    }
    if (subtype == SF_FORMAT_PCM_24) {
        return 3;
    }
    if (subtype == SF_FORMAT_PCM_32) {
        return 4;
    }
    if (subtype == SF_FORMAT_FLOAT) {
        return 4;
    }
    if (subtype == SF_FORMAT_DOUBLE) {
        return 8;
    }

    // might be different to 2, but this is good enough for
    // WAV and AIFF file size error trapping.
    return 2;
}

sampleFormat sf_subtype_to_effective_format(unsigned int format)
{
    unsigned int subtype = format & SF_FORMAT_SUBMASK;
    if (subtype == SF_FORMAT_PCM_24) {
        return int24Sample;
    } else if (sf_subtype_more_than_16_bits(format)) {
        return widestSampleFormat;
    } else {
        return int16Sample;
    }
}

FileExtensions sf_get_all_extensions()
{
    FileExtensions exts;
    SF_FORMAT_INFO format_info;
    int count, k;

    memset(&format_info, 0, sizeof(format_info));

    sf_command(NULL, SFC_GET_FORMAT_MAJOR_COUNT,
               &count, sizeof(count));

    for (k=0; k < count; k++) {
        format_info.format = k;
        sf_command(NULL, SFC_GET_FORMAT_MAJOR,
                   &format_info, sizeof(format_info));

        exts.push_back(LAT1CTOWX(format_info.extension));
    }

    // Some other extensions that are often sound files
    // but aren't included by libsndfile

    exts.insert(exts.end(), {
        wxT("aif"), // AIFF file with a DOS-style extension
        wxT("ircam"),
        wxT("snd"),
        wxT("svx"),
        wxT("svx8"),
        wxT("sv16"),
    });

    return exts;
}

wxString sf_normalize_name(const char* name)
{
    wxString n = LAT1CTOWX(name);

    n.Replace(wxT("8 bit"), wxT("8-bit"));
    n.Replace(wxT("16 bit"), wxT("16-bit"));
    n.Replace(wxT("24 bit"), wxT("24-bit"));
    n.Replace(wxT("32 bit"), wxT("32-bit"));
    n.Replace(wxT("64 bit"), wxT("64-bit"));

    return n;
}

#ifdef __WXMAC__

// TODO: find out the appropriate OSType
// for the ones with an '????'.  The others
// are at least the same type used by
// SoundApp.

#define NUM_HEADERS 13

//
// Mac OS 4-char type
//

# ifdef __UNIX__
#  include <CoreServices/CoreServices.h>
# else
#  include <Types.h>
# endif

OSType MacNames[NUM_HEADERS] = {
    'WAVE', // WAVE
    'AIFF', // AIFF
    'NeXT', // Sun/NeXT AU
    'BINA', // RAW i.e. binary
    'PAR ', // ??? Ensoniq PARIS
    '8SVX', // Amiga IFF / SVX8
    'NIST', // ??? NIST/Sphere
    'VOC ', // VOC
    '\?\?\?\?', // ?? Propellorheads Rex
    'SF  ', // ?? IRCAM
    'W64 ', // ?? Wave64
    'MAT4', // ?? Matlab 4
    'MAT5', // ?? Matlab 5
};

static OSType sf_header_mactype(int format)
{
    if (format >= 0x10000) {
        return MacNames[(format / 0x10000) - 1];
    } else if (format >= 0 && format < NUM_HEADERS) {
        return MacNames[format];
    } else {
        return '\?\?\?\?';
    }
}

#endif // __WXMAC__

//std::mutex libSndFileMutex;

int SFFileCloser::operator()(SNDFILE* sf) const
{
    auto err = SFCall<int>(sf_close, sf);
    if (err) {
        char buffer[1000];
        sf_error_str(sf, buffer, 1000);
        BasicUI::ShowMessageBox(
            /* i18n-hint: %s will be the error message from the libsndfile software library */
            XO("Error (file may not have been written): %s")
            // Not attempting to localize error messages
            // from the library
            .Format(buffer));
    }
    return err;
}
