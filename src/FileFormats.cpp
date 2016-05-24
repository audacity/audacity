/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormats.cpp

  Dominic Mazzoni

*******************************************************************//*!

\file FileFormats.cpp
\brief Works with libsndfile to provide encoding and other file
information.

*//*******************************************************************/


#include <wx/arrstr.h>
#include <wx/intl.h>
#include "sndfile.h"

#ifndef SNDFILE_1
#error Requires libsndfile 1.0 or higher
#endif

#include "FileFormats.h"

#include "Internat.h"

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
   SF_FORMAT_INFO	format_info;

   memset(&format_info, 0, sizeof(format_info));
   format_info.format = format;
   sf_command(NULL, SFC_GET_FORMAT_MAJOR,
              &format_info, sizeof (format_info)) ;

   return LAT1CTOWX(format_info.name);
}

unsigned int sf_header_index_to_type(int i)
{
   SF_FORMAT_INFO	format_info ;

   memset(&format_info, 0, sizeof(format_info));
   format_info.format = i;
   sf_command (NULL, SFC_GET_FORMAT_MAJOR,
               &format_info, sizeof (format_info));

   return format_info.format & SF_FORMAT_TYPEMASK;
}

//
// enumerating encodings
//

int sf_num_encodings()
{
   int count ;

   sf_command (NULL, SFC_GET_FORMAT_SUBTYPE_COUNT, &count, sizeof (int)) ;

   return count;
}

wxString sf_encoding_index_name(int i)
{
   SF_FORMAT_INFO	format_info ;

   memset(&format_info, 0, sizeof(format_info));
   format_info.format = i;
   sf_command (NULL, SFC_GET_FORMAT_SUBTYPE,
               &format_info, sizeof (format_info));
   return sf_normalize_name(format_info.name);
}

unsigned int sf_encoding_index_to_subtype(int i)
{
   SF_FORMAT_INFO	format_info ;

   memset(&format_info, 0, sizeof(format_info));
   format_info.format = i;
   sf_command (NULL, SFC_GET_FORMAT_SUBTYPE,
               &format_info, sizeof (format_info));

   return format_info.format & SF_FORMAT_SUBMASK;
}

//
// getting info about an actual SF format
//

wxString sf_header_name(int format)
{
   SF_FORMAT_INFO	format_info;

   memset(&format_info, 0, sizeof(format_info));
   format_info.format = (format & SF_FORMAT_TYPEMASK);
   sf_command(NULL, SFC_GET_FORMAT_INFO, &format_info, sizeof(format_info));

   return LAT1CTOWX(format_info.name);
}

wxString sf_header_shortname(int format)
{
   SF_FORMAT_INFO	format_info;
   char *tmp;
   int i;
   wxString s;

   memset(&format_info, 0, sizeof(format_info));
   format_info.format = (format & SF_FORMAT_TYPEMASK);
   sf_command(NULL, SFC_GET_FORMAT_INFO, &format_info, sizeof(format_info));

   tmp = (char *)malloc(strlen(format_info.name)+1);
   strcpy(tmp, format_info.name);
   i = 0;
   while(tmp[i]) {
      if (tmp[i]==' ')
         tmp[i] = 0;
      else
         i++;
   }

   s = LAT1CTOWX(tmp);

   free(tmp);

   return s;
}

wxString sf_header_extension(int format)
{
   SF_FORMAT_INFO	format_info;

   memset(&format_info, 0, sizeof(format_info));
   format_info.format = (format & SF_FORMAT_TYPEMASK);
   sf_command(NULL, SFC_GET_FORMAT_INFO, &format_info, sizeof(format_info));

   return LAT1CTOWX(format_info.extension);
}

wxString sf_encoding_name(int encoding)
{
   SF_FORMAT_INFO	format_info;

   memset(&format_info, 0, sizeof(format_info));
   format_info.format = (encoding & SF_FORMAT_SUBMASK);
   sf_command(NULL, SFC_GET_FORMAT_INFO, &format_info, sizeof(format_info));

   return sf_normalize_name(format_info.name);
}

int sf_num_simple_formats()
{
   int count ;

   sf_command (NULL, SFC_GET_SIMPLE_FORMAT_COUNT, &count, sizeof (int)) ;

   return count;
}

static SF_FORMAT_INFO g_format_info;

SF_FORMAT_INFO *sf_simple_format(int i)
{
   memset(&g_format_info, 0, sizeof(g_format_info));

   g_format_info.format = i;
   sf_command (NULL, SFC_GET_SIMPLE_FORMAT,
               &g_format_info, sizeof(g_format_info));

   return &g_format_info;
}

bool sf_subtype_more_than_16_bits(unsigned int format)
{
   unsigned int subtype = format & SF_FORMAT_SUBMASK;
   return (subtype == SF_FORMAT_FLOAT ||
           subtype == SF_FORMAT_DOUBLE ||
           subtype == SF_FORMAT_PCM_24 ||
           subtype == SF_FORMAT_PCM_32);
}

bool sf_subtype_is_integer(unsigned int format)
{
   unsigned int subtype = format & SF_FORMAT_SUBMASK;
   return (subtype == SF_FORMAT_PCM_16 ||
           subtype == SF_FORMAT_PCM_24 ||
           subtype == SF_FORMAT_PCM_32);
}

wxArrayString sf_get_all_extensions()
{
   wxArrayString exts;
   SF_FORMAT_INFO	format_info;
   int count, k;

   memset(&format_info, 0, sizeof(format_info));

   sf_command(NULL, SFC_GET_FORMAT_MAJOR_COUNT,
              &count, sizeof(count));

   for(k=0; k<count; k++) {
      format_info.format = k;
      sf_command(NULL, SFC_GET_FORMAT_MAJOR,
                 &format_info, sizeof (format_info)) ;

      exts.Add(LAT1CTOWX(format_info.extension));
   }

   // Some other extensions that are often sound files
   // but aren't included by libsndfile

   exts.Add(wxT("aif")); // AIFF file with a DOS-style extension
   exts.Add(wxT("ircam"));
   exts.Add(wxT("snd"));
   exts.Add(wxT("svx"));
   exts.Add(wxT("svx8"));
   exts.Add(wxT("sv16"));

   return exts;
}

wxString sf_normalize_name(const char *name)
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

OSType sf_header_mactype(int format)
{
   if (format >= 0x10000)
      return MacNames[(format/0x10000)-1];
   else if (format>=0 && format<NUM_HEADERS)
      return MacNames[format];
   else
      return '\?\?\?\?';
}

#endif // __WXMAC__

#include <wx/msgdlg.h>
ODLock libSndFileMutex;

void SFFileCloser::operator() (SNDFILE *sf) const
{
   auto err = SFCall<int>(sf_close, sf);
   if (err) {
      char buffer[1000];
      sf_error_str(sf, buffer, 1000);
      wxMessageBox(wxString::Format
         /* i18n-hint: %s will be the error message from libsndfile */
         (_("Error (file may not have been written): %s"),
         buffer));
   }
}
