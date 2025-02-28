/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormats.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_FILE_FORMATS__
#define __AUDACITY_FILE_FORMATS__

#include "Identifier.h"
#include "SampleFormat.h"

#include <memory>
#include <sndfile.h>

class ChoiceSetting;
class wxString;

//
// enumerating headers
//

/** @brief Get the number of container formats supported by libsndfile
 *
 * Uses SFC_GET_FORMAT_MAJOR_COUNT in sf_command interface */
FILE_FORMATS_API
int sf_num_headers();

/** @brief Get the name of a container format from libsndfile
 *
 * Uses SFC_GET_FORMAT_MAJOR in the sf_command() interface. Resulting C string
 * from libsndfile is converted to a wxString
 * @param format_num The libsndfile format number for the container format
 * required
 */
FILE_FORMATS_API
wxString sf_header_index_name(int format_num);

FILE_FORMATS_API
unsigned int sf_header_index_to_type(int format_num);

//
// enumerating encodings
//
/** @brief Get the number of data encodings libsndfile supports (in any
 * container or none */
FILE_FORMATS_API
int sf_num_encodings();
/** @brief Get the string name of the data encoding of the requested format
 *
 * uses SFC_GET_FORMAT_SUBTYPE */
FILE_FORMATS_API
wxString sf_encoding_index_name(int encoding_num);
FILE_FORMATS_API
unsigned int sf_encoding_index_to_subtype(int encoding_num);

//
// getting info about an actual SF format
//
/** @brief Get the string name of the specified container format
 *
 * AND format with SF_FORMAT_TYPEMASK to get only the container format and
 * then use SFC_GET_FORMAT_INFO to get the description
 * @param format the libsndfile format to get the name for (only the container
 * part is used) */
FILE_FORMATS_API
wxString sf_header_name(int format);
/** @brief Get an abbreviated form of the string name of the specified format
 *
 * Do sf_header_name() then truncate the string at the first space in the name
 * to get just the first word of the format name.
 * @param format the libsndfile format to get the name for (only the container
 * part is used) */
FILE_FORMATS_API
wxString sf_header_shortname(int format);
/** @brief Get the most common file extension for the given format
 *
 * AND the given format with SF_FORMAT_TYPEMASK to get just the container
 * format, then retrieve the most common extension using SFC_GET_FORMAT_INFO.
 * @param format the libsndfile format to get the name for (only the container
 * part is used) */
FILE_FORMATS_API
wxString sf_header_extension(int format);
/** @brief Get the string name of the specified data encoding
 *
 * AND encoding_num with SF_FORMAT_SUBMASK to get only the data encoding and
 * then use SFC_GET_FORMAT_INFO to get the description
 * @param encoding_num the libsndfile encoding to get the name for (only the
 * data encoding is used) */
wxString sf_encoding_name(int encoding_num);

//
// simple formats
//

int sf_num_simple_formats();
SF_FORMAT_INFO* sf_simple_format(int i);

//
// other utility functions
//

FILE_FORMATS_API
bool sf_subtype_more_than_16_bits(unsigned int format);
FILE_FORMATS_API
bool sf_subtype_is_integer(unsigned int format);
FILE_FORMATS_API
int sf_subtype_bytes_per_sample(unsigned int format);

FILE_FORMATS_API
//! Choose the narrowest value in the sampleFormat enumeration for a given libsndfile format
sampleFormat sf_subtype_to_effective_format(unsigned int format);

FILE_FORMATS_API
extern FileExtensions sf_get_all_extensions();

wxString sf_normalize_name(const char* name);

// This function wrapper uses a mutex to serialize calls to the SndFile library.
// PRL: Keeping this in a comment, but with Unitary, the only remaining uses
// of libsndfile should be in import/export and so are on the main thread only
//extern std::mutex libSndFileMutex;

template<typename R, typename F, typename ... Args>
inline R SFCall(F fun, Args&&... args)
{
    //std::lock_guard<std::mutex> guard{ libSndFileMutex };
    return fun(std::forward<Args>(args)...);
}

//RAII for SNDFILE*
struct FILE_FORMATS_API SFFileCloser {
    int operator ()(SNDFILE*) const;
};
struct SFFile : public std::unique_ptr<SNDFILE, ::SFFileCloser>
{
    SFFile() = default;
    SFFile(SFFile&& that)
        : std::unique_ptr<SNDFILE, ::SFFileCloser>(std::move(that))
    {}

    // Close explicitly, not ignoring return values.
    int close()
    {
        auto result = get_deleter() (get());
        release();
        return result;
    }
};

#endif
