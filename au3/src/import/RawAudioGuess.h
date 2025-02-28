/**********************************************************************

  Audacity: A Digital Audio Editor

  RawAudioGuess.h

  Dominic Mazzoni

  Attempts to determine the format of an audio file that doesn't
  have any header information.  Returns the format as a
  libsndfile-compatible format, along with the guessed number of
  channels and the byte-offset.

**********************************************************************/

#include <wx/defs.h>

#include <sndfile.h>

#ifndef SNDFILE_1
#error Requires libsndfile 1.0.3 or higher
#endif

/* Returns the best guess as to the format, as a libsndfile
   SF_FORMAT value
*/
int RawAudioGuess(const wxString& in_fname, unsigned* out_offset, unsigned* out_channels);
