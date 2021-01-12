/**********************************************************************

   Audacity: A Digital Audio Editor

   ImporterInterface.h

   Leland Lucius

   Copyright (c) 2014, Audacity Team 
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

   3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
   COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
   BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE.
   
**********************************************************************/

#ifndef __AUDACITY_IMPORTERINTERFACE_H__
#define __AUDACITY_IMPORTERINTERFACE_H__

#include "audacity/Types.h"
#include "audacity/ConfigInterface.h"
#include "audacity/ComponentInterface.h"

// ============================================================================
//
// ImporterInterface class
//
// ============================================================================

class ImporterHostInterface;
class ImporterClientInterface;
class ImporterInterface : public ComponentInterface
{
public:
   virtual ~ImporterInterface() {};

   // Get unique string ID of this plugin, usually it corresponds
   // to the underlying library, i.e. "libsndfile", "libflac", "libav"
   // These MUST NOT change across Audacity versions (but new IDs can
   // be added).
   virtual wxString GetPluginStringID() = 0;

   // Get a description of the file type this importer can import.
   // Examples: "Ogg Vorbis", "MP3", "Uncompressed PCM"
   virtual TranslatableString GetPluginFormatDescription() = 0;

   // Get a list of extensions this plugin expects to be able to
   // import.  If a filename matches any of these extensions,
   // this importer will get first dibs on importing it.
   virtual FileExtensions GetSupportedExtensions() = 0;
   virtual bool SupportsExtension(const FileExtension & extension) = 0;

   // Create the client that will be used to import a file.
   virtual ImporterClientInterface *CreateClient() = 0;
};


// ============================================================================
//
// ImporterHostInterface class
//
// ============================================================================

class ImporterHostInterface
{
public:
   virtual ~ImporterHostInterface() {};

   // Called by the client to add a new stream to the import.
   virtual bool AddStream(int stream,
                          sampleFormat sampleformat,
                          float sampleRate,
                          int numChannels,
                          ChannelName *channelMap) = 0;

   // Accepts interleaved samples from the client.
   virtual bool PutSamples(int stream, size_t numSamples, samplePtr inBuffer) = 0;

   // Accepts non-interleaved samples from the client.
   virtual bool PutSamples(int stream, int channel, size_t numSamples, samplePtr inBuffer) = 0;
   
   // The client will call this as the import progresses.
   virtual bool UpdateProgress(float current, float total) = 0;
};

// ============================================================================
//
// ImporterClientInterface class
//
// ============================================================================

class ImporterClientInterface
{
public:
   virtual ~ImporterClientInterface() {};

   // Provides a pointer to the associated host for this importer.
   virtual void SetHost(ImporterHostInterface *host) = 0;

   // Open the given file, returning true if it is a recognized
   // format, false otherwise.  This puts the importer into the open
   // state.
   virtual bool Open(const wxString & fileName) = 0;

   // Do any processing necessary to close the file and release resources.
   // This will be called only if Open() succeeded.
   virtual void Close() = 0;

   // This is similar to GetImporterDescription, but if possible the
   // importer will return a more specific description of the
   // specific file that is open.
   virtual TranslatableString GetFileDescription() = 0;

   // Return stream descriptions list
   virtual void GetStreamInfo(wxArrayString & streamInfo) = 0;

   // Set stream "import/don't import" flag
   virtual void SetStreamUsage(int streamID, bool use) = 0;

   // do the actual import, creating whatever tracks are necessary with
   // the WaveTrackFactory and calling the progress callback every iteration
   // through the importing loop
   virtual bool Import() = 0;
};

#endif // __AUDACITY_IMPORTERINTERFACE_H__
