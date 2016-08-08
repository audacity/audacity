/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2012 Audacity Team.
   License: GPL v2.  See License.txt.

   ODDecodeFFmpegTask.h
   Michael Chinen

******************************************************************/

#include "../Experimental.h"
#include "../MemoryX.h"

#ifdef EXPERIMENTAL_OD_FFMPEG

#ifndef __ODDECODEFFMPEGTASK__
#define __ODDECODEFFMPEGTASK__

#include <vector>
#include "ODDecodeTask.h"
#include "ODTaskThread.h"

struct FFmpegContext;
class ODFileDecoder;
class WaveTrack;
/// A class representing a modular task to be used with the On-Demand structures.
class ODDecodeFFmpegTask final : public ODDecodeTask
{
public:
   using Channels = std::vector < WaveTrack* >;
   using Streams = std::vector < Channels >;

   static Streams FromList(const std::list<TrackHolders> &channels);

   /// Constructs an ODTask
   ODDecodeFFmpegTask(const ScsPtr &scs, Streams &&channels, const std::shared_ptr<FFmpegContext> &context, int streamIndex);
   virtual ~ODDecodeFFmpegTask();

   movable_ptr<ODTask> Clone() const override;
   ///Creates an ODFileDecoder that decodes a file of filetype the subclass handles.
   ODFileDecoder* CreateFileDecoder(const wxString & fileName) override;

   ///Lets other classes know that this class handles the ffmpeg type
   ///Subclasses should override to return respective type.
   unsigned int GetODType() override {return eODFFMPEG;}

protected:
   // non-owning pointers to WaveTracks:
   Streams mChannels;

   ScsPtr mScs;
   std::shared_ptr<FFmpegContext> mContext;
   int   mStreamIndex;
};
#endif //__ODDECODEFFMPEGTASK__

#endif //EXPERIMENTAL_OD_FFMPEG
