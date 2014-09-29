/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2012 Audacity Team.
   License: GPL v2.  See License.txt.

   ODDecodeFFmpegTask.h
   Michael Chinen

******************************************************************/

#include "../Experimental.h"

#ifdef EXPERIMENTAL_OD_FFMPEG

#ifndef __ODDECODEFFMPEGTASK__
#define __ODDECODEFFMPEGTASK__

#include <vector>
#include "ODDecodeTask.h"
#include "ODTaskThread.h"

class ODFileDecoder;
class WaveTrack;
/// A class representing a modular task to be used with the On-Demand structures.
class ODDecodeFFmpegTask:public ODDecodeTask
{
 public:

   /// Constructs an ODTask
   ODDecodeFFmpegTask(void* scs,int numStreams, WaveTrack*** channels, void* formatContext, int streamIndex);
   virtual ~ODDecodeFFmpegTask();

   virtual ODTask* Clone();
   ///Creates an ODFileDecoder that decodes a file of filetype the subclass handles.
   virtual ODFileDecoder* CreateFileDecoder(const wxString & fileName);

   ///Lets other classes know that this class handles the ffmpeg type
   ///Subclasses should override to return respective type.
   virtual unsigned int GetODType(){return eODFFMPEG;}

 protected:
   WaveTrack*** mChannels;
   int   mNumStreams;
   void* mScs;
   void* mFormatContext;
   int   mStreamIndex;
};
#endif //__ODDECODEFFMPEGTASK__

#endif //EXPERIMENTAL_OD_FFMPEG
