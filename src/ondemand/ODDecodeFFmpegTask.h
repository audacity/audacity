/*
 *  ODDecodeFFmpegTask.h
 *  Audacity
 *
 *  Created by apple on 3/8/10.
 *  Copyright 2010 __MyCompanyName__. All rights reserved.
 *
 */
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
   ODDecodeFFmpegTask(void* scs,int numStreams, WaveTrack*** channels, void* formatContext);
   virtual ~ODDecodeFFmpegTask();

   virtual ODTask* Clone();
   ///Creates an ODFileDecoder that decodes a file of filetype the subclass handles.
   virtual ODFileDecoder* CreateFileDecoder(const wxString & fileName);
   
   ///Lets other classes know that this class handles the ffmpeg type   
   ///Subclasses should override to return respective type.
   virtual unsigned int GetODType(){return eODFFMPEG;}

   /// overridden because we cannot always seek - this depends on the file and our confidence which is
   /// computed by this function.
    virtual bool SeekingAllowed();

 protected:
   WaveTrack*** mChannels;
   int   mNumStreams;
   void* mScs;
   void* mFormatContext;
};
#endif //__ODDECODEFFMPEGTASK__

#endif //EXPERIMENTAL_OD_FFMPEG