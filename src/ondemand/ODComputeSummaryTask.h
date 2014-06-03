/**********************************************************************

  Audacity: A Digital Audio Editor

  ODComputeSummaryTask.h

  Created by Michael Chinen (mchinen) on 6/8/08.
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODComputeSummaryTask
\brief Computes the summary data for a PCM (WAV) file and writes it to disk,
updating the ODPCMAliasBlockFile and the GUI of the newly available data.

*//*******************************************************************/

#ifndef __AUDACITY_ODComputeSummaryTask__
#define __AUDACITY_ODComputeSummaryTask__

#include <vector>
#include "ODTask.h"
#include "ODTaskThread.h"
class ODPCMAliasBlockFile;
class WaveTrack;

/// A class representing a modular task to be used with the On-Demand structures.
class ODComputeSummaryTask:public ODTask
{
 public:

   // Constructor / Destructor

   /// Constructs an ODTask
   ODComputeSummaryTask();
   virtual ~ODComputeSummaryTask(){};

   virtual ODTask* Clone();

   ///Subclasses should override to return respective type.
   virtual unsigned int GetODType(){return eODPCMSummary;}

   ///Return the task name
   virtual const char* GetTaskName(){return "ODComputeSummaryTask";}

   virtual const wxChar* GetTip(){return _("Import complete. Calculating waveform");}

   virtual bool UsesCustomWorkUntilPercentage(){return true;}
   virtual float ComputeNextWorkUntilPercentageComplete();

   ///releases memory that the ODTask owns.  Subclasses should override.
   virtual void Terminate();

protected:
   ///recalculates the percentage complete.
   virtual void CalculatePercentComplete();

   ///Computes and writes the data for one BlockFile if it still has a refcount.
   virtual void DoSomeInternal();

   ///Readjusts the blockfile order in the default manner.  If we have had an ODRequest
   ///Then it updates in the OD manner.
   virtual void Update();

   ///Orders the input as either On-Demand or default layered order.
   void OrderBlockFiles(std::vector<ODPCMAliasBlockFile*> &unorderedBlocks);

   ///tells us whether or not Update has been run at least once.
   void MarkUpdateRan();
   bool HasUpdateRan();

   //mBlockFiles is touched on several threads- the OD terminate thread, and the task thread, so we need to mutex it.
   ODLock  mBlockFilesMutex;
   std::vector<ODPCMAliasBlockFile*> mBlockFiles;
   int mMaxBlockFiles;
   int mComputedBlockFiles;
   ODLock  mHasUpdateRanMutex;
   bool mHasUpdateRan;
};

#endif

