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
#include "../Internat.h"
class ODPCMAliasBlockFile;
class WaveTrack;

/// A class representing a modular task to be used with the On-Demand structures.
class ODComputeSummaryTask final : public ODTask
{
 public:

   // Constructor / Destructor

   /// Constructs an ODTask
   ODComputeSummaryTask();
   virtual ~ODComputeSummaryTask(){};

   std::unique_ptr<ODTask> Clone() const override;

   ///Subclasses should override to return respective type.
   unsigned int GetODType() override { return eODPCMSummary; }

   ///Return the task name
   const char* GetTaskName() override { return "ODComputeSummaryTask"; }

   TranslatableString GetTip() override
      { return XO("Import complete. Calculating waveform"); }

   bool UsesCustomWorkUntilPercentage() override { return true; }
   float ComputeNextWorkUntilPercentageComplete() override;

   ///releases memory that the ODTask owns.  Subclasses should override.
   void Terminate() override;

protected:
   ///recalculates the percentage complete.
   void CalculatePercentComplete() override;

   ///Computes and writes the data for one BlockFile if it still has a refcount.
   void DoSomeInternal() override;

   ///Readjusts the blockfile order in the default manner.  If we have had an ODRequest
   ///Then it updates in the OD manner.
   void Update() override;

   ///Orders the input as either On-Demand or default layered order.
   void OrderBlockFiles
      (std::vector< std::weak_ptr< ODPCMAliasBlockFile > > &unorderedBlocks);

   ///tells us whether or not Update has been run at least once.
   void MarkUpdateRan();
   bool HasUpdateRan();

   //mBlockFiles is touched on several threads- the OD terminate thread, and the task thread, so we need to mutex it.
   ODLock  mBlockFilesMutex;
   std::vector< std::weak_ptr< ODPCMAliasBlockFile > > mBlockFiles;
   int mMaxBlockFiles;
   ODLock  mHasUpdateRanMutex;
   bool mHasUpdateRan;
};

#endif

