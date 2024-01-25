/**********************************************************************

Audacity: A Digital Audio Editor

SampleBlock.h

**********************************************************************/

#ifndef __AUDACITY_SAMPLE_BLOCK__
#define __AUDACITY_SAMPLE_BLOCK__

#include "GlobalVariable.h"
#include "SampleFormat.h"

#include <functional>
#include <memory>
#include <unordered_set>

#include "Observer.h"
#include "XMLTagHandler.h"

class AudacityProject;
class ProjectFileIO;
class XMLWriter;

using BlockSampleView = std::shared_ptr<std::vector<float>>;
class SampleBlock;
using SampleBlockPtr = std::shared_ptr<SampleBlock>;
class SampleBlockFactory;
using SampleBlockFactoryPtr = std::shared_ptr<SampleBlockFactory>;

using SampleBlockID = long long;

class MinMaxRMS
{
public:
   float min = 0;
   float max = 0;
   float RMS = 0;
};

///\brief Abstract class allows access to contents of a block of sound samples,
/// serialization as XML, and reference count management that can suppress
/// reclamation of its storage
class WAVE_TRACK_API SampleBlock
{
public:
   //! Type of function that is informed when a block is about to be deleted
   struct DeletionCallback : GlobalHook<DeletionCallback,
      void(const SampleBlock&)
   >{};

   virtual ~SampleBlock();

   virtual void CloseLock() noexcept = 0;

   virtual SampleBlockID GetBlockID() const = 0;

   // If !mayThrow and there is an error, ignores it and returns zero.
   // That may be appropriate when only attempting to display samples, not edit.
   size_t GetSamples(samplePtr dest,
                     sampleFormat destformat,
                     size_t sampleoffset,
                     size_t numsamples, bool mayThrow = true);

   virtual BlockSampleView GetFloatSampleView(bool mayThrow) = 0;

   virtual size_t GetSampleCount() const = 0;

   //! Non-throwing, should fill with zeroes on failure
   virtual bool
      GetSummary256(float *dest, size_t frameoffset, size_t numframes) = 0;
   //! Non-throwing, should fill with zeroes on failure
   virtual bool
      GetSummary64k(float *dest, size_t frameoffset, size_t numframes) = 0;

   /// Gets extreme values for the specified region
   // If !mayThrow and there is an error, ignores it and returns zeroes.
   // That may be appropriate when only attempting to display samples, not edit.
   MinMaxRMS GetMinMaxRMS(
      size_t start, size_t len, bool mayThrow = true);

   /// Gets extreme values for the entire block
   // If !mayThrow and there is an error, ignores it and returns zeroes.
   // That may be appropriate when only attempting to display samples, not edit.
   MinMaxRMS GetMinMaxRMS(bool mayThrow = true) const;

   virtual size_t GetSpaceUsage() const = 0;

   virtual void SaveXML(XMLWriter &xmlFile) = 0;

protected:
   virtual size_t DoGetSamples(samplePtr dest,
                     sampleFormat destformat,
                     size_t sampleoffset,
                     size_t numsamples) = 0;

   virtual MinMaxRMS DoGetMinMaxRMS(size_t start, size_t len) = 0;

   virtual MinMaxRMS DoGetMinMaxRMS() const = 0;
};

// Makes a useful function object
inline std::function< void(const SampleBlock&) >
BlockSpaceUsageAccumulator (unsigned long long &total)
{
   return [&total]( const SampleBlock &block ){
      total += block.GetSpaceUsage();
   };
};

struct SampleBlockCreateMessage { };

///\brief abstract base class with methods to produce @ref SampleBlock objects
class WAVE_TRACK_API SampleBlockFactory
   : public Observer::Publisher<SampleBlockCreateMessage>
{
public:
   //! Global factory of per-project factories of sample blocks
   struct WAVE_TRACK_API Factory : GlobalHook<Factory,
      SampleBlockFactoryPtr( AudacityProject& )
   >{};

   // Invoke the installed factory (throw an exception if none was installed)
   static SampleBlockFactoryPtr New( AudacityProject &project );

   virtual ~SampleBlockFactory();

   // Returns a non-null pointer or else throws an exception
   SampleBlockPtr Create(constSamplePtr src,
      size_t numsamples,
      sampleFormat srcformat);

   // Returns a non-null pointer or else throws an exception
   SampleBlockPtr CreateSilent(
      size_t numsamples,
      sampleFormat srcformat);

   // Returns a non-null pointer or else throws an exception
   SampleBlockPtr CreateFromXML(
      sampleFormat srcformat,
      const AttributesList &attrs);

   using SampleBlockIDs = std::unordered_set<SampleBlockID>;
   /*! @return ids of all sample blocks created by this factory and still extant */
   virtual SampleBlockIDs GetActiveBlockIDs() = 0;

protected:
   // The override should throw more informative exceptions on error than the
   // default InconsistencyException thrown by Create
   virtual SampleBlockPtr DoCreate(constSamplePtr src,
      size_t numsamples,
      sampleFormat srcformat) = 0;

   // The override should throw more informative exceptions on error than the
   // default InconsistencyException thrown by CreateSilent
   virtual SampleBlockPtr DoCreateSilent(
      size_t numsamples,
      sampleFormat srcformat) = 0;

   // The override should throw more informative exceptions on error than the
   // default InconsistencyException thrown by CreateFromXML
   virtual SampleBlockPtr DoCreateFromXML(
      sampleFormat srcformat,
      const AttributesList &attrs) = 0;
};

#endif
