/**********************************************************************

Audacity: A Digital Audio Editor

SampleBlock.cpp

**********************************************************************/

#include "Audacity.h"
#include "InconsistencyException.h"
#include "SampleBlock.h"
#include "SampleFormat.h"

#include <wx/defs.h>

static SampleBlockFactoryFactory& installedFactory()
{
   static SampleBlockFactoryFactory theFactory;
   return theFactory;
}

SampleBlockFactoryFactory SampleBlockFactory::RegisterFactoryFactory(
   SampleBlockFactoryFactory newFactory )
{
   auto &theFactory = installedFactory();
   auto result = std::move( theFactory );
   theFactory = std::move( newFactory );
   return result;
}

SampleBlockFactoryPtr SampleBlockFactory::New( AudacityProject &project )
{
   auto &factory = installedFactory();
   if ( ! factory )
      THROW_INCONSISTENCY_EXCEPTION;
   return factory( project );
}

SampleBlockFactory::~SampleBlockFactory() = default;

SampleBlockPtr SampleBlockFactory::Create(constSamplePtr src,
   size_t numsamples,
   sampleFormat srcformat)
{
   auto result = DoCreate(src, numsamples, srcformat);
   if (!result)
      THROW_INCONSISTENCY_EXCEPTION;
   return result;
}

SampleBlockPtr SampleBlockFactory::CreateSilent(
   size_t numsamples,
   sampleFormat srcformat)
{
   auto result = DoCreateSilent(numsamples, srcformat);
   if (!result)
      THROW_INCONSISTENCY_EXCEPTION;
   return result;
}

SampleBlockPtr SampleBlockFactory::CreateFromXML(
   sampleFormat srcformat,
   const wxChar **attrs)
{
   auto result = DoCreateFromXML(srcformat, attrs);
   if (!result)
      THROW_INCONSISTENCY_EXCEPTION;
   return result;
}

SampleBlock::~SampleBlock() = default;

size_t SampleBlock::GetSamples(samplePtr dest,
                   sampleFormat destformat,
                   size_t sampleoffset,
                   size_t numsamples, bool mayThrow)
{
   try{ return DoGetSamples(dest, destformat, sampleoffset, numsamples); }
   catch( ... ) {
      if( mayThrow )
         throw;
      ClearSamples( dest, destformat, 0, numsamples );
      return 0;
   }
}
 
 MinMaxRMS SampleBlock::GetMinMaxRMS(
                        size_t start, size_t len, bool mayThrow)
{
   try{ return DoGetMinMaxRMS(start, len); }
   catch( ... ) {
      if( mayThrow )
         throw;
      return {};
   }
}
 
 MinMaxRMS SampleBlock::GetMinMaxRMS(bool mayThrow) const
{
   try{ return DoGetMinMaxRMS(); }
   catch( ... ) {
      if( mayThrow )
         throw;
      return {};
   }
}

