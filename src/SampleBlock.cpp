/**********************************************************************

Audacity: A Digital Audio Editor

SampleBlock.cpp

**********************************************************************/

#include "Audacity.h"
#include "SampleBlock.h"

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

SampleBlock::~SampleBlock() = default;

