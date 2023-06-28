#pragma once

#include <memory>

class NUMERIC_FORMATS_API ProjectTempoListenerInterface
{
public:
   virtual ~ProjectTempoListenerInterface();
   virtual void SetProjectTempo(double newTempo) = 0;
};
