/**********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioGraphSource.cpp

  Paul Licameli

**********************************************************************/
#include "AudioGraphSource.h"

AudioGraph::Source::~Source() = default;

bool AudioGraph::Source::Terminates() const
{
    return true;
}
