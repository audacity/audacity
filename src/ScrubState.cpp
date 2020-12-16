/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file ScrubState.cpp
 
 Paul Licameli split from AudioIO.cpp
 
 **********************************************************************/

#include "ScrubState.h"

ScrubbingPlaybackPolicy::ScrubbingPlaybackPolicy(
   const ScrubbingOptions &options)
   : mOptions{ options }
{}

ScrubbingPlaybackPolicy::~ScrubbingPlaybackPolicy() = default;
