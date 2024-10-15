/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file CFResources.cpp
 
 Paul Licameli
 
 **********************************************************************/
#include "CFResources.h"

#ifdef __APPLE__

//! Explicit instantiation for default void*; checks completeness of the header
template struct CFReleaser<>;

#endif
