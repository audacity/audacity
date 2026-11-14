/**********************************************************************

  Audacity: A Digital Audio Editor

  DynamicLibraryHelpers.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

class wxDynamicLibrary;
struct FFMPegVersion;

//! Does not require the named function to be found in the library
#define GET_SYMBOL(name) (lib.HasSymbol(#name) ? functions.name \
                              = reinterpret_cast<decltype(functions.name)>(lib.GetSymbol(#name)) : functions.name = nullptr)
//! Requires the named function to be found in the library, else load fails
#define RESOLVE(name) if (nullptr == GET_SYMBOL(name)) return false

bool GetAVVersion(
    const wxDynamicLibrary& lib, const char* name, FFMPegVersion& version);
