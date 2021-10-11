/**********************************************************************

  Audacity: A Digital Audio Editor

  AVCodecFunctionsLoader.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

class wxDynamicLibrary;
struct AVCodecFunctions;

bool LoadAVCodecFunctions(
   const wxDynamicLibrary& lib, AVCodecFunctions& functions);
