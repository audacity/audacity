/**********************************************************************

  Audacity: A Digital Audio Editor

  AVFormatFunctionsLoader.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

class wxDynamicLibrary;
struct AVFormatFunctions;

bool LoadAVFormatFunctions(
   const wxDynamicLibrary& lib, AVFormatFunctions& functions);
