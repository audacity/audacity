/**********************************************************************

  Audacity: A Digital Audio Editor

  AVUtilFunctionsLoader.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

class wxDynamicLibrary;
struct AVUtilFunctions;

bool LoadAVUtilFunctions(
    const wxDynamicLibrary& lib, AVUtilFunctions& functions);
