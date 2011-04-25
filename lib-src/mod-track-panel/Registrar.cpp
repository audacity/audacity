/**********************************************************************

  Audacity: A Digital Audio Editor

  Registrar.cpp

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

********************************************************************//**

\class Registrar
\brief Registrar is a class that other classes register resources of
various kinds with.  It makes a system that is much more amenable to 
plugging in of new resources.

*//********************************************************************/

#include <wx/wx.h>
#include "Registrar.h"

