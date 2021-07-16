/**********************************************************************

   Audacity: A Digital Audio Editor

   ComponentInterface.h

   Leland Lucius

   Copyright (c) 2014, Audacity Team 
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

   3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
   COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
   BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE.
   
**********************************************************************/

#ifndef __AUDACITY_COMPONENT_INTERFACE_H__
#define __AUDACITY_COMPONENT_INTERFACE_H__

#include "Identifier.h"
#include "Internat.h"
#include <wx/string.h> // member variables

class ComponentInterfaceSymbol;
using VendorSymbol = ComponentInterfaceSymbol;

class ShuttleParams;

/**************************************************************************//**

\brief ComponentInterface provides name / vendor / version functions to identify
plugins.  It is what makes a class a plug-in.  Additionally it provides an
optional parameter definitions function, for those components such as commands,
effects and (soon) preference pagess that define parameters.
********************************************************************************/
class COMPONENTS_API ComponentInterface /* not final */
{
public:
   virtual ~ComponentInterface();

   // These should return an untranslated value
   virtual PluginPath GetPath() = 0;

   // The internal string persists in configuration files
   // So config compatibility will break if it is changed across Audacity versions
   virtual ComponentInterfaceSymbol GetSymbol() = 0;

   virtual VendorSymbol GetVendor() = 0;

   virtual wxString GetVersion() = 0;

   // This returns a translated string
   // Any verb should be present tense indicative, not imperative
   virtual TranslatableString GetDescription() = 0;

   // non-virtual convenience function
   TranslatableString GetName();

   // Parameters, if defined.  false means no defined parameters.
   virtual bool DefineParams( ShuttleParams & WXUNUSED(S) ){ return false;};   
};

#endif // __AUDACITY_IDENTINTERFACE_H__
