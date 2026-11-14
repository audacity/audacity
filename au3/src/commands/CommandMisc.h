/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: GPL v2 or later - see LICENSE.txt

   Dan Horgan

******************************************************************//**

\file CommandMisc
\brief Some typedefs which are used in various Command-related files

*//*******************************************************************/

#ifndef __COMMANDMISC__
#define __COMMANDMISC__

#include <map>
#include "Validators.h"

class wxVariant;
class OldStyleCommandType;

// Map from parameter name to the value of the parameter
// to do: use hash
typedef std::map<wxString, wxVariant> ParamValueMap;
typedef std::map<wxString, bool> ParamBoolMap;

// Map from parameter name to a suitable Validator
// to do: use hash
typedef std::map<wxString, std::unique_ptr<Validator> > ValidatorMap;

// Map from command name to type
// to do: use hash
typedef std::map<wxString, std::unique_ptr<OldStyleCommandType> > CommandMap;

#endif /* End of include guard: __COMMANDMISC__ */
