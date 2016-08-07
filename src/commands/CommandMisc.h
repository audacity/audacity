/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: GPL v2 - see LICENSE.txt

   Dan Horgan

******************************************************************//**

\file CommandMisc
\brief Some typedefs which are used in various Command-related files

*//*******************************************************************/

#ifndef __COMMANDMISC__
#define __COMMANDMISC__

#include <map>
#include <wx/string.h>
#include <wx/variant.h>
#include "Validators.h"
class CommandType;

// Map from parameter name to the value of the parameter
// to do: use hash
typedef std::map<wxString, wxVariant> ParamValueMap;

// Map from parameter name to a suitable Validator
// to do: use hash
typedef std::map<wxString, movable_ptr<Validator>> ValidatorMap;

// Map from command name to type
// to do: use hash
typedef std::map<wxString, movable_ptr<CommandType>> CommandMap;

#endif /* End of include guard: __COMMANDMISC__ */
