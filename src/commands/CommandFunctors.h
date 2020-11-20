//
//  CommandFunctors.h
//  Audacity
//
//  Created by Paul Licameli on 4/22/16.
//
//

#ifndef __AUDACITY_COMMAND_FUNCTORS__
#define __AUDACITY_COMMAND_FUNCTORS__

class AudacityProject;
class AudacityApp;
class CommandContext;
class wxEvtHandler;

// Base class for objects, to whose member functions, the CommandManager will
// dispatch.
//
// It, or a subclass of it, must be the first base class of the object, and the
// first base class of that base class, etc., for the same reason that
// wxEvtHandler must be first (that is, the downcast from a pointer to the base
// to a pointer to the object, must be a vacuous operation).
//
// In fact, then, we just make it an alias of wxEvtHandler, in case you really
// need to inherit from wxEvtHandler for other reasons, and otherwise you
// couldn't satisfy the requirement for both base classes at once.
using CommandHandlerObject = wxEvtHandler;

// First of two functions registered with each command: an extractor
// of the handler object from the AudacityProject
using CommandHandlerFinder =
   std::function< CommandHandlerObject&(AudacityProject &) >;

// Second of two function pointers registered with each command: a pointer
// to a member function of the handler object
using CommandFunctorPointer =
   void (CommandHandlerObject::*)(const CommandContext &context );

#endif
