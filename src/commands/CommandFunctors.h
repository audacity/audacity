//
//  CommandFunctors.h
//  Audacity
//
//  Created by Paul Licameli on 4/22/16.
//
//

#ifndef __AUDACITY_COMMAND_FUNCTORS__
#define __AUDACITY_COMMAND_FUNCTORS__

#include <wx/string.h>
#include <wx/event.h>
#include "../MemoryX.h"

class AudacityProject;
class wxEvent;

struct CommandContext {
   CommandContext(
      AudacityProject &p
      , const wxEvent *e = nullptr
      , int ii = 0
   )
      : project{ p }
      , pEvt{ e }
      , index{ ii }
   {}

   AudacityProject &project;
   const wxEvent *pEvt;
   int index;
};

class wxEvent;
typedef wxString PluginID;

class AUDACITY_DLL_API CommandFunctor /* not final */
{
public:
   CommandFunctor(){};
   virtual ~CommandFunctor(){};
   virtual void operator()(const CommandContext &context) = 0;
};

using CommandFunctorPointer = std::shared_ptr <CommandFunctor>;


// Define functor subclasses that dispatch to the correct call sequence on
// member functions of AudacityProject (or other class!)

template<typename OBJ>
using audCommandFunction = void (OBJ::*)();

template<typename OBJ>
class VoidFunctor final : public CommandFunctor
{
public:
   explicit VoidFunctor(OBJ *This, audCommandFunction<OBJ> pfn)
   : mThis{ This }, mCommandFunction{ pfn } {}
   void operator () (const CommandContext &context) override
   { (mThis->*mCommandFunction) (); }
private:
   OBJ *const mThis;
   const audCommandFunction<OBJ> mCommandFunction;
};

template<typename OBJ>
using audCommandKeyFunction = void (OBJ::*)(const wxEvent *);

template<typename OBJ>
class KeyFunctor final : public CommandFunctor
{
public:
   explicit KeyFunctor(OBJ *This, audCommandKeyFunction<OBJ> pfn)
   : mThis{ This }, mCommandKeyFunction{ pfn } {}
   void operator () (const CommandContext &context) override
   { (mThis->*mCommandKeyFunction) (context.pEvt); }
private:
   OBJ *const mThis;
   const audCommandKeyFunction<OBJ> mCommandKeyFunction;
};

// This allows functions to be used either by command manager or by a wxMenu popup,
// but the functions MUST ignore the argument!
template<typename OBJ>
using audCommandPopupFunction = void (OBJ::*)(wxCommandEvent&);

template<typename OBJ>
class PopupFunctor final : public CommandFunctor
{
public:
   explicit PopupFunctor(OBJ *This, audCommandPopupFunction<OBJ> pfn)
   : mThis{ This }, mCommandPopupFunction{ pfn } {}
   void operator () (const CommandContext &context) override
   { wxCommandEvent dummy; (mThis->*mCommandPopupFunction) (dummy); }
private:
   OBJ *const mThis;
   const audCommandPopupFunction<OBJ> mCommandPopupFunction;
};

template<typename OBJ>
using audCommandListFunction = void (OBJ::*)(int);

template<typename OBJ>
class ListFunctor final : public CommandFunctor
{
public:
   explicit ListFunctor(OBJ *This, audCommandListFunction<OBJ> pfn)
   : mThis{ This }, mCommandListFunction{ pfn } {}
   void operator () (const CommandContext &context) override
   { (mThis->*mCommandListFunction)(context.index); }
private:
   OBJ *const mThis;
   const audCommandListFunction<OBJ> mCommandListFunction;
};

template<typename OBJ>
using audCommandPluginFunction = bool (OBJ::*)(const PluginID &, int);

template<typename OBJ>
class PluginFunctor final : public CommandFunctor
{
public:
   explicit PluginFunctor(OBJ *This, const PluginID &id, audCommandPluginFunction<OBJ> pfn)
   : mPluginID{ id }, mThis{ This }, mCommandPluginFunction{ pfn } {}
   void operator () (const CommandContext &context) override
   { (mThis->*mCommandPluginFunction)
      (mPluginID,
       0 // AudacityProject::OnEffectFlags::kNone
      ); }
private:
   const PluginID mPluginID;
   OBJ *const mThis;
   const audCommandPluginFunction<OBJ> mCommandPluginFunction;
};

// Now define an overloaded factory function
template<typename OBJ>
inline CommandFunctorPointer MakeFunctor(OBJ *This,
                                         audCommandFunction<OBJ> pfn)
{ return CommandFunctorPointer{ safenew VoidFunctor<OBJ>{ This, pfn } }; }

template<typename OBJ>
inline CommandFunctorPointer MakeFunctor(OBJ *This,
                                         audCommandKeyFunction<OBJ> pfn)
{ return CommandFunctorPointer{ safenew KeyFunctor<OBJ>{ This, pfn } }; }

template<typename OBJ>
inline CommandFunctorPointer MakeFunctor(OBJ *This,
                                         audCommandPopupFunction<OBJ> pfn)
{ return CommandFunctorPointer{ safenew PopupFunctor<OBJ>{ This, pfn } }; }

template<typename OBJ>
inline CommandFunctorPointer MakeFunctor(OBJ *This,
                                         audCommandListFunction<OBJ> pfn)
{ return CommandFunctorPointer{ safenew ListFunctor<OBJ>{ This, pfn } }; }

template<typename OBJ>
inline CommandFunctorPointer MakeFunctor(OBJ *This, const PluginID &id,
                                         audCommandPluginFunction<OBJ> pfn)
{ return CommandFunctorPointer{ safenew PluginFunctor<OBJ>{ This, id, pfn } }; }

// Now define the macro abbreviations that call the factory
#define FNT(OBJ, This, X) (MakeFunctor<OBJ>(This, X ))
#define FNTS(OBJ, This, X, S) (MakeFunctor<OBJ>(This, (S), X ))

#define FN(X) FNT(AudacityProject, this, & AudacityProject :: X)
#define FNS(X, S) FNTS(AudacityProject, this, & AudacityProject :: X, S)

#endif
