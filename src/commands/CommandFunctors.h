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
#include "../MemoryX.h"

class wxEvent;
typedef wxString PluginID;

class AUDACITY_DLL_API CommandFunctor /* not final */
{
public:
   CommandFunctor(){};
   virtual ~CommandFunctor(){};
   virtual void operator()(int index, const wxEvent *e) = 0;
};

using CommandFunctorPointer = std::shared_ptr <CommandFunctor>;


// Define functor subclasses that dispatch to the correct call sequence on
// member functions of AudacityProject (or other class!)

template<typename THIS>
using audCommandFunction = void (THIS::*)();

template<typename THIS>
class VoidFunctor final : public CommandFunctor
{
public:
   explicit VoidFunctor(THIS *This, audCommandFunction<THIS> pfn)
   : mThis{ This }, mCommandFunction{ pfn } {}
   void operator () (int, const wxEvent *) override
   { (mThis->*mCommandFunction) (); }
private:
   THIS *const mThis;
   const audCommandFunction<THIS> mCommandFunction;
};

template<typename THIS>
using audCommandKeyFunction = void (THIS::*)(const wxEvent *);

template<typename THIS>
class KeyFunctor final : public CommandFunctor
{
public:
   explicit KeyFunctor(THIS *This, audCommandKeyFunction<THIS> pfn)
   : mThis{ This }, mCommandKeyFunction{ pfn } {}
   void operator () (int, const wxEvent *evt) override
   { (mThis->*mCommandKeyFunction) (evt); }
private:
   THIS *const mThis;
   const audCommandKeyFunction<THIS> mCommandKeyFunction;
};

template<typename THIS>
using audCommandListFunction = void (THIS::*)(int);

template<typename THIS>
class ListFunctor final : public CommandFunctor
{
public:
   explicit ListFunctor(THIS *This, audCommandListFunction<THIS> pfn)
   : mThis{ This }, mCommandListFunction{ pfn } {}
   void operator () (int index, const wxEvent *) override
   { (mThis->*mCommandListFunction)(index); }
private:
   THIS *const mThis;
   const audCommandListFunction<THIS> mCommandListFunction;
};

template<typename THIS>
using audCommandPluginFunction = bool (THIS::*)(const PluginID &, int);

template<typename THIS>
class PluginFunctor final : public CommandFunctor
{
public:
   explicit PluginFunctor(THIS *This, const PluginID &id, audCommandPluginFunction<THIS> pfn)
   : mPluginID{ id }, mThis{ This }, mCommandPluginFunction{ pfn } {}
   void operator () (int, const wxEvent *) override
   { (mThis->*mCommandPluginFunction)
      (mPluginID,
       0 // AudacityProject::OnEffectFlags::kNone
      ); }
private:
   const PluginID mPluginID;
   THIS *const mThis;
   const audCommandPluginFunction<THIS> mCommandPluginFunction;
};

// Now define an overloaded factory function
template<typename THIS>
inline CommandFunctorPointer MakeFunctor(THIS *This,
                                         audCommandFunction<THIS> pfn)
{ return CommandFunctorPointer{ safenew VoidFunctor<THIS>{ This, pfn } }; }

template<typename THIS>
inline CommandFunctorPointer MakeFunctor(THIS *This,
                                         audCommandKeyFunction<THIS> pfn)
{ return CommandFunctorPointer{ safenew KeyFunctor<THIS>{ This, pfn } }; }

template<typename THIS>
inline CommandFunctorPointer MakeFunctor(THIS *This,
                                         audCommandListFunction<THIS> pfn)
{ return CommandFunctorPointer{ safenew ListFunctor<THIS>{ This, pfn } }; }

template<typename THIS>
inline CommandFunctorPointer MakeFunctor(THIS *This, const PluginID &id,
                                         audCommandPluginFunction<THIS> pfn)
{ return CommandFunctorPointer{ safenew PluginFunctor<THIS>{ This, id, pfn } }; }

// Now define the macro abbreviations that call the factory
#define FNT(THIS, This, X) (MakeFunctor<THIS>(This, X ))
#define FNTS(THIS, This, X, S) (MakeFunctor<THIS>(This, (S), X ))

#define FN(X) FNT(AudacityProject, this, & AudacityProject :: X)
#define FNS(X, S) FNTS(AudacityProject, this, & AudacityProject :: X, S)

#endif
