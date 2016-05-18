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

Registrar * pRegistrar = NULL;

// By defining the external function and including it here, we save ourselves maintaing two lists.
// Also we save ourselves recompiling Registrar each time the classes that regiser change.
// Part of the idea is that the Registrar knows very little about the classes that
// register with it.
#define DISPATCH( Name ) extern int Name##Dispatch( Registrar & R, t_RegistrarDispatchType Type );\
   Name##Dispatch( *pRegistrar, Type )

// Not a class function, otherwise the functions called 
// are treated as belonging to the class.
int RegistrarDispatch( t_RegistrarDispatchType Type )
{
   wxASSERT( pRegistrar != NULL );

   DISPATCH( SkewedRuler );
   DISPATCH( MidiArtist );
   DISPATCH( WaveArtist );
   DISPATCH( EnvelopeArtist );
   DISPATCH( LabelArtist );
   DISPATCH( DragGridSizer );
   DISPATCH( TrackPanel2 );
   return 0;
}

// Start()
// Static function.  Allocates Registrar.
void Registrar::Start()
{
   wxASSERT( pRegistrar ==NULL );
   pRegistrar = new Registrar();

   RegistrarDispatch( RegResource );
   RegistrarDispatch( RegArtist );
   RegistrarDispatch( RegDataType );
   RegistrarDispatch( RegCommand );
   RegistrarDispatch( RegMenuItem );
}

// Finish()
// Static function.  DeAllocates Registrar.
void Registrar::Finish()
{
   wxASSERT( pRegistrar !=NULL );
   delete pRegistrar;
   pRegistrar = NULL;
}

void Registrar::ShowNewPanel()
{
   wxASSERT( pRegistrar !=NULL );
   if( pRegistrar->pShowFn != NULL)
      pRegistrar->pShowFn();
}