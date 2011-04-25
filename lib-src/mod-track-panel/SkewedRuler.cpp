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
#include "SkewedRuler.h"

START_NAMESPACE


extern int SkewedRulerDispatch( Registrar & R, t_RegistrarDispatchType Type )
{
   switch( Type )
   {
   case RegArtist:
      break;
   case RegDataType:
      break;
   case RegCommand:
      break;
   case RegMenuItem:
      break;
   default:
      break;
   }
   return 1;
}


// For now I've bunged these empty dispatch functions into the same
// file as SkewedRuler.
// When I am ready to work on them I will create new files for them.
int MidiArtistDispatch( Registrar & R, t_RegistrarDispatchType Type )
{
   switch( Type )
   {
   case RegArtist:
      break;
   case RegDataType:
      break;
   case RegCommand:
      break;
   case RegMenuItem:
      break;
   default:
      break;
   }
   return 1;
}

extern int WaveArtistDispatch( Registrar & R, t_RegistrarDispatchType Type )
{
   switch( Type )
   {
   case RegArtist:
      break;
   case RegDataType:
      break;
   case RegCommand:
      break;
   case RegMenuItem:
      break;
   default:
      break;
   }
   return 1;
}

int EnvelopeArtistDispatch( Registrar & R, t_RegistrarDispatchType Type )
{
   switch( Type )
   {
   case RegArtist:
      break;
   case RegDataType:
      break;
   case RegCommand:
      break;
   case RegMenuItem:
      break;
   default:
      break;
   }
   return 1;
}

int LabelArtistDispatch( Registrar & R, t_RegistrarDispatchType Type )
{
   switch( Type )
   {
   case RegArtist:
      break;
   case RegDataType:
      break;
   case RegCommand:
      break;
   case RegMenuItem:
      break;
   default:
      break;
   }
   return 1;
}

int DragGridSizerDispatch( Registrar & R, t_RegistrarDispatchType Type )
{
   switch( Type )
   {
   case RegArtist:
      break;
   case RegDataType:
      break;
   case RegCommand:
      break;
   case RegMenuItem:
      break;
   default:
      break;
   }
   return 1;
}


};//End of Namespace.
