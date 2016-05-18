/**********************************************************************

  Audacity: A Digital Audio Editor

  Registrar.cpp

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

********************************************************************//**

\class SkewedRuller
\brief SkewedRuler draws a ruler for aligning two sequences.

*//********************************************************************/

#include <wx/wx.h>
#include "Registrar.h"
#include "SkewedRuler.h"

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



