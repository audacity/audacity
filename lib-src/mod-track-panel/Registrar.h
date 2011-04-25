/**********************************************************************

  Audacity: A Digital Audio Editor

  Registrar.h

  James Crook

  Manages centralised registration of resources.

**********************************************************************/

#ifndef __AUDACITY_REGISTRAR__
#define __AUDACITY_REGISTRAR__

// MSVC auto-indents, but I don't want that, for the namespace.
// so using a macro for that works around ir
// AND allows me to change the namespace name easily.

#define START_NAMESPACE namespace ModTrackPanel {

START_NAMESPACE 

typedef enum
{
   RegArtist,
   RegDataType,
   RegCommand,
   RegMenuItem, 
   RegLast
} t_RegistrarDispatchType;

class Registrar {
public:
   static void Start();
   static void Finish();

};


extern int RegistrarDispatch( t_RegistrarDispatchType Type );

};//End of Namespace.
#endif
