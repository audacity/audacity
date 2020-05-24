/**********************************************************************

  Audacity: A Digital Audio Editor

  Registrar.h

  James Crook

  Manages centralised registration of resources.

**********************************************************************/

#ifndef __AUDACITY_REGISTRAR__
#define __AUDACITY_REGISTRAR__

typedef enum
{
   RegResource,
   RegArtist,
   RegDataType,
   RegCommand,
   RegMenuItem, 
   RegLast
} t_RegistrarDispatchType;

class Registrar {
   Registrar::Registrar(){
      pShowFn = NULL;}
public:
   // Fairly generic registrar functions.
   static void Start();
   static void Finish();

   // Somewhat specific to this application registrar functions.
   // These mostly reflect one-offs, where a more sophisticated 
   // system would manage a list.
   static void ShowNewPanel();
public:
   void (*pShowFn)(void);
};


extern int RegistrarDispatch( t_RegistrarDispatchType Type );

#endif
