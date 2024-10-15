/* Handle required apple events -EAD */

#include <Files.h>
#include <string.h>
#include <AppleEvents.h>
#include "macstuff.h"
#include "MacCommandWin.h"
#include "MacFileUtils.h"
//#include "MiscellaneousUtilities.h"

#define TEXTREC		(*hTERec)	// the command 
extern TEHandle hTERec;			// window text record 


//=========================================================================
// Handle quit apple event
//=========================================================================

pascal OSErr AEQuit (AppleEvent *theAppleEvent, AppleEvent *theReply, long Refcon)
{
    osfinish();
}

//=========================================================================
// Handle Open Document apple event by trying to load it.
//=========================================================================
extern xlload (char *, int, int);
extern xlabort(char *);

pascal OSErr AEOpenFiles(AppleEvent *theAppleEvent, AppleEvent *theReply,
                         long Refcon)
{
    AEDescList docList;
    AEKeyword keywd;
    DescType returnedType;
    Size actualSize;
    long itemsInList;
    FSSpec theSpec;
    CInfoPBRec pb;
    Str255 name;
    short i;
    
    if (AEGetParamDesc(theAppleEvent, keyDirectObject, typeAEList, &docList) !=
        noErr) return;
    if (AECountItems (&docList, &itemsInList) != noErr) return;
    
    SetSelection (TEXTREC->teLength, TEXTREC->teLength);
    for (i = 1; i <= itemsInList; i++) {
        AEGetNthPtr (&docList, i, typeFSS, &keywd, &returnedType, 
            (Ptr) &theSpec, sizeof(theSpec), &actualSize);
        
        GetFullPath(&theSpec, name);
        P2CStr(name); // was: pstrterm(name);
        if (xlload ((char *)name + 1, 1, 0) == 0) xlabort ("load error");
    }
    macputs ("> ");
    PrepareForInput ();
}
