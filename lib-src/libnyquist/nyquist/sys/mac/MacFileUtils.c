// Routines that deal with some mac file system stuff -EAD

#include <Files.h>
#include <TextUtils.h>
#include <string.h>
//#include "MiscellaneousUtilities.h"

//=========================================================================
// Function prototypes
//=========================================================================

void set_mac_file_type(char *filename);
void GetFullPath(FSSpec *theSpec, StringPtr theName);
void PathNameFromDirID(long dirID, short vRefNum, StringPtr fullPathName);



//=========================================================================
// Set the output soundfile type and creator
//=========================================================================

void set_mac_file_type(char *filename)
{
    Str255	fName;
    FSSpec	fSpec;
    FInfo	fFInfo;
    
    fFInfo.fdType = 'AIFF';
    fFInfo.fdCreator = 'Sd2a';

    BlockMoveData(filename, &fName[1], 256);
    fName[0] = strlen(filename);
    FSMakeFSSpec(0, 0, fName, &fSpec);
    FSpSetFInfo(&fSpec, &fFInfo);
}

//==================================================================================================================================
// void GetFullPath(FSSpec *theSpec, StringPtr theName)
//==================================================================================================================================
// Extracts the full pathname for the file pointed to by theSpec and returns it in theName.
//==================================================================================================================================

void GetFullPath(FSSpec *theSpec, StringPtr theName)
{
    *theName = 0;
    if (theSpec->parID != 1) PathNameFromDirID(theSpec->parID, theSpec->vRefNum, theName);
    // was: pstrcat(theName, theSpec->name);
    strcat(P2CStr(theName), P2CStr(theSpec->name));
    C2PStr((char *) theName);
    C2PStr((char *) theSpec->name);
    //pstrcat(theName, "\p:");
    theName[*theName + 1] = 0;
}

//==================================================================================================================================
// void PathNameFromDirID(long dirID, short vRefNum, StringPtr fullPathName)
//==================================================================================================================================
// Given a vRefNum and a directory ID, creates a full path specification.
//==================================================================================================================================

void PathNameFromDirID(long dirID, short vRefNum, StringPtr fullPathName)
{
    Str255 directoryName;
    DirInfo block;
    OSErr err;
    fullPathName[0] = 0;
    block.ioDrDirID = block.ioDrParID = dirID;
    block.ioNamePtr = directoryName;
    do {
        block.ioVRefNum = vRefNum;
        block.ioFDirIndex = -1;
        block.ioDrDirID = block.ioDrParID;
        err = PBGetCatInfo((CInfoPBPtr)&block, false);
        //pstrcat(directoryName, (StringPtr)"\p:");
        //pstrinsert(fullPathName, directoryName);
        strcat(P2CStr(directoryName), ":");
        strcat((char *) directoryName, (char *) fullPathName);
        strcpy((char *)fullPathName, (char *) directoryName);
    } while (block.ioDrDirID != 2);
    C2PStr((char *) fullPathName);
}
