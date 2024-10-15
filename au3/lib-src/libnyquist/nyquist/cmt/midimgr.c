/* midimgr.c -- this file contains interface code to support use of Apple Midi Manager */
/*
 * This code is based on code supplied with the Apple Midi Manager.
 * Copyright 1991, Carnegie Mellon University
 */
 
/* BUGS: 
 *     If exclusive() is called to turn exclusive messages on or off DURING the
 * receipt of an exclusive message, incoming data will be garbled.  The correct
 * handling would be to record when receipt of an exclusive message is in
 * progress, then properly remove any partial message when exclusive is turned
 * off, and ignore any remaining message part when exclusive is turned on.
 * The present code does neither.
 */
 
#include "cext.h"
#undef round
#ifdef THINK_C
#include <pascal.h> /* for ThinkC 7 */
#endif

#include "stdio.h"
#include "userio.h"
#include "MIDI.h"
#include "midifns.h"
#include "midibuff.h"
#include "midierr.h"
#include "midimgr.h"
#include "midicode.h"
#include "cmdline.h"

/* Needed for KillEverybody */
#include <toolutils.h>
#include <AppleEvents.h>
#include <EPPC.h>
#include <Gestalt.h>
#include <PPCToolbox.h> 
#include <Processes.h>
#include <Sound.h>


#define CMTclientID             'CMT '
/* note the following are in alphabetical order for Patcher display */
#define timePortID              'Atim'
#define inputPortID             'Bin '
#define outputPortID    'Cout'
#define noClient                '    '

#define noTimeBaseRefNum        0
#define noReadHook                      0L
#define zeroTime                        0L
#define timePortBuffSize        0L
#define inputPortBuffSize       2048
#define outputPortBuffSize      0L
#define refCon0 0L

pascal short CMTreader(MIDIPacket *ThePacketPtr, long TheRefCon);

/* "patch" switch from command line.  This switch is cached in patch_flag and tells
   whether to look in the resource fork for a patch, or just hook up to midi in and
   out.  If the resource fork is used, the patch will be saved upon exit. */
private boolean patch_flag; 
extern boolean ctrlFilter;
extern boolean exclFilter;
extern boolean realFilter;

private midi_read_lock = false;	/* used to stop input during data structure manipulation */

private void set_error(int bit);
#ifndef NYQUIST
void PatchPorts(void);
void SavePatch(OSType PortID, short PortInfoResID, char *PortInfoResName);
#endif

/* exported:  */
public short InputRefNum;                       /* Input port reference number. */
public short OutputRefNum;                      /* Output port reference number. */
public short TimeRefNum;                        /* Time base port reference number. */

Boolean         GManualPatch;   /* True if not launched by a PatchBay Config. File. */

/****************************************************************************
*
*       variables shared with other modules
*
****************************************************************************/

/* midi input buffer */
long buff[BUFF_SIZE/4]; /* data buffer, declared long to get 32-bit alignment */
int buffhead = 0;               /* buffer head and tail pointers */
int bufftail = 0;

/* user supplied system exclusive buffer */
byte *xbuff = NULL;     /* address of the user-supplied buffer */
public long xbufmask;    /* mask for circular buffer address calculation */
long xbufhead = 0;       /* buffer head and tail pointers */
long xbuftail = 0;
boolean xbuf_flush = true;	/* says to flush remainder of sysex message */

#ifdef SYSEXDEBUG
int sysexcount = 0;	/* for debugging */
int sysexdone = 0;
int sysexheadcount = 0;
byte sysexfirst = 0;
int sysexsysex = 0;
#endif

/* midi_flush -- empty out buffers */
/**/
void midi_flush()
{
    midi_read_lock = true;
    buffhead = 0;
    bufftail = 0;
    xbufhead = 0;
    xbuftail = 0;
    xbuf_flush = true;	/* in case sysex continuation messages are still coming */
    midi_read_lock = false;
}


/* Nyquist only uses CMT for Midi and Adagio file IO */
#ifndef NYQUIST
/* Get String representation of MIDI Mgr Version Num.*/
/* See Mac Tech Note #189 for details. */
char *StdMacVerNumToStr(long VerNum, char *VerStr)
{
        char    *RetVal;
        char    MajVer, MinVer, VerStage, VerRev, BugFixVer = 0;
        
        if (VerNum == 0)
        {
                RetVal = NULL;
        }
        else
        {
                MajVer          = (VerNum & 0xFF000000) >> 24;
                MinVer          = (VerNum & 0x00FF0000) >> 16;
                VerStage        = (VerNum & 0x0000FF00) >> 8;
                VerRev          = (VerNum & 0x000000FF) >> 0;
                BugFixVer       =  MinVer & 0x0F;
                
                switch (VerStage)
                {
                        case 0x20:
                                    VerStage = 'd';
                                    break;
                        case 0x40:
                                    VerStage = 'a';
                                    break;
                        case 0x60:
                                    VerStage = 'b';
                                    break;
                        case 0x80:
                                    VerStage = ' ';
                                    break;
                        default:
                                    VerStage = '?';
                                    break;
                }
                
                if (BugFixVer == 0)
                {
                        sprintf(VerStr,"%X.%X%c%X", 
                                    MajVer, MinVer>>4, VerStage, VerRev);
                }
                else
                {
                        sprintf(VerStr,"%X.%X.%X%c%X", 
                                    MajVer, MinVer >> 4, MinVer & 0x0F, VerStage, VerRev);
                }
                
                RetVal = VerStr;
        }
                
        return(RetVal);
}


/* C2PStrCpy -- Convert a C String (from Cstr) into a Pascal string */
/*
 * NOTE: this is not the same code as shipped with midi manager example
 */
char *C2PStrCpy(char *Cstr, Str255 Pstr)
{
        char *c = Cstr;
        char *p = ((char *) Pstr) + 1;
        
        while (*c) *p++ = *c++;
        *Pstr = c - Cstr;
        return( (char *) Pstr );
}

/* This checks to see if THINK C is running under System 7,
   and ONLY WORKS UNDER SYSTEM 7!  Don't use unless you check! */
boolean ThinkCRunning(void)
{
     ProcessSerialNumber processSN;
    OSErr myErr;
    ProcessInfoRec infoRec;
    
    processSN.lowLongOfPSN = kNoProcess;
    processSN.highLongOfPSN = kNoProcess;
    do {
        myErr = GetNextProcess(&processSN);
        
        infoRec.processInfoLength = sizeof(ProcessInfoRec);
        infoRec.processName = 0L;
        infoRec.processAppSpec = 0L;
        myErr = GetProcessInformation(&processSN, &infoRec);
        if (!myErr) {
                if (infoRec.processSignature == 'KAHL') {
                    return(true);
                }
        }
    } while (myErr == noErr);
    return(false);
}

/* This kills off all the other running processes... 
   ONLY WORKS UNDER SYSTEM 7!  Don't use unless you check! */
void KillEverybody(void)
{
    ProcessSerialNumber myProc, processSN;
    ProcessSerialNumber finderPSN;
    ProcessInfoRec infoRec;
    Str31 processName;
    FSSpec procSpec;
    
    OSErr myErr = noErr;
    OSErr otherError;
    AppleEvent theEvent;
    AEDesc theAddress;
    Boolean ourFlag, notFinder;
    Boolean finderFound = false;
    
    GetCurrentProcess(&myProc);
    /* Preset the PSN to no PSN, see IM VI, the Process Manager */
    processSN.lowLongOfPSN = kNoProcess;
    processSN.highLongOfPSN = kNoProcess;
    finderPSN.lowLongOfPSN = 0UL; /* brk: was nil */
    finderPSN.highLongOfPSN = 0UL; /* brk: was nil */
    
    do {
        myErr = GetNextProcess(&processSN);
        /* See if it's us first */
        notFinder = true;
        SameProcess(&myProc, &processSN, &ourFlag);

            infoRec.processInfoLength = sizeof(ProcessInfoRec);
            infoRec.processName = (StringPtr) &processName;
            infoRec.processAppSpec = &procSpec;
            GetProcessInformation(&processSN, &infoRec);
        if (!ourFlag && !finderFound) {
            /* see if it's the Finder, we have to kill the finder LAST */
            /* or else non-sys 7 apps won't get killed */
            /* since the Finder must be there to convert the AppleEvent to Puppet Strings */
            /* if the app is not APpleEvent aware */
            /* Also, FileShare HAS to be killed before the Finder */
            /* or your life will be unpleasant */

            if (infoRec.processSignature == 'MACS' && infoRec.processType == 'FNDR') {
                /* save this number for later  */
                finderPSN = processSN;
                notFinder = false;
                finderFound = true;
            
            } else {
                notFinder = true;
            }
        }
        if (!myErr && !ourFlag && notFinder) {
            otherError = AECreateDesc(typeProcessSerialNumber, (Ptr)&processSN, sizeof(processSN), &theAddress);
            if (!otherError)
                otherError = AECreateAppleEvent(kCoreEventClass, kAEQuitApplication, &theAddress, kAutoGenerateReturnID,
                                                                    kAnyTransactionID, &theEvent);
            if (!otherError)
                AEDisposeDesc(&theAddress);
            /* Again, the Finder will convert the AppleEvent to puppetstrings if */
            /* the application is a System 6 or non-AE aware app.  This ONLY  */
            /* happens for the 4 required (oapp,odoc,pdoc, and quit) AppleEvents  */
            /* and ONLY if you use the PSN for the address */
            if (!otherError)
                AESend(&theEvent, 0L, kAENoReply + kAEAlwaysInteract + kAECanSwitchLayer, kAENormalPriority, kAEDefaultTimeout,
                       0L, 0L);
            AEDisposeDesc(&theEvent);
        }
    } while (!myErr);
    
    /* Now, if the finder was running, it's safe to kill it */
    if (finderPSN.lowLongOfPSN || finderPSN.highLongOfPSN) {
        otherError = AECreateDesc(typeProcessSerialNumber, (Ptr)&finderPSN, sizeof(processSN), &theAddress);
        if (!otherError)
            otherError = AECreateAppleEvent(kCoreEventClass, kAEQuitApplication, &theAddress, kAutoGenerateReturnID,
                                                            kAnyTransactionID, &theEvent);
        if (!otherError)
            AEDisposeDesc(&theAddress);
        if (!otherError)
            AESend(&theEvent, 0L, kAENoReply + kAEAlwaysInteract + kAECanSwitchLayer, kAENormalPriority, kAEDefaultTimeout, 0L,
                   0L);
        AEDisposeDesc(&theEvent);
    }
}

/* Sign into the MIDI Manager. */
/* Set up time, input, and output ports. */
/* Start our time base clock. */
void setup_midimgr(void)
{
    MIDIPortParams  Init;   /* MIDI Mgr Init data structure */ 
    Handle                  TheIconHndl;
    OSErr                   TheErr;
    long                    MIDIMgrVerNum;  /* MIDI Manager Ver (Std Mac Ver #) */
    Str255 name = "\pCMU MIDI Toolkit";
    char MIDIMgrVerStr[256]; /* MIDI Manager Ver (Std Mac Ver # String) */
    long vers;
    EventRecord theEvent;
    
    Gestalt(gestaltSystemVersion, &vers);
    vers = (vers >> 8) & 0xf;                               /* shift result over and mask out major version number */
    if ((vers >= 7) && (!cl_switch("keep"))  && (!ThinkCRunning()))  {
        gprintf(TRANS,"Killing other processes...\n");
        KillEverybody();
        for (vers=0; vers<100; ++vers) {
            while (WaitNextEvent(everyEvent, &theEvent, 0L, 0L)) ;
        }
    }
        
    /* Make sure MIDIMgr is installed and save version num. */
    MIDIMgrVerNum = SndDispVersion(midiToolNum);
    if (MIDIMgrVerNum == 0) {
        gprintf(ERROR, "The MIDI Manager is not installed! Exiting...\n");
        EXIT(1);
    } else {        
        StdMacVerNumToStr(MIDIMgrVerNum, MIDIMgrVerStr);
        gprintf(TRANS,"MIDI Manager Version %s\n", MIDIMgrVerStr);
    }

        
    /* Sign in to the MIDI Manager. */
    TheIconHndl = GetResource('ICN#', 1);
    TheErr = MIDISignIn(CMTclientID,
                        0L, 
                        TheIconHndl,
                        name);
    if (TheErr) {
            gprintf(ERROR, "Trouble signing into MIDI Manager!  Aborting...");
            EXIT(1);
    }

    /* Assume not a Patchbay configuration. */
    GManualPatch = true;    

    /* Add time port. */
    Init.portID = timePortID;
    Init.portType = midiPortTypeTime;
    Init.timeBase = noTimeBaseRefNum;
    Init.readHook = noReadHook;
    Init.initClock.syncType = midiInternalSync;
    Init.initClock.curTime = zeroTime;
    Init.initClock.format = midiFormatMSec;
    Init.refCon = SetCurrentA5();
    C2PStrCpy("TimeBase", Init.name);
    TheErr = MIDIAddPort(CMTclientID, timePortBuffSize, &TimeRefNum, &Init);
    /* Has a PatchBay connection been resolved? */
    if (TheErr == midiVConnectMade) {
        GManualPatch = false;
    } else if (TheErr == memFullErr) {
        gprintf(ERROR, "Not enough room in heap zone to add time port!  Aborting...");
        MIDISignOut(CMTclientID);       
        EXIT(1);
    }
        
    /* Add an input port. */
    Init.portID = inputPortID;
    Init.portType = midiPortTypeInput;
    Init.timeBase = TimeRefNum;
    Init.offsetTime = midiGetCurrent;
    Init.readHook = NewMIDIReadHookProc(CMTreader);
    Init.refCon = SetCurrentA5();
    C2PStrCpy("InputPort", Init.name);
    TheErr = MIDIAddPort(CMTclientID, inputPortBuffSize, &InputRefNum, &Init);
    /* Has a PatchBay connection been resolved? */
    if (TheErr == midiVConnectMade) {
        GManualPatch = false;
    } else if (TheErr == memFullErr) {
        gprintf(ERROR, "Not enough room in heap zone to add input port!  Aborting...");
        MIDISignOut(CMTclientID);       
        EXIT(1);
    }
        
    /* Add an output port. */
    Init.portID = outputPortID;
    Init.portType = midiPortTypeOutput;
    Init.timeBase = TimeRefNum;
    Init.offsetTime = midiGetCurrent;
    Init.readHook = NULL;
    Init.refCon = refCon0;
    C2PStrCpy("OutputPort", Init.name);
    TheErr = MIDIAddPort(CMTclientID, outputPortBuffSize, &OutputRefNum, &Init);
    /* Has a PatchBay connection been resolved? */
    if (TheErr == midiVConnectMade) {
        GManualPatch = false;
    } else if (TheErr == memFullErr) {
        printf("Not enough room in heap zone to add output port!  Aborting...");
        MIDISignOut(CMTclientID);       
        EXIT(1);
    }
        
    if (GManualPatch) {
        PatchPorts(); /* connect ports as they were */
    }
    /* to clean this up (later) call finish_midimgr() */
    cu_register((cu_fn_type) finish_midimgr, (cu_parm_type) finish_midimgr);
        
    /* Start our Clock. */
    MIDIStartTime(TimeRefNum);              
}


/* The Read Hook Function. */

/* 1st 4 bytes of sysex message get saved here and enqueued later */
char save_sysex_head[4];
int save_sysex_head_x = 0;

void sysex_insert(unsigned char data) {
    if (save_sysex_head_x < 4) {
        save_sysex_head[save_sysex_head_x++] = data;
    }
    xbuff[xbuftail++] = data;
    xbuftail &= xbufmask;
    if (xbuftail == xbufhead) {
        set_error(SYSEXOVFL);
    }
    if (data == MIDI_EOX) { /* we're done with the message */
        *((long *) (((byte *) buff) + bufftail)) = *((long *)save_sysex_head);
        bufftail = (bufftail + 4) & BUFF_MASK;
        if (bufftail == buffhead) {
            set_error(BUFFOVFL);
        }
    }
}

/* Read all incomming MIDI data. */

pascal short CMTreader(MIDIPacket *ThePacketPtr, long TheRefCon)
{
    /* Set up our A5 world. */
    long    SysA5 = SetA5(TheRefCon);
    short   RetVal = midiMorePacket, i, j;
    unsigned char *mm_data = ThePacketPtr->data;
    register byte data1 = mm_data[1];
        
    if (midi_read_lock) {
        /* Don't want to read packet now, get it later */
        /* DOES THIS REALLY WORK?  WHAT WILL CAUSE AN INTERRUPT
         * TO OCCUR LATER?  THIS IS ONLY USED BY midi_flush, IS
         * BASED ON THE MidiArp CODE FROM APPLE, AND IS UNTESTED - RBD
         */
        RetVal = midiKeepPacket;
        goto alldone;	
    }

    /* see if Packet is an error message */
    if (((ThePacketPtr->flags & midiTypeMask) == midiMgrType) && 
        *((short *) (&(ThePacketPtr->data))) < midiMaxErr) {
        set_error(MIDIMGRERR);
        goto alldone;
    }
        
    /* filter out control changes */
    if (ctrlFilter) {
        register int hibits = *mm_data & 0xF0;
        if (hibits == 0xD0 ||   /* Chan Pressure */
            hibits == 0xE0 ||       /* Pitch Bend */
            hibits == 0xA0 ||       /* Poly Pressure */
            ((hibits == 0xB0) &&    /* Control change (don't count switches) */
             ((data1 < 64) || (data1 > 121)))) {
            /* CONTROL MESSAGE HAS BEEN FILTERED */
            goto alldone;
        }
    } else if (realFilter) {
        register int hibits = *mm_data & 0xF0;
        if (hibits >= 0xF8) goto alldone;
    }

        
    /* if not a continuation, copy the data into cmt_data */
    /* The logic to detect a non-continued
     * packet or a first packet is: "flags bit 1 is clear".
     */
    if ((((ThePacketPtr->flags & midiContMask) == midiNoCont)) && 
        (*mm_data != MIDI_SYSEX)) {
        register byte *cmt_data = ((byte *) buff) + bufftail;
        *((long *) cmt_data) = *((long *) mm_data);

        bufftail = (bufftail + 4) & BUFF_MASK;
        if (bufftail == buffhead) {
            /* filled buffer faster than client emptied it */
            set_error(BUFFOVFL);
        }
    }
        
    /* see if we have a sysex message to copy to buffer */
    if (xbuff && !exclFilter &&
        ((ThePacketPtr->flags & midiContMask) || *mm_data == MIDI_SYSEX)) {
        int i;
        register byte *x_data = xbuff + xbuftail;

        /* iterate over data in message */
        /* NOTE: in the previous implementation, I thought Sysex messages were
         * always starting at the beginning of the buffer, but that didn't work.
         * This implementation assumes nothing -- it is slower because of additional
         * testing and parsing inside the loop, but seems to work.
         */
        for (i = ThePacketPtr->len - 6; i > 0; i--) {
            if (xbuf_flush) {	/* we're searching for beginning of message */
                if (*mm_data == MIDI_SYSEX) {
                    xbuf_flush = false;
                    sysex_insert(MIDI_SYSEX);
                }
            } else {	/* we're scanning to the end of the message */
                if (*mm_data == MIDI_SYSEX) {	/* found it, insert proper EOX */
                    sysex_insert(MIDI_EOX);
                    sysex_insert(MIDI_SYSEX);
                } else if (*mm_data == MIDI_EOX) {	/* found it */
                    sysex_insert(MIDI_EOX);
                    xbuf_flush = true;
                } else sysex_insert(*mm_data);

            }
            mm_data++;
        }
    }
alldone:

    /* Restore the systems A5 world. */
    SetA5(SysA5);
    
    return(RetVal);
}


/* Sign out from the MIDI Manager. */
void finish_midimgr(void)
{
        if (GManualPatch && patch_flag) {
                SavePatch(timePortID, timePortResInfoID, "timePortInfo");
                SavePatch(inputPortID, inputPortResInfoID, "inputPortInfo");
                SavePatch(outputPortID, outputPortResInfoID, "outputPortInfo");
        }
        MIDISignOut(CMTclientID);
}



/* Alert user to Resource Manager Error. */
void
ReportResError(char *Msg)
{
        OSErr   TheErr;
        char    Buf[256];
        
        if ( (TheErr = ResError()) != noErr) {
                gprintf(ERROR, "ResError %d: %s...Aborting.", TheErr, Msg);
                EXIT(1);
        } else {
                /* gprintf(ERROR, "%s OK\n", Msg); */
        }
}


/****************************************************************************
*                                       error handling
* Effect:
*       various error conditions are flagged by setting bits in
*       the global midi_error_flags.  it is up to the client to clear this 
*       word when necessary.
****************************************************************************/

private void set_error(int bit)
{
        midi_error_flags |= (1 << bit);
}


void midi_show_errors()
{
    if (midi_error_flags & (1<<BUFFOVFL)) 
        gprintf(ERROR, "Midi Buffer Overflow Error\n");
    if (midi_error_flags & (1<<MIDIMGRERR)) 
        gprintf(ERROR, "Midi Manager Error\n");
    if (midi_error_flags & (1<<SYSEXOVFL)) 
        gprintf(ERROR, "Midi Sysex Overflow Error\n");
}


/**************** PATCHING CODE ***************/

/*
        MIDIArp Time, Input, and Output Port 
        Info Record Resource ID's.
*/

/* Get previously saved port connections (port info records) */
/* from application's 'port' resource. */
void
PatchPorts(void)
{
        MIDIPortInfoHdl PortInfoH;      /* Handle to port info record. */
        MIDIPortInfoPtr PortInfoP;      /* Pointer to port info record. */
        short                   i, TheErr;
        
        patch_flag = cl_switch("patch");
                
                /* SET UP TIME PORT CONNECTIONS. */
        if (patch_flag)
                PortInfoH = (MIDIPortInfoHdl) GetResource(portResType, timePortResInfoID);
        if (!patch_flag || PortInfoH == NULL) {
                MIDIIDListHdl clients, ports;
                OSErr err;
                
                gprintf(TRANS, "Connecting to MIDI IN and OUT\n");
#ifdef MIDIMGR_VERBOSE
                clients = MIDIGetClients();
                gprintf(TRANS, "clients = %lx\n", clients);
                HLock((Handle) clients);
                
                for (i = 0; i < (*clients)->numIDs; i++) {
                        OSType id = (*clients)->list[i];
                        gprintf(TRANS, "%d: %c%c%c%c\n", i, (char) (id>>24),
                                    (char) ((id >> 16) & 0xFF), (char) ((id >> 8) & 0xFF),
                                    (char) (id & 0xFF));
                }
                ports = MIDIGetPorts('amdr');
                HLock((Handle) ports);
                for (i = 0; i < (*ports)->numIDs; i++) {
                        OSType id = (*ports)->list[i];
                        gprintf(TRANS, "%d: %c%c%c%c\n", i, (char) (id>>24),
                                    (char) ((id >> 16) & 0xFF), (char) ((id >> 8) & 0xFF),
                                    (char) (id & 0xFF));
                }
                HUnlock((Handle) ports);
                HUnlock((Handle) clients);
#endif
                /* the work starts here */
                err = MIDIConnectData('CMT ', 'Cout', 'amdr', 'Aout');
                /* gprintf(TRANS, "Connected CMT.Cout to amdr.Aout: %d\n", err); */
                err = MIDIConnectData('amdr', 'Ain ', 'CMT ', 'Bin ');
                /* gprintf(TRANS, "Connected amdr.Ain to CMT.Bin: %d\n", err); */

                return;
        }
        HLock((Handle) PortInfoH);
        PortInfoP = *PortInfoH;
        if (GetHandleSize((Handle) PortInfoH) != 0)
        {
                        /* Were we supposed to be sync'd to another client? */
                if (PortInfoP->timeBase.clientID != noClient)
                {               
                                    /* Yes, so make that client our time base. */
                        TheErr = MIDIConnectTime(
                                                                    PortInfoP->timeBase.clientID, 
                                                                    PortInfoP->timeBase.portID,
                                                                    CMTclientID, 
                                                                    timePortID 
                                                                    );
#ifdef IGNORE
                                    /* Is the client still signed in? */
                        if (TheErr != midiVConnectErr) 
                        {       
                                                    /* Yes, so set our sync mode to external. */
                                    MIDISetSync(ArpGlobals.TimeRefNum, midiExternalSync);
                        }
#endif
                        
                }
                        /* Were we somebody else's time base? */
                for (i=0; i<PortInfoP->numConnects; i++)
                {
                        MIDIConnectTime(CMTclientID, 
                                                                    timePortID, 
                                                                    PortInfoP->cList[i].clientID, 
                                                                    PortInfoP->cList[i].portID);
                }
        }
        HUnlock((Handle) PortInfoH);
        ReleaseResource((Handle) PortInfoH);
        ReportResError("PatchPorts/ReleaseResource()");
        
                /* SET UP INPUT PORT CONNECTIONS. */
        PortInfoH = (MIDIPortInfoHdl) GetResource(portResType, inputPortResInfoID);
        if (PortInfoH == NULL)
        {
                ReportResError("PatchPorts/GetResource()");
        }
        HLock((Handle) PortInfoH);
        PortInfoP = *PortInfoH;
        if (GetHandleSize((Handle) PortInfoH) != 0)
        {
                        /* Were we connected to anyone? */
                for (i=0; i<PortInfoP->numConnects; i++)
                {
                        MIDIConnectData(CMTclientID, 
                                                                    inputPortID, 
                                                                    PortInfoP->cList[i].clientID, 
                                                                    PortInfoP->cList[i].portID);
                }
        }
        HUnlock((Handle) PortInfoH);
        ReleaseResource((Handle) PortInfoH);
        ReportResError("PatchPorts/GetResource()");
        
                /* SET UP OUTPUT PORT CONNECTIONS. */
        PortInfoH = (MIDIPortInfoHdl) GetResource(portResType, outputPortResInfoID);
        if (PortInfoH == NULL)
        {       
                ReportResError("PatchPorts/GetResource()");
        }
        HLock((Handle) PortInfoH);
        PortInfoP = *PortInfoH;
        if (GetHandleSize((Handle) PortInfoH) != 0) {
                        /* Were we connected to anyone? */
                for (i=0; i<PortInfoP->numConnects; i++)
                {
                        MIDIConnectData(CMTclientID, 
                                                                    outputPortID, 
                                                                    PortInfoP->cList[i].clientID, 
                                                                    PortInfoP->cList[i].portID);
                }
        }
        HUnlock((Handle) PortInfoH);
        ReleaseResource((Handle) PortInfoH);
        ReportResError("PatchPorts/ReleaseResource()");
        
}

/* Save current port connections (port info records) */
/* to application's 'port' resource. */
void
SavePatch(OSType PortID, short PortInfoResID, char *PortInfoResName)
{
        Handle                  PortResH;       /* Handle to ptch resource. */
        CursHandle              WatchCurs;      
        
        WatchCurs = GetCursor(watchCursor);
        HLock((Handle) WatchCurs);
        SetCursor(*WatchCurs);
        HUnlock((Handle) WatchCurs);

        
                /* Remove existing port info resource. */
        PortResH = GetResource(portResType, PortInfoResID);
        /* gprintf(TRANS, "PortResH: %lx, *PortResH: %lx\n", PortResH, *PortResH); */
        if (PortResH) {
                ReportResError("SavePatch/GetResource()");
                RmveResource(PortResH);
                ReportResError("SavePatch/RmveResource()");
                DisposHandle(PortResH);
                UpdateResFile(CurResFile());
                ReportResError("SavePatch/UpdateResFile()");
        }
        
                /*      Get new configurateion. */
        PortResH = (Handle) MIDIGetPortInfo(CMTclientID, PortID);
        
                /*      Save new configurateion. */
        CtoPstr(PortInfoResName);
        AddResource(PortResH, portResType, PortInfoResID,
            (ConstStr255Param) PortInfoResName);
        PtoCstr((unsigned char *) PortInfoResName);
        
        ReportResError("SavePatch/AddResource()");
        WriteResource(PortResH);
        ReportResError("SavePatch/WriteResource()");
        UpdateResFile(CurResFile());
        ReportResError("SavePatch/UpdateResFile()");
        ReleaseResource(PortResH);
        ReportResError("SavePatch/ReleaseResource()");
        
        InitCursor();
}
#endif /* NYQUIST */
