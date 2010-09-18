#ifdef _MSC_VER
 #pragma warning(disable: 4244) // stop warnings about downsize typecasts
 #pragma warning(disable: 4018) // stop warnings about signed/unsigned
#endif

#include "stdlib.h"
#include "string.h"
#include "portmidi.h"
#include "porttime.h"
#include "pmutil.h"
#include "pminternal.h"
#include <assert.h>

#define MIDI_CLOCK      0xf8
#define MIDI_ACTIVE     0xfe
#define MIDI_STATUS_MASK 0x80
#define MIDI_SYSEX      0xf0
#define MIDI_EOX        0xf7
#define MIDI_START      0xFA
#define MIDI_STOP       0xFC
#define MIDI_CONTINUE   0xFB
#define MIDI_F9         0xF9
#define MIDI_FD         0xFD
#define MIDI_RESET      0xFF
#define MIDI_NOTE_ON    0x90
#define MIDI_NOTE_OFF   0x80
#define MIDI_CHANNEL_AT 0xD0
#define MIDI_POLY_AT    0xA0
#define MIDI_PROGRAM    0xC0
#define MIDI_CONTROL    0xB0
#define MIDI_PITCHBEND  0xE0
#define MIDI_MTC        0xF1
#define MIDI_SONGPOS    0xF2
#define MIDI_SONGSEL    0xF3
#define MIDI_TUNE       0xF6

#define is_empty(midi) ((midi)->tail == (midi)->head)

/* this is not static so that pm_init can set it directly if
 *   (see pmmac.c:pm_init())
 */
int pm_initialized = FALSE;

int pm_hosterror;
char pm_hosterror_text[PM_HOST_ERROR_MSG_LEN];

#ifdef PM_CHECK_ERRORS

#include <stdio.h>

#define STRING_MAX 80

static void prompt_and_exit(void)
{
    char line[STRING_MAX];
    printf("type ENTER...");
    fgets(line, STRING_MAX, stdin);
    /* this will clean up open ports: */
    exit(-1);
}


static PmError pm_errmsg(PmError err)
{
    if (err == pmHostError) {
        /* it seems pointless to allocate memory and copy the string,
         * so I will do the work of Pm_GetHostErrorText directly
         */
        printf("PortMidi found host error...\n  %s\n", pm_hosterror_text);
        pm_hosterror = FALSE;
        pm_hosterror_text[0] = 0; /* clear the message */
        prompt_and_exit();
    } else if (err < 0) {
        printf("PortMidi call failed...\n  %s\n", Pm_GetErrorText(err));
        prompt_and_exit();
    }
    return err;
}
#else
#define pm_errmsg(err) err
#endif

/*
====================================================================
system implementation of portmidi interface
====================================================================
*/

int pm_descriptor_max = 0;
int pm_descriptor_index = 0;
descriptor_type descriptors = NULL;

/* pm_add_device -- describe interface/device pair to library 
 *
 * This is called at intialization time, once for each 
 * interface (e.g. DirectSound) and device (e.g. SoundBlaster 1)
 * The strings are retained but NOT COPIED, so do not destroy them!
 *
 * returns pmInvalidDeviceId if device memory is exceeded
 * otherwise returns pmNoError
 */
PmError pm_add_device(char *interf, char *name, int input, 
                      void *descriptor, pm_fns_type dictionary) {
    if (pm_descriptor_index >= pm_descriptor_max) {
        // expand descriptors
        descriptor_type new_descriptors = (descriptor_type) 
            pm_alloc(sizeof(descriptor_node) * (pm_descriptor_max + 32));
        if (!new_descriptors) return pmInsufficientMemory;
        if (descriptors) {
            memcpy(new_descriptors, descriptors, 
                   sizeof(descriptor_node) * pm_descriptor_max);
            free(descriptors);
        }
        pm_descriptor_max += 32;
        descriptors = new_descriptors;
    }
    descriptors[pm_descriptor_index].pub.interf = interf;
    descriptors[pm_descriptor_index].pub.name = name;
    descriptors[pm_descriptor_index].pub.input = input;
    descriptors[pm_descriptor_index].pub.output = !input;

    /* default state: nothing to close (for automatic device closing) */
    descriptors[pm_descriptor_index].pub.opened = FALSE;

    /* ID number passed to win32 multimedia API open */
    descriptors[pm_descriptor_index].descriptor = descriptor;
    
    /* points to PmInternal, allows automatic device closing */
    descriptors[pm_descriptor_index].internalDescriptor = NULL;

    descriptors[pm_descriptor_index].dictionary = dictionary;
    
    pm_descriptor_index++;
    
    return pmNoError;
}


/* utility to look up device, given a pattern, 
   note: pattern is modified
 */
int pm_find_default_device(char *pattern, int is_input)
{
    int id = pmNoDevice;
    int i;
    /* first parse pattern into name, interf parts */
    char *interf_pref = ""; /* initially assume it is not there */
    char *name_pref = strstr(pattern, ", ");

    if (name_pref) { /* found separator, adjust the pointer */
        interf_pref = pattern;
        name_pref[0] = 0;
        name_pref += 2;
    } else {
        name_pref = pattern; /* whole string is the name pattern */
    }
    for (i = 0; i < pm_descriptor_index; i++) {
        const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
        if (info->input == is_input &&
            strstr(info->name, name_pref) &&
            strstr(info->interf, interf_pref)) {
            id = i;
            break;
        }
    }    
    return id;
}


/*
====================================================================
portmidi implementation
====================================================================
*/

PMEXPORT int Pm_CountDevices( void ) {
    Pm_Initialize();
    /* no error checking -- Pm_Initialize() does not fail */
    return pm_descriptor_index;
}


PMEXPORT const PmDeviceInfo* Pm_GetDeviceInfo( PmDeviceID id ) {
    Pm_Initialize(); /* no error check needed */
    if (id >= 0 && id < pm_descriptor_index) {
        return &descriptors[id].pub;
    }
    return NULL;
}

/* pm_success_fn -- "noop" function pointer */
PmError pm_success_fn(PmInternal *midi) {
    return pmNoError;
}

/* none_write -- returns an error if called */
PmError none_write_short(PmInternal *midi, PmEvent *buffer) {
    return pmBadPtr;
}

/* pm_fail_timestamp_fn -- placeholder for begin_sysex and flush */
PmError pm_fail_timestamp_fn(PmInternal *midi, PmTimestamp timestamp) {
    return pmBadPtr;
}

PmError none_write_byte(PmInternal *midi, unsigned char byte, 
                        PmTimestamp timestamp) {
    return pmBadPtr;
}

/* pm_fail_fn -- generic function, returns error if called */
PmError pm_fail_fn(PmInternal *midi) {
    return pmBadPtr;
}

static PmError none_open(PmInternal *midi, void *driverInfo) {
    return pmBadPtr;
}
static void none_get_host_error(PmInternal * midi, char * msg, unsigned int len) {
    *msg = 0; // empty string
}
static unsigned int none_has_host_error(PmInternal * midi) {
    return FALSE;
}
PmTimestamp none_synchronize(PmInternal *midi) {
    return 0;
}

#define none_abort pm_fail_fn
#define none_close pm_fail_fn

pm_fns_node pm_none_dictionary = {
    none_write_short,
    none_sysex,
    none_sysex,
    none_write_byte,
    none_write_short,
    none_write_flush,
    none_synchronize,
    none_open,
    none_abort, 
    none_close,
    none_poll,
    none_has_host_error,
    none_get_host_error 
};


PMEXPORT const char *Pm_GetErrorText( PmError errnum ) {
    const char *msg;

    switch(errnum)
    {
    case pmNoError:                  
        msg = ""; 
        break;
    case pmHostError:                
        msg = "PortMidi: `Host error'"; 
        break;
    case pmInvalidDeviceId:          
        msg = "PortMidi: `Invalid device ID'"; 
        break;
    case pmInsufficientMemory:       
        msg = "PortMidi: `Insufficient memory'"; 
        break;
    case pmBufferTooSmall:           
        msg = "PortMidi: `Buffer too small'"; 
        break;
    case pmBadPtr:                   
        msg = "PortMidi: `Bad pointer'"; 
        break;
    case pmInternalError:            
        msg = "PortMidi: `Internal PortMidi Error'"; 
        break;
    case pmBufferOverflow:
        msg = "PortMidi: `Buffer overflow'";
        break;
    case pmBadData:
        msg = "PortMidi: `Invalid MIDI message Data'";
        break;
    case pmBufferMaxSize:
        msg = "PortMidi: `Buffer cannot be made larger'";
        break;
    default:                         
        msg = "PortMidi: `Illegal error number'"; 
        break;
    }
    return msg;
}


/* This can be called whenever you get a pmHostError return value.
 * The error will always be in the global pm_hosterror_text.
 */
PMEXPORT void Pm_GetHostErrorText(char * msg, unsigned int len) {
    assert(msg);
    assert(len > 0);
    if (pm_hosterror) {
        strncpy(msg, (char *) pm_hosterror_text, len);
        pm_hosterror = FALSE;
        pm_hosterror_text[0] = 0; /* clear the message; not necessary, but it
                                     might help with debugging */
        msg[len - 1] = 0; /* make sure string is terminated */
    } else {
        msg[0] = 0; /* no string to return */
    }
}


PMEXPORT int Pm_HasHostError(PortMidiStream * stream) {
    if (pm_hosterror) return TRUE;
    if (stream) {
        PmInternal * midi = (PmInternal *) stream;
        pm_hosterror = (*midi->dictionary->has_host_error)(midi);
        if (pm_hosterror) {
            midi->dictionary->host_error(midi, pm_hosterror_text, 
                                         PM_HOST_ERROR_MSG_LEN);
            /* now error message is global */
            return TRUE;
        }
    }
    return FALSE;
}


PMEXPORT PmError Pm_Initialize( void ) {
    if (!pm_initialized) {
        pm_hosterror = FALSE;
        pm_hosterror_text[0] = 0; /* the null string */
        pm_init();
        pm_initialized = TRUE;
    }
    return pmNoError;
}


PMEXPORT PmError Pm_Terminate( void ) {
    if (pm_initialized) {
        pm_term();
        // if there are no devices, descriptors might still be NULL
        if (descriptors != NULL) {
            free(descriptors);
            descriptors = NULL;
        }
        pm_descriptor_index = 0;
        pm_descriptor_max = 0;
        pm_initialized = FALSE;
    }
    return pmNoError;
}


/* Pm_Read -- read up to length messages from source into buffer */
/*
 * returns number of messages actually read, or error code
 */
PMEXPORT int Pm_Read(PortMidiStream *stream, PmEvent *buffer, int32_t length) {
    PmInternal *midi = (PmInternal *) stream;
    int n = 0;
    PmError err = pmNoError;
    pm_hosterror = FALSE;
    /* arg checking */
    if(midi == NULL)
        err = pmBadPtr;
    else if(!descriptors[midi->device_id].pub.opened)
        err = pmBadPtr;
    else if(!descriptors[midi->device_id].pub.input)
        err = pmBadPtr;    
    /* First poll for data in the buffer...
     * This either simply checks for data, or attempts first to fill the buffer
     * with data from the MIDI hardware; this depends on the implementation.
     * We could call Pm_Poll here, but that would redo a lot of redundant
     * parameter checking, so I copied some code from Pm_Poll to here: */
    else err = (*(midi->dictionary->poll))(midi);

    if (err != pmNoError) {
        if (err == pmHostError) {
            midi->dictionary->host_error(midi, pm_hosterror_text, 
                                         PM_HOST_ERROR_MSG_LEN);
          pm_hosterror = TRUE;
        }
        return pm_errmsg(err);
    }

    while (n < length) {
        PmError err = Pm_Dequeue(midi->queue, buffer++);
        if (err == pmBufferOverflow) {
            /* ignore the data we have retreived so far */
            return pm_errmsg(pmBufferOverflow);
        } else if (err == 0) { /* empty queue */
            break;
        }
        n++;
    }
    return n;
}

PMEXPORT PmError Pm_Poll( PortMidiStream *stream )
{
    PmInternal *midi = (PmInternal *) stream;
    PmError err;

    pm_hosterror = FALSE;
    /* arg checking */
    if(midi == NULL)
        err = pmBadPtr;
    else if (!descriptors[midi->device_id].pub.opened)
        err = pmBadPtr;
    else if (!descriptors[midi->device_id].pub.input)
        err = pmBadPtr;
    else
        err = (*(midi->dictionary->poll))(midi);

    if (err != pmNoError) {
        if (err == pmHostError) {
            midi->dictionary->host_error(midi, pm_hosterror_text, 
                                         PM_HOST_ERROR_MSG_LEN);
           pm_hosterror = TRUE;
        }
        return pm_errmsg(err);
    }

    return !Pm_QueueEmpty(midi->queue);
}


/* this is called from Pm_Write and Pm_WriteSysEx to issue a
 * call to the system-dependent end_sysex function and handle 
 * the error return
 */
static PmError pm_end_sysex(PmInternal *midi)
{
    PmError err = (*midi->dictionary->end_sysex)(midi, 0);
    midi->sysex_in_progress = FALSE;
    if (err == pmHostError) {
        midi->dictionary->host_error(midi, pm_hosterror_text, 
                                     PM_HOST_ERROR_MSG_LEN);
        pm_hosterror = TRUE;
    }
    return err;
}


/* to facilitate correct error-handling, Pm_Write, Pm_WriteShort, and
   Pm_WriteSysEx all operate a state machine that "outputs" calls to
   write_short, begin_sysex, write_byte, end_sysex, and write_realtime */

PMEXPORT PmError Pm_Write( PortMidiStream *stream, PmEvent *buffer, int32_t length)
{
    PmInternal *midi = (PmInternal *) stream;
    PmError err = pmNoError;
    int i;
    int bits;
    
    pm_hosterror = FALSE;
    /* arg checking */
    if(midi == NULL)
        err = pmBadPtr;
    else if(!descriptors[midi->device_id].pub.opened)
        err = pmBadPtr;
    else if(!descriptors[midi->device_id].pub.output)
        err = pmBadPtr;
    else
        err = pmNoError;
    
    if (err != pmNoError) goto pm_write_error;
    
    if (midi->latency == 0) {
        midi->now = 0;
    } else {
        midi->now = (*(midi->time_proc))(midi->time_info);
        if (midi->first_message || midi->sync_time + 100 /*ms*/ < midi->now) {
            /* time to resync */
            midi->now = (*midi->dictionary->synchronize)(midi);
            midi->first_message = FALSE;
        }
    }
    /* error recovery: when a sysex is detected, we call
     *   dictionary->begin_sysex() followed by calls to
     *   dictionary->write_byte() and dictionary->write_realtime()
     *   until an end-of-sysex is detected, when we call
     *   dictionary->end_sysex(). After an error occurs, 
     *   Pm_Write() continues to call functions. For example,
     *   it will continue to call write_byte() even after
     *   an error sending a sysex message, and end_sysex() will be
     *   called when an EOX or non-real-time status is found.
     * When errors are detected, Pm_Write() returns immediately, 
     *   so it is possible that this will drop data and leave
     *   sysex messages in a partially transmitted state.
     */
    for (i = 0; i < length; i++) {
        uint32_t msg = buffer[i].message;
        bits = 0;
        /* is this a sysex message? */
        if (Pm_MessageStatus(msg) == MIDI_SYSEX) {
            if (midi->sysex_in_progress) {
                /* error: previous sysex was not terminated by EOX */
                midi->sysex_in_progress = FALSE;
                err = pmBadData;
                goto pm_write_error;
            }
            midi->sysex_in_progress = TRUE;
            if ((err = (*midi->dictionary->begin_sysex)(midi, 
                               buffer[i].timestamp)) != pmNoError)
                goto pm_write_error;
            if ((err = (*midi->dictionary->write_byte)(midi, MIDI_SYSEX,
                               buffer[i].timestamp)) != pmNoError) 
                goto pm_write_error;
            bits = 8;
            /* fall through to continue sysex processing */
        } else if ((msg & MIDI_STATUS_MASK) && 
                   (Pm_MessageStatus(msg) != MIDI_EOX)) {
            /* a non-sysex message */
            if (midi->sysex_in_progress) {
                /* this should be a realtime message */
                if (is_real_time(msg)) {
                    if ((err = (*midi->dictionary->write_realtime)(midi, 
                                       &(buffer[i]))) != pmNoError)
                        goto pm_write_error;
                } else {
                    midi->sysex_in_progress = FALSE;
                    err = pmBadData;
                    /* ignore any error from this, because we already have one */
                    /* pass 0 as timestamp -- it's ignored */
                    (*midi->dictionary->end_sysex)(midi, 0);
                    goto pm_write_error;
                }
            } else { /* regular short midi message */
                if ((err = (*midi->dictionary->write_short)(midi, 
                                   &(buffer[i]))) != pmNoError)
                    goto pm_write_error;
                continue;
            }
        }
        if (midi->sysex_in_progress) { /* send sysex bytes until EOX */
            /* see if we can accelerate data transfer */
            if (bits == 0 && midi->fill_base && /* 4 bytes to copy */
                (*midi->fill_offset_ptr) + 4 <= midi->fill_length &&
                (msg & 0x80808080) == 0) { /* all data */
                    /* copy 4 bytes from msg to fill_base + fill_offset */
                    unsigned char *ptr = midi->fill_base + 
                                         *(midi->fill_offset_ptr);
                    ptr[0] = msg; ptr[1] = msg >> 8; 
                    ptr[2] = msg >> 16; ptr[3] = msg >> 24;
                    (*midi->fill_offset_ptr) += 4;
                     continue;
            }
            /* no acceleration, so do byte-by-byte copying */
            while (bits < 32) {
                unsigned char midi_byte = (unsigned char) (msg >> bits);
                if ((err = (*midi->dictionary->write_byte)(midi, midi_byte, 
                                   buffer[i].timestamp)) != pmNoError)
                    goto pm_write_error;
                if (midi_byte == MIDI_EOX) {
                    err = pm_end_sysex(midi);
                    if (err != pmNoError) goto error_exit;
                    break; /* from while loop */
                }
                bits += 8;
            }
        } else {
            /* not in sysex mode, but message did not start with status */
            err = pmBadData;
            goto pm_write_error;
        }
    }
    /* after all messages are processed, send the data */
    if (!midi->sysex_in_progress)
        err = (*midi->dictionary->write_flush)(midi, 0);
pm_write_error:
    if (err == pmHostError) {
        midi->dictionary->host_error(midi, pm_hosterror_text, 
                                     PM_HOST_ERROR_MSG_LEN);
        pm_hosterror = TRUE;
    }
error_exit:
    return pm_errmsg(err);
}


PMEXPORT PmError Pm_WriteShort(PortMidiStream *stream, PmTimestamp when, PmMessage msg)
{
    PmEvent event;
    
    event.timestamp = when;
    event.message = msg;
    return Pm_Write(stream, &event, 1);
}


PMEXPORT PmError Pm_WriteSysEx(PortMidiStream *stream, PmTimestamp when, 
                      unsigned char *msg)
{
    /* allocate buffer space for PM_DEFAULT_SYSEX_BUFFER_SIZE bytes */
    /* each PmEvent holds sizeof(PmMessage) bytes of sysex data */
    #define BUFLEN ((int) (PM_DEFAULT_SYSEX_BUFFER_SIZE / sizeof(PmMessage)))
    PmEvent buffer[BUFLEN];
    int buffer_size = 1; /* first time, send 1. After that, it's BUFLEN */
    PmInternal *midi = (PmInternal *) stream;
    /* the next byte in the buffer is represented by an index, bufx, and
       a shift in bits */
    int shift = 0;
    int bufx = 0;
    buffer[0].message = 0;
    buffer[0].timestamp = when;

    while (1) {
        /* insert next byte into buffer */
        buffer[bufx].message |= ((*msg) << shift);
        shift += 8;
        if (*msg++ == MIDI_EOX) break;
        if (shift == 32) {
            shift = 0;
            bufx++;
            if (bufx == buffer_size) {
                PmError err = Pm_Write(stream, buffer, buffer_size);
                /* note: Pm_Write has already called errmsg() */
                if (err) return err;
                /* prepare to fill another buffer */
                bufx = 0;
                buffer_size = BUFLEN;
                /* optimization: maybe we can just copy bytes */
                if (midi->fill_base) {
                    PmError err;
                    while (*(midi->fill_offset_ptr) < midi->fill_length) {
                        midi->fill_base[(*midi->fill_offset_ptr)++] = *msg;
                        if (*msg++ == MIDI_EOX) {
                            err = pm_end_sysex(midi);
                            if (err != pmNoError) return pm_errmsg(err);
                            goto end_of_sysex;
                        }
                    }
                    /* I thought that I could do a pm_Write here and
                     * change this if to a loop, avoiding calls in Pm_Write
                     * to the slower write_byte, but since 
                     * sysex_in_progress is true, this will not flush
                     * the buffer, and we'll infinite loop: */
                    /* err = Pm_Write(stream, buffer, 0);
                       if (err) return err; */
                    /* instead, the way this works is that Pm_Write calls
                     * write_byte on 4 bytes. The first, since the buffer
                     * is full, will flush the buffer and allocate a new
                     * one. This primes the buffer so
                     * that we can return to the loop above and fill it
                     * efficiently without a lot of function calls.
                     */
                    buffer_size = 1; /* get another message started */
                }
            }
            buffer[bufx].message = 0;
            buffer[bufx].timestamp = when;
        } 
        /* keep inserting bytes until you find MIDI_EOX */
    }
end_of_sysex:
    /* we're finished sending full buffers, but there may
     * be a partial one left.
     */
    if (shift != 0) bufx++; /* add partial message to buffer len */
    if (bufx) { /* bufx is number of PmEvents to send from buffer */
        PmError err = Pm_Write(stream, buffer, bufx);
        if (err) return err;
    }
    return pmNoError;
}



PMEXPORT PmError Pm_OpenInput(PortMidiStream** stream,
                     PmDeviceID inputDevice,
                     void *inputDriverInfo,
                     int32_t bufferSize,
                     PmTimeProcPtr time_proc,
                     void *time_info)
{
    PmInternal *midi;
    PmError err = pmNoError;
    pm_hosterror = FALSE;
    *stream = NULL;
    
    /* arg checking */
    if (inputDevice < 0 || inputDevice >= pm_descriptor_index) 
        err = pmInvalidDeviceId;
    else if (!descriptors[inputDevice].pub.input) 
        err =  pmInvalidDeviceId;
    else if(descriptors[inputDevice].pub.opened)
        err =  pmInvalidDeviceId;
    
    if (err != pmNoError) 
        goto error_return;

    /* create portMidi internal data */
    midi = (PmInternal *) pm_alloc(sizeof(PmInternal)); 
    *stream = midi;
    if (!midi) {
        err = pmInsufficientMemory;
        goto error_return;
    }
    midi->device_id = inputDevice;
    midi->write_flag = FALSE;
    midi->time_proc = time_proc;
    midi->time_info = time_info;
    /* windows adds timestamps in the driver and these are more accurate than
       using a time_proc, so do not automatically provide a time proc. Non-win
       implementations may want to provide a default time_proc in their
       system-specific midi_out_open() method.
     */
    if (bufferSize <= 0) bufferSize = 256; /* default buffer size */
    midi->queue = Pm_QueueCreate(bufferSize, (int32_t) sizeof(PmEvent));
    if (!midi->queue) {
        /* free portMidi data */
        *stream = NULL;
        pm_free(midi); 
        err = pmInsufficientMemory;
        goto error_return;
    }
    midi->buffer_len = bufferSize; /* portMidi input storage */
    midi->latency = 0; /* not used */
    midi->sysex_in_progress = FALSE;
    midi->sysex_message = 0; 
    midi->sysex_message_count = 0; 
    midi->filters = PM_FILT_ACTIVE;
    midi->channel_mask = 0xFFFF;
    midi->sync_time = 0;
    midi->first_message = TRUE;
    midi->dictionary = descriptors[inputDevice].dictionary;
    midi->fill_base = NULL;
    midi->fill_offset_ptr = NULL;
    midi->fill_length = 0;
    descriptors[inputDevice].internalDescriptor = midi;
    /* open system dependent input device */
    err = (*midi->dictionary->open)(midi, inputDriverInfo);
    if (err) {
        *stream = NULL;
        descriptors[inputDevice].internalDescriptor = NULL;
        /* free portMidi data */
        Pm_QueueDestroy(midi->queue);
        pm_free(midi);
    } else {
        /* portMidi input open successful */
        descriptors[inputDevice].pub.opened = TRUE;
    }
error_return:
    /* note: if there is a pmHostError, it is the responsibility
     * of the system-dependent code (*midi->dictionary->open)()
     * to set pm_hosterror and pm_hosterror_text
     */
    return pm_errmsg(err);
}


PMEXPORT PmError Pm_OpenOutput(PortMidiStream** stream,
                      PmDeviceID outputDevice,
                      void *outputDriverInfo,
                      int32_t bufferSize,
                      PmTimeProcPtr time_proc,
                      void *time_info,
                      int32_t latency ) 
{
    PmInternal *midi;
    PmError err = pmNoError;
    pm_hosterror = FALSE;
    *stream =  NULL;
    
    /* arg checking */
    if (outputDevice < 0 || outputDevice >= pm_descriptor_index)
        err = pmInvalidDeviceId;
    else if (!descriptors[outputDevice].pub.output) 
        err = pmInvalidDeviceId;
    else if (descriptors[outputDevice].pub.opened)
        err = pmInvalidDeviceId;
    if (err != pmNoError) 
        goto error_return;

    /* create portMidi internal data */
    midi = (PmInternal *) pm_alloc(sizeof(PmInternal)); 
    *stream = midi;                 
    if (!midi) {
        err = pmInsufficientMemory;
        goto error_return;
    }
    midi->device_id = outputDevice;
    midi->write_flag = TRUE;
    midi->time_proc = time_proc;
    /* if latency > 0, we need a time reference. If none is provided,
       use PortTime library */
    if (time_proc == NULL && latency != 0) {
        if (!Pt_Started()) 
            Pt_Start(1, 0, 0);
        /* time_get does not take a parameter, so coerce */
        midi->time_proc = (PmTimeProcPtr) Pt_Time;
    }
    midi->time_info = time_info;
    midi->buffer_len = bufferSize;
    midi->queue = NULL; /* unused by output */
    /* if latency zero, output immediate (timestamps ignored) */
    /* if latency < 0, use 0 but don't return an error */
    if (latency < 0) latency = 0;
    midi->latency = latency;
    midi->sysex_in_progress = FALSE;
    midi->sysex_message = 0; /* unused by output */
    midi->sysex_message_count = 0; /* unused by output */
    midi->filters = 0; /* not used for output */
    midi->channel_mask = 0xFFFF;
    midi->sync_time = 0;
    midi->first_message = TRUE;
    midi->dictionary = descriptors[outputDevice].dictionary;
    midi->fill_base = NULL;
    midi->fill_offset_ptr = NULL;
    midi->fill_length = 0;
    descriptors[outputDevice].internalDescriptor = midi;
    /* open system dependent output device */
    err = (*midi->dictionary->open)(midi, outputDriverInfo);
    if (err) {
        *stream = NULL;
        descriptors[outputDevice].internalDescriptor = NULL;
        /* free portMidi data */
        pm_free(midi); 
    } else {
        /* portMidi input open successful */
        descriptors[outputDevice].pub.opened = TRUE;
    }
error_return:
    /* note: system-dependent code must set pm_hosterror and
     * pm_hosterror_text if a pmHostError occurs
     */
    return pm_errmsg(err);
}


PMEXPORT PmError Pm_SetChannelMask(PortMidiStream *stream, int mask)
{
    PmInternal *midi = (PmInternal *) stream;
    PmError err = pmNoError;

    if (midi == NULL)
        err = pmBadPtr;
    else
        midi->channel_mask = mask;

    return pm_errmsg(err);
}


PMEXPORT PmError Pm_SetFilter(PortMidiStream *stream, int32_t filters) {
    PmInternal *midi = (PmInternal *) stream;
    PmError err = pmNoError;

    /* arg checking */
    if (midi == NULL)
        err = pmBadPtr;
    else if (!descriptors[midi->device_id].pub.opened)
        err = pmBadPtr;
    else
        midi->filters = filters;
    return pm_errmsg(err);
}


PMEXPORT PmError Pm_Close( PortMidiStream *stream ) {
    PmInternal *midi = (PmInternal *) stream;
    PmError err = pmNoError;

    pm_hosterror = FALSE;
    /* arg checking */
    if (midi == NULL) /* midi must point to something */
        err = pmBadPtr;
    /* if it is an open device, the device_id will be valid */
    else if (midi->device_id < 0 || midi->device_id >= pm_descriptor_index)
        err = pmBadPtr;
    /* and the device should be in the opened state */
    else if (!descriptors[midi->device_id].pub.opened)
        err = pmBadPtr;
    
    if (err != pmNoError) 
        goto error_return;

    /* close the device */
    err = (*midi->dictionary->close)(midi);
    /* even if an error occurred, continue with cleanup */
    descriptors[midi->device_id].internalDescriptor = NULL;
    descriptors[midi->device_id].pub.opened = FALSE;
    if (midi->queue) Pm_QueueDestroy(midi->queue);
    pm_free(midi); 
error_return:
    /* system dependent code must set pm_hosterror and
     * pm_hosterror_text if a pmHostError occurs.
     */
    return pm_errmsg(err);
}

PmError Pm_Synchronize( PortMidiStream* stream ) {
    PmInternal *midi = (PmInternal *) stream;
    PmError err = pmNoError;
    if (midi == NULL)
        err = pmBadPtr;
    else if (!descriptors[midi->device_id].pub.output)
        err = pmBadPtr;
    else if (!descriptors[midi->device_id].pub.opened)
        err = pmBadPtr;
    else
        midi->first_message = TRUE;
    return err;
}

PMEXPORT PmError Pm_Abort( PortMidiStream* stream ) {
    PmInternal *midi = (PmInternal *) stream;
    PmError err;
    /* arg checking */
    if (midi == NULL)
        err = pmBadPtr;
    else if (!descriptors[midi->device_id].pub.output)
        err = pmBadPtr;
    else if (!descriptors[midi->device_id].pub.opened)
        err = pmBadPtr;
    else
        err = (*midi->dictionary->abort)(midi);

    if (err == pmHostError) {
        midi->dictionary->host_error(midi, pm_hosterror_text, 
                                     PM_HOST_ERROR_MSG_LEN);
        pm_hosterror = TRUE;
    }
    return pm_errmsg(err);
}



/* pm_channel_filtered returns non-zero if the channel mask is blocking the current channel */
#define pm_channel_filtered(status, mask) \
    ((((status) & 0xF0) != 0xF0) && (!(Pm_Channel((status) & 0x0F) & (mask))))


/* The following two functions will checks to see if a MIDI message matches
   the filtering criteria.  Since the sysex routines only want to filter realtime messages,
   we need to have separate routines.
 */


/* pm_realtime_filtered returns non-zero if the filter will kill the current message.
   Note that only realtime messages are checked here.
 */
#define pm_realtime_filtered(status, filters) \
    ((((status) & 0xF0) == 0xF0) && ((1 << ((status) & 0xF)) & (filters)))

/*
    return ((status == MIDI_ACTIVE) && (filters & PM_FILT_ACTIVE))
            ||  ((status == MIDI_CLOCK) && (filters & PM_FILT_CLOCK))
            ||  ((status == MIDI_START) && (filters & PM_FILT_PLAY))
            ||  ((status == MIDI_STOP) && (filters & PM_FILT_PLAY))
            ||  ((status == MIDI_CONTINUE) && (filters & PM_FILT_PLAY))
            ||  ((status == MIDI_F9) && (filters & PM_FILT_F9))
            ||  ((status == MIDI_FD) && (filters & PM_FILT_FD))
            ||  ((status == MIDI_RESET) && (filters & PM_FILT_RESET))
            ||  ((status == MIDI_MTC) && (filters & PM_FILT_MTC))
            ||  ((status == MIDI_SONGPOS) && (filters & PM_FILT_SONG_POSITION))
            ||  ((status == MIDI_SONGSEL) && (filters & PM_FILT_SONG_SELECT))
            ||  ((status == MIDI_TUNE) && (filters & PM_FILT_TUNE));
}*/


/* pm_status_filtered returns non-zero if a filter will kill the current message, based on status.
   Note that sysex and real time are not checked.  It is up to the subsystem (winmm, core midi, alsa)
   to filter sysex, as it is handled more easily and efficiently at that level.
   Realtime message are filtered in pm_realtime_filtered.
 */
#define pm_status_filtered(status, filters) ((1 << (16 + ((status) >> 4))) & (filters))


/*
    return  ((status == MIDI_NOTE_ON) && (filters & PM_FILT_NOTE))
            ||  ((status == MIDI_NOTE_OFF) && (filters & PM_FILT_NOTE))
            ||  ((status == MIDI_CHANNEL_AT) && (filters & PM_FILT_CHANNEL_AFTERTOUCH))
            ||  ((status == MIDI_POLY_AT) && (filters & PM_FILT_POLY_AFTERTOUCH))
            ||  ((status == MIDI_PROGRAM) && (filters & PM_FILT_PROGRAM))
            ||  ((status == MIDI_CONTROL) && (filters & PM_FILT_CONTROL))
            ||  ((status == MIDI_PITCHBEND) && (filters & PM_FILT_PITCHBEND));

}
*/

static void pm_flush_sysex(PmInternal *midi, PmTimestamp timestamp)
{
    PmEvent event;
    
    /* there may be nothing in the buffer */
    if (midi->sysex_message_count == 0) return; /* nothing to flush */
    
    event.message = midi->sysex_message;
    event.timestamp = timestamp;
    /* copied from pm_read_short, avoids filtering */
    if (Pm_Enqueue(midi->queue, &event) == pmBufferOverflow) {
        midi->sysex_in_progress = FALSE;
    }
    midi->sysex_message_count = 0;
    midi->sysex_message = 0;
}


/* pm_read_short and pm_read_bytes
   are the interface between system-dependent MIDI input handlers
   and the system-independent PortMIDI code.
   The input handler MUST obey these rules:
   1) all short input messages must be sent to pm_read_short, which
      enqueues them to a FIFO for the application.
   2) each buffer of sysex bytes should be reported by calling pm_read_bytes
      (which sets midi->sysex_in_progress). After the eox byte, 
      pm_read_bytes will clear sysex_in_progress
 */

/* pm_read_short is the place where all input messages arrive from 
   system-dependent code such as pmwinmm.c. Here, the messages
   are entered into the PortMidi input buffer. 
 */
void pm_read_short(PmInternal *midi, PmEvent *event)
{ 
    int status;
    /* arg checking */
    assert(midi != NULL);
    /* midi filtering is applied here */
    status = Pm_MessageStatus(event->message);
    if (!pm_status_filtered(status, midi->filters)
        && (!is_real_time(status) || 
            !pm_realtime_filtered(status, midi->filters))
        && !pm_channel_filtered(status, midi->channel_mask)) {
        /* if sysex is in progress and we get a status byte, it had
           better be a realtime message or the starting SYSEX byte;
           otherwise, we exit the sysex_in_progress state
         */
        if (midi->sysex_in_progress && (status & MIDI_STATUS_MASK)) {
            /* two choices: real-time or not. If it's real-time, then
             * this should be delivered as a sysex byte because it is
             * embedded in a sysex message
             */
            if (is_real_time(status)) {
                midi->sysex_message |= 
                        (status << (8 * midi->sysex_message_count++));
                if (midi->sysex_message_count == 4) {
                    pm_flush_sysex(midi, event->timestamp);
                }
            } else { /* otherwise, it's not real-time. This interrupts
                      * a sysex message in progress */
                midi->sysex_in_progress = FALSE;
            }
        } else if (Pm_Enqueue(midi->queue, event) == pmBufferOverflow) {
            midi->sysex_in_progress = FALSE;
        }
    }
}

/* pm_read_bytes -- read one (partial) sysex msg from MIDI data */
/*
 * returns how many bytes processed
 */
unsigned int pm_read_bytes(PmInternal *midi, const unsigned char *data, 
                    int len, PmTimestamp timestamp)
{
    int i = 0; /* index into data, must not be unsigned (!) */
    PmEvent event;
    event.timestamp = timestamp;
    assert(midi);
    /* note that since buffers may not have multiples of 4 bytes,
     * pm_read_bytes may be called in the middle of an outgoing
     * 4-byte PortMidi message. sysex_in_progress indicates that
     * a sysex has been sent but no eox.
     */
    if (len == 0) return 0; /* sanity check */
    if (!midi->sysex_in_progress) {
        while (i < len) { /* process all data */
            unsigned char byte = data[i++];
            if (byte == MIDI_SYSEX &&
                !pm_realtime_filtered(byte, midi->filters)) {
                midi->sysex_in_progress = TRUE;
                i--; /* back up so code below will get SYSEX byte */
                break; /* continue looping below to process msg */
            } else if (byte == MIDI_EOX) {
                midi->sysex_in_progress = FALSE;
                return i; /* done with one message */
            } else if (byte & MIDI_STATUS_MASK) {
                /* We're getting MIDI but no sysex in progress.
                 * Either the SYSEX status byte was dropped or
                 * the message was filtered. Drop the data, but
                 * send any embedded realtime bytes.
                 */
                /* assume that this is a real-time message:
                 * it is an error to pass non-real-time messages
                 * to pm_read_bytes
                 */
                event.message = byte;
                pm_read_short(midi, &event);
            }
        } /* all bytes in the buffer are processed */
    }
    /* Now, i<len implies sysex_in_progress. If sysex_in_progress
     * becomes false in the loop, there must have been an overflow
     * and we can just drop all remaining bytes 
     */
    while (i < len && midi->sysex_in_progress) {
        if (midi->sysex_message_count == 0 && i <= len - 4 &&
            ((event.message = (((PmMessage) data[i]) | 
                             (((PmMessage) data[i+1]) << 8) |
                             (((PmMessage) data[i+2]) << 16) |
                             (((PmMessage) data[i+3]) << 24))) &
             0x80808080) == 0) { /* all data, no status */ 
            if (Pm_Enqueue(midi->queue, &event) == pmBufferOverflow) {
                midi->sysex_in_progress = FALSE;
            }
            i += 4;
        } else {
            while (i < len) {
                /* send one byte at a time */
                unsigned char byte = data[i++];
                if (is_real_time(byte) && 
                    pm_realtime_filtered(byte, midi->filters)) {
                    continue; /* real-time data is filtered, so omit */
                }
                midi->sysex_message |= 
                    (byte << (8 * midi->sysex_message_count++));
                if (byte == MIDI_EOX) {
                    midi->sysex_in_progress = FALSE;
                    pm_flush_sysex(midi, event.timestamp);
                    return i;
                } else if (midi->sysex_message_count == 4) {
                    pm_flush_sysex(midi, event.timestamp);
                    /* after handling at least one non-data byte
                     * and reaching a 4-byte message boundary,
                     * resume trying to send 4 at a time in outer loop
                     */
                    break;
                }
            }
        }
    }
    return i;
}


