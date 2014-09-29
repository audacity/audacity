/* pmwinmm.c -- system specific definitions */

#ifdef _MSC_VER
 #pragma warning(disable: 4133) // stop warnings about implicit typecasts
#endif

#ifndef _WIN32_WINNT
    /* without this define, InitializeCriticalSectionAndSpinCount is 
     * undefined. This version level means "Windows 2000 and higher" 
     */
    #define _WIN32_WINNT 0x0500
#endif

#include "windows.h"
#include "mmsystem.h"
#include "portmidi.h"
#include "pmutil.h"
#include "pminternal.h"
#include "pmwinmm.h"
#include <string.h>
#include "porttime.h"

/* asserts used to verify portMidi code logic is sound; later may want
    something more graceful */
#include <assert.h>
#ifdef DEBUG
/* this printf stuff really important for debugging client app w/host errors.
    probably want to do something else besides read/write from/to console
    for portability, however */
#define STRING_MAX 80
#include "stdio.h"
#endif

#define streql(x, y) (strcmp(x, y) == 0)

#define MIDI_SYSEX      0xf0
#define MIDI_EOX        0xf7

/* callback routines */
static void CALLBACK winmm_in_callback(HMIDIIN hMidiIn,
                                       WORD wMsg, DWORD dwInstance, 
                                       DWORD dwParam1, DWORD dwParam2);
static void CALLBACK winmm_streamout_callback(HMIDIOUT hmo, UINT wMsg,
                                              DWORD dwInstance, DWORD dwParam1, 
                                              DWORD dwParam2);
#ifdef USE_SYSEX_BUFFERS
static void CALLBACK winmm_out_callback(HMIDIOUT hmo, UINT wMsg,
                                        DWORD dwInstance, DWORD dwParam1, 
                                        DWORD dwParam2);
#endif

extern pm_fns_node pm_winmm_in_dictionary;
extern pm_fns_node pm_winmm_out_dictionary;

static void winmm_out_delete(PmInternal *midi); /* forward reference */

/*
A note about buffers: WinMM seems to hold onto buffers longer than
one would expect, e.g. when I tried using 2 small buffers to send
long sysex messages, at some point WinMM held both buffers. This problem
was fixed by making buffers bigger. Therefore, it seems that there should 
be enough buffer space to hold a whole sysex message. 

The bufferSize passed into Pm_OpenInput (passed into here as buffer_len)
will be used to estimate the largest sysex message (= buffer_len * 4 bytes).
Call that the max_sysex_len = buffer_len * 4.

For simple midi output (latency == 0), allocate 3 buffers, each with half
the size of max_sysex_len, but each at least 256 bytes.

For stream output, there will already be enough space in very short
buffers, so use them, but make sure there are at least 16.

For input, use many small buffers rather than 2 large ones so that when 
there are short sysex messages arriving frequently (as in control surfaces)
there will be more free buffers to fill. Use max_sysex_len / 64 buffers,
but at least 16, of size 64 bytes each.

The following constants help to represent these design parameters:
*/
#define NUM_SIMPLE_SYSEX_BUFFERS 3
#define MIN_SIMPLE_SYSEX_LEN 256

#define MIN_STREAM_BUFFERS 16
#define STREAM_BUFFER_LEN 24

#define INPUT_SYSEX_LEN 64
#define MIN_INPUT_BUFFERS 16

/* if we run out of space for output (assume this is due to a sysex msg,
   expand by up to NUM_EXPANSION_BUFFERS in increments of EXPANSION_BUFFER_LEN
 */
#define NUM_EXPANSION_BUFFERS 128
#define EXPANSION_BUFFER_LEN 1024

/* A sysex buffer has 3 DWORDS as a header plus the actual message size */
#define MIDIHDR_SYSEX_BUFFER_LENGTH(x) ((x) + sizeof(long)*3)
/* A MIDIHDR with a sysex message is the buffer length plus the header size */
#define MIDIHDR_SYSEX_SIZE(x) (MIDIHDR_SYSEX_BUFFER_LENGTH(x) + sizeof(MIDIHDR))
#ifdef USE_SYSEX_BUFFERS
/* Size of a MIDIHDR with a buffer contaning multiple MIDIEVENT structures */
#define MIDIHDR_SIZE(x) ((x) + sizeof(MIDIHDR))
#endif

/*
==============================================================================
win32 mmedia system specific structure passed to midi callbacks
==============================================================================
*/

/* global winmm device info */
MIDIINCAPS *midi_in_caps = NULL;
MIDIINCAPS midi_in_mapper_caps;
UINT midi_num_inputs = 0;
MIDIOUTCAPS *midi_out_caps = NULL;
MIDIOUTCAPS midi_out_mapper_caps;
UINT midi_num_outputs = 0;

/* per device info */
typedef struct midiwinmm_struct {
    union {
        HMIDISTRM stream;   /* windows handle for stream */
        HMIDIOUT out;       /* windows handle for out calls */
        HMIDIIN in;         /* windows handle for in calls */
    } handle;

    /* midi output messages are sent in these buffers, which are allocated
     * in a round-robin fashion, using next_buffer as an index
     */
    LPMIDIHDR *buffers;     /* pool of buffers for midi in or out data */
    int max_buffers;        /* length of buffers array */
    int buffers_expanded;   /* buffers array expanded for extra msgs? */
    int num_buffers;        /* how many buffers allocated in buffers array */
    int next_buffer;        /* index of next buffer to send */
    HANDLE buffer_signal;   /* used to wait for buffer to become free */
#ifdef USE_SYSEX_BUFFERS
    /* sysex buffers will be allocated only when
     * a sysex message is sent. The size of the buffer is fixed.
     */
    LPMIDIHDR sysex_buffers[NUM_SYSEX_BUFFERS]; /* pool of buffers for sysex data */
    int next_sysex_buffer;      /* index of next sysexbuffer to send */
#endif
    unsigned long last_time;    /* last output time */
    int first_message;          /* flag: treat first message differently */
    int sysex_mode;             /* middle of sending sysex */
    unsigned long sysex_word;   /* accumulate data when receiving sysex */
    unsigned int sysex_byte_count; /* count how many received */
    LPMIDIHDR hdr;              /* the message accumulating sysex to send */
    unsigned long sync_time;    /* when did we last determine delta? */
    long delta;                 /* difference between stream time and
                                       real time */
    int error;                  /* host error from doing port midi call */
    CRITICAL_SECTION lock;      /* prevents reentrant callbacks (input only) */
} midiwinmm_node, *midiwinmm_type;


/*
=============================================================================
general MIDI device queries
=============================================================================
*/
static void pm_winmm_general_inputs()
{
    UINT i;
    WORD wRtn;
    midi_num_inputs = midiInGetNumDevs();
    midi_in_caps = (MIDIINCAPS *) pm_alloc(sizeof(MIDIINCAPS) * 
                                           midi_num_inputs);
    if (midi_in_caps == NULL) {
        /* if you can't open a particular system-level midi interface
         * (such as winmm), we just consider that system or API to be
         * unavailable and move on without reporting an error.
         */
        return;
    }

    for (i = 0; i < midi_num_inputs; i++) {
        wRtn = midiInGetDevCaps(i, (LPMIDIINCAPS) & midi_in_caps[i],
                                sizeof(MIDIINCAPS));
        if (wRtn == MMSYSERR_NOERROR) {
            /* ignore errors here -- if pm_descriptor_max is exceeded, some
               devices will not be accessible. */
            pm_add_device("MMSystem", midi_in_caps[i].szPname, TRUE,
                          (void *) i, &pm_winmm_in_dictionary);
        }
    }
}


static void pm_winmm_mapper_input()
{
    WORD wRtn;
    /* Note: if MIDIMAPPER opened as input (documentation implies you
        can, but current system fails to retrieve input mapper
        capabilities) then you still should retrieve some formof
        setup info. */
    wRtn = midiInGetDevCaps((UINT) MIDIMAPPER,
                            (LPMIDIINCAPS) & midi_in_mapper_caps, 
                            sizeof(MIDIINCAPS));
    if (wRtn == MMSYSERR_NOERROR) {
        pm_add_device("MMSystem", midi_in_mapper_caps.szPname, TRUE,
                      (void *) MIDIMAPPER, &pm_winmm_in_dictionary);
    }
}


static void pm_winmm_general_outputs()
{
    UINT i;
    DWORD wRtn;
    midi_num_outputs = midiOutGetNumDevs();
    midi_out_caps = pm_alloc( sizeof(MIDIOUTCAPS) * midi_num_outputs );

    if (midi_out_caps == NULL) {
        /* no error is reported -- see pm_winmm_general_inputs */
        return ;
    }

    for (i = 0; i < midi_num_outputs; i++) {
        wRtn = midiOutGetDevCaps(i, (LPMIDIOUTCAPS) & midi_out_caps[i],
                                 sizeof(MIDIOUTCAPS));
        if (wRtn == MMSYSERR_NOERROR) {
            pm_add_device("MMSystem", midi_out_caps[i].szPname, FALSE,
                          (void *) i, &pm_winmm_out_dictionary);
        }
    }
}


static void pm_winmm_mapper_output()
{
    WORD wRtn;
    /* Note: if MIDIMAPPER opened as output (pseudo MIDI device
        maps device independent messages into device dependant ones,
        via NT midimapper program) you still should get some setup info */
    wRtn = midiOutGetDevCaps((UINT) MIDIMAPPER, (LPMIDIOUTCAPS)
                             & midi_out_mapper_caps, sizeof(MIDIOUTCAPS));
    if (wRtn == MMSYSERR_NOERROR) {
        pm_add_device("MMSystem", midi_out_mapper_caps.szPname, FALSE,
                      (void *) MIDIMAPPER, &pm_winmm_out_dictionary);
    }
}


/*
=========================================================================================
host error handling
=========================================================================================
*/
static unsigned int winmm_has_host_error(PmInternal * midi)
{
    midiwinmm_type m = (midiwinmm_type)midi->descriptor;
    return m->error;
}


/* str_copy_len -- like strcat, but won't overrun the destination string */
/*
 * returns length of resulting string
 */
static int str_copy_len(char *dst, char *src, int len)
{
    strncpy(dst, src, len);
    /* just in case suffex is greater then len, terminate with zero */
    dst[len - 1] = 0;
    return strlen(dst);
}


static void winmm_get_host_error(PmInternal * midi, char * msg, UINT len)
{
    /* precondition: midi != NULL */
    midiwinmm_node * m = (midiwinmm_node *) midi->descriptor;
    char *hdr1 = "Host error: ";
    char *hdr2 = "Host callback error: ";

    msg[0] = 0; /* initialize result string to empty */

    if (descriptors[midi->device_id].pub.input) {
        /* input and output use different winmm API calls */
        if (m) { /* make sure there is an open device to examine */
            if (m->error != MMSYSERR_NOERROR) {
                int n = str_copy_len(msg, hdr1, len);
                /* read and record host error */
                int err = midiInGetErrorText(m->error, msg + n, len - n);
                assert(err == MMSYSERR_NOERROR);
                m->error = MMSYSERR_NOERROR;
            }
        }
    } else { /* output port */
        if (m) {
            if (m->error != MMSYSERR_NOERROR) {
                int n = str_copy_len(msg, hdr1, len);
                int err = midiOutGetErrorText(m->error, msg + n, len - n);
                assert(err == MMSYSERR_NOERROR);
                m->error = MMSYSERR_NOERROR;
            }
        }
    }
}


/*
=============================================================================
buffer handling
=============================================================================
*/
static MIDIHDR *allocate_buffer(long data_size)
{
    LPMIDIHDR hdr = (LPMIDIHDR) pm_alloc(MIDIHDR_SYSEX_SIZE(data_size));
    MIDIEVENT *evt;
    if (!hdr) return NULL;
    evt = (MIDIEVENT *) (hdr + 1); /* place MIDIEVENT after header */
    hdr->lpData = (LPSTR) evt;
    hdr->dwBufferLength = MIDIHDR_SYSEX_BUFFER_LENGTH(data_size);
    hdr->dwBytesRecorded = 0;
    hdr->dwFlags = 0;
    hdr->dwUser = hdr->dwBufferLength;
    return hdr;
}

#ifdef USE_SYSEX_BUFFERS
static MIDIHDR *allocate_sysex_buffer(long data_size)
{
    /* we're actually allocating more than data_size because the buffer 
     * will include the MIDIEVENT header in addition to the data 
     */
    LPMIDIHDR hdr = (LPMIDIHDR) pm_alloc(MIDIHDR_SYSEX_SIZE(data_size));
    MIDIEVENT *evt;
    if (!hdr) return NULL;
    evt = (MIDIEVENT *) (hdr + 1); /* place MIDIEVENT after header */
    hdr->lpData = (LPSTR) evt;
    hdr->dwFlags = 0;
    hdr->dwUser = 0;
    return hdr;
}
#endif

static PmError allocate_buffers(midiwinmm_type m, long data_size, long count)
{
    int i;
    /* buffers is an array of count pointers to MIDIHDR/MIDIEVENT struct */
    m->num_buffers = 0; /* in case no memory can be allocated */
    m->buffers = (LPMIDIHDR *) pm_alloc(sizeof(LPMIDIHDR) * count);
    if (!m->buffers) return pmInsufficientMemory;
    m->max_buffers = count;
    for (i = 0; i < count; i++) {
        LPMIDIHDR hdr = allocate_buffer(data_size);
        if (!hdr) { /* free everything allocated so far and return */
            for (i = i - 1; i >= 0; i--) pm_free(m->buffers[i]);
            pm_free(m->buffers);
            m->max_buffers = 0;
            return pmInsufficientMemory;
        }
        m->buffers[i] = hdr; /* this may be NULL if allocation fails */
    }
    m->num_buffers = count;
    return pmNoError;
}

#ifdef USE_SYSEX_BUFFERS
static PmError allocate_sysex_buffers(midiwinmm_type m, long data_size)
{
    PmError rslt = pmNoError;
    /* sysex_buffers is an array of count pointers to MIDIHDR/MIDIEVENT struct */
    int i;
    for (i = 0; i < NUM_SYSEX_BUFFERS; i++) {
        LPMIDIHDR hdr = allocate_sysex_buffer(data_size);

        if (!hdr) rslt = pmInsufficientMemory;
        m->sysex_buffers[i] = hdr; /* this may be NULL if allocation fails */
        hdr->dwFlags = 0; /* mark as free */
    }
    return rslt;
}
#endif

#ifdef USE_SYSEX_BUFFERS
static LPMIDIHDR get_free_sysex_buffer(PmInternal *midi)
{
    LPMIDIHDR r = NULL;
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    if (!m->sysex_buffers[0]) {
        if (allocate_sysex_buffers(m, SYSEX_BYTES_PER_BUFFER)) {
            return NULL;
        }
    }
    /* busy wait until we find a free buffer */
    while (TRUE) {
        int i;
        for (i = 0; i < NUM_SYSEX_BUFFERS; i++) {
            /* cycle through buffers, modulo NUM_SYSEX_BUFFERS */
            m->next_sysex_buffer++;
            if (m->next_sysex_buffer >= NUM_SYSEX_BUFFERS) m->next_sysex_buffer = 0;
            r = m->sysex_buffers[m->next_sysex_buffer];
            if ((r->dwFlags & MHDR_PREPARED) == 0) goto found_sysex_buffer;
        }
        /* after scanning every buffer and not finding anything, block */
        if (WaitForSingleObject(m->buffer_signal, 1000) == WAIT_TIMEOUT) {
#ifdef DEBUG
            printf("PortMidi warning: get_free_sysex_buffer() wait timed out after 1000ms\n");
#endif
        }
    }
found_sysex_buffer:
    r->dwBytesRecorded = 0;
    r->dwBufferLength = 0; /* changed to correct value later */
    return r;
}
#endif

static LPMIDIHDR get_free_output_buffer(PmInternal *midi)
{
    LPMIDIHDR r = NULL;
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    while (TRUE) {
        int i;
        for (i = 0; i < m->num_buffers; i++) {
            /* cycle through buffers, modulo m->num_buffers */
            m->next_buffer++;
            if (m->next_buffer >= m->num_buffers) m->next_buffer = 0;
            r = m->buffers[m->next_buffer];
            if ((r->dwFlags & MHDR_PREPARED) == 0) goto found_buffer;
        }
        /* after scanning every buffer and not finding anything, block */
        if (WaitForSingleObject(m->buffer_signal, 1000) == WAIT_TIMEOUT) {
#ifdef DEBUG
            printf("PortMidi warning: get_free_output_buffer() wait timed out after 1000ms\n");
#endif
            /* if we're trying to send a sysex message, maybe the 
             * message is too big and we need more message buffers.
             * Expand the buffer pool by 128KB using 1024-byte buffers.
             */
            /* first, expand the buffers array if necessary */
            if (!m->buffers_expanded) {
                LPMIDIHDR *new_buffers = (LPMIDIHDR *) pm_alloc(
                        (m->num_buffers + NUM_EXPANSION_BUFFERS) * 
                        sizeof(LPMIDIHDR));
                /* if no memory, we could return a no-memory error, but user
                 * probably will be unprepared to deal with it. Maybe the
                 * MIDI driver is temporarily hung so we should just wait.
                 * I don't know the right answer, but waiting is easier.
                 */
                if (!new_buffers) continue;
                /* copy buffers to new_buffers and replace buffers */
                memcpy(new_buffers, m->buffers, 
                       m->num_buffers * sizeof(LPMIDIHDR));
                pm_free(m->buffers);
                m->buffers = new_buffers;
                m->max_buffers = m->num_buffers + NUM_EXPANSION_BUFFERS;
                m->buffers_expanded = TRUE;
            }
            /* next, add one buffer and return it */
            if (m->num_buffers < m->max_buffers) {
                r = allocate_buffer(EXPANSION_BUFFER_LEN);
                /* again, if there's no memory, we may not really be 
                 * dead -- maybe the system is temporarily hung and
                 * we can just wait longer for a message buffer */
                if (!r) continue;
                m->buffers[m->num_buffers++] = r;
                goto found_buffer; /* break out of 2 loops */
            }
            /* else, we've allocated all NUM_EXPANSION_BUFFERS buffers,
             * and we have no free buffers to send. We'll just keep
             * polling to see if any buffers show up.
             */
        }
    }
found_buffer:
    r->dwBytesRecorded = 0;
    /* actual buffer length is saved in dwUser field */
    r->dwBufferLength = (DWORD) r->dwUser;
    return r;
}

#ifdef EXPANDING_SYSEX_BUFFERS
note: this is not working code, but might be useful if you want
      to grow sysex buffers.
static PmError resize_sysex_buffer(PmInternal *midi, long old_size, long new_size)
{
    LPMIDIHDR big;
    int i;
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    /* buffer must be smaller than 64k, but be also a multiple of 4 */
    if (new_size > 65520) {
        if (old_size >= 65520)
            return pmBufferMaxSize;
        else
            new_size = 65520;
    }
    /* allocate a bigger message  */
    big = allocate_sysex_buffer(new_size);
    /* printf("expand to %d bytes\n", new_size);*/
    if (!big) return pmInsufficientMemory;
    m->error = midiOutPrepareHeader(m->handle.out, big, sizeof(MIDIHDR));
    if (m->error) {
        pm_free(big);
        return pmHostError;
    }
    /* make sure we're not going to overwrite any memory */
    assert(old_size <= new_size);
    memcpy(big->lpData, m->hdr->lpData, old_size);
    /* keep track of how many sysex bytes are in message so far */
    big->dwBytesRecorded = m->hdr->dwBytesRecorded;
    big->dwBufferLength = new_size;
    /* find which buffer this was, and replace it */
    for (i = 0; i < NUM_SYSEX_BUFFERS; i++) {
        if (m->sysex_buffers[i] == m->hdr) {
            m->sysex_buffers[i] = big;
            m->sysex_buffer_size[i] = new_size;
            pm_free(m->hdr);
            m->hdr = big;
            break;
        }
    }
    assert(i != NUM_SYSEX_BUFFERS);

    return pmNoError;
}
#endif

/*
=========================================================================================
begin midi input implementation
=========================================================================================
*/


static PmError allocate_input_buffer(HMIDIIN h, long buffer_len)
{
    LPMIDIHDR hdr = allocate_buffer(buffer_len);
    if (!hdr) return pmInsufficientMemory;
    pm_hosterror = midiInPrepareHeader(h, hdr, sizeof(MIDIHDR));
    if (pm_hosterror) {
        pm_free(hdr);
        return pm_hosterror;
    }
    pm_hosterror = midiInAddBuffer(h, hdr, sizeof(MIDIHDR));
    return pm_hosterror;
}


static PmError winmm_in_open(PmInternal *midi, void *driverInfo)
{
    DWORD dwDevice;
    int i = midi->device_id;
    int max_sysex_len = midi->buffer_len * 4;
    int num_input_buffers = max_sysex_len / INPUT_SYSEX_LEN;
    midiwinmm_type m;

    dwDevice = (DWORD) descriptors[i].descriptor;

    /* create system dependent device data */
    m = (midiwinmm_type) pm_alloc(sizeof(midiwinmm_node)); /* create */
    midi->descriptor = m;
    if (!m) goto no_memory;
    m->handle.in = NULL;
    m->buffers = NULL; /* not used for input */
    m->num_buffers = 0; /* not used for input */
    m->max_buffers = FALSE; /* not used for input */
    m->buffers_expanded = 0; /* not used for input */
    m->next_buffer = 0; /* not used for input */
    m->buffer_signal = 0; /* not used for input */
#ifdef USE_SYSEX_BUFFERS
    for (i = 0; i < NUM_SYSEX_BUFFERS; i++) 
        m->sysex_buffers[i] = NULL; /* not used for input */
    m->next_sysex_buffer = 0; /* not used for input */
#endif
    m->last_time = 0;
    m->first_message = TRUE; /* not used for input */
    m->sysex_mode = FALSE;
    m->sysex_word = 0;
    m->sysex_byte_count = 0;
    m->hdr = NULL; /* not used for input */
    m->sync_time = 0;
    m->delta = 0;
    m->error = MMSYSERR_NOERROR;
    /* 4000 is based on Windows documentation -- that's the value used in the
       memory manager. It's small enough that it should not hurt performance even
       if it's not optimal.
     */
    InitializeCriticalSectionAndSpinCount(&m->lock, 4000);
    /* open device */
    pm_hosterror = midiInOpen(
	    &(m->handle.in),  /* input device handle */
	    dwDevice,  /* device ID */
	    (DWORD_PTR) winmm_in_callback,  /* callback address */
	    (DWORD_PTR) midi,  /* callback instance data */
	    CALLBACK_FUNCTION); /* callback is a procedure */
    if (pm_hosterror) goto free_descriptor;

    if (num_input_buffers < MIN_INPUT_BUFFERS)
        num_input_buffers = MIN_INPUT_BUFFERS;
    for (i = 0; i < num_input_buffers; i++) {
        if (allocate_input_buffer(m->handle.in, INPUT_SYSEX_LEN)) {
            /* either pm_hosterror was set, or the proper return code
               is pmInsufficientMemory */
            goto close_device;
        }
    }
    /* start device */
    pm_hosterror = midiInStart(m->handle.in);
    if (pm_hosterror) goto reset_device;
    return pmNoError;

    /* undo steps leading up to the detected error */
reset_device:
    /* ignore return code (we already have an error to report) */
    midiInReset(m->handle.in);
close_device:
    midiInClose(m->handle.in); /* ignore return code */
free_descriptor:
    midi->descriptor = NULL;
    pm_free(m);
no_memory:
    if (pm_hosterror) {
        int err = midiInGetErrorText(pm_hosterror, (char *) pm_hosterror_text,
                                     PM_HOST_ERROR_MSG_LEN);
        assert(err == MMSYSERR_NOERROR);
        return pmHostError;
    }
    /* if !pm_hosterror, then the error must be pmInsufficientMemory */
    return pmInsufficientMemory;
    /* note: if we return an error code, the device will be
       closed and memory will be freed. It's up to the caller
       to free the parameter midi */
}

static PmError winmm_in_poll(PmInternal *midi) {
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    return m->error;
}



/* winmm_in_close -- close an open midi input device */
/*
 * assume midi is non-null (checked by caller)
 */
static PmError winmm_in_close(PmInternal *midi)
{
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    if (!m) return pmBadPtr;
    /* device to close */
    if (pm_hosterror = midiInStop(m->handle.in)) {
        midiInReset(m->handle.in); /* try to reset and close port */
        midiInClose(m->handle.in);
    } else if (pm_hosterror = midiInReset(m->handle.in)) {
        midiInClose(m->handle.in); /* best effort to close midi port */
    } else {
        pm_hosterror = midiInClose(m->handle.in);
    }
    midi->descriptor = NULL;
    DeleteCriticalSection(&m->lock);
    pm_free(m); /* delete */
    if (pm_hosterror) {
        int err = midiInGetErrorText(pm_hosterror, (char *) pm_hosterror_text,
                                     PM_HOST_ERROR_MSG_LEN);
        assert(err == MMSYSERR_NOERROR);
        return pmHostError;
    }
    return pmNoError;
}


/* Callback function executed via midiInput SW interrupt (via midiInOpen). */
static void FAR PASCAL winmm_in_callback(
    HMIDIIN hMidiIn,    /* midiInput device Handle */
    WORD wMsg,          /* midi msg */
    DWORD dwInstance,   /* application data */
    DWORD dwParam1,     /* MIDI data */
    DWORD dwParam2)    /* device timestamp (wrt most recent midiInStart) */
{
    static int entry = 0;
    PmInternal *midi = (PmInternal *) dwInstance;
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;

    /* NOTE: we do not just EnterCriticalSection() here because an
     * MIM_CLOSE message arrives when the port is closed, but then
     * the m->lock has been destroyed.
     */

    switch (wMsg) {
    case MIM_DATA: {
        /* if this callback is reentered with data, we're in trouble. 
         * It's hard to imagine that Microsoft would allow callbacks 
         * to be reentrant -- isn't the model that this is like a 
         * hardware interrupt? -- but I've seen reentrant behavior 
         * using a debugger, so it happens.
         */
        long new_driver_time;
        EnterCriticalSection(&m->lock);

        /* dwParam1 is MIDI data received, packed into DWORD w/ 1st byte of
                message LOB;
           dwParam2 is time message received by input device driver, specified
            in [ms] from when midiInStart called.
           each message is expanded to include the status byte */

        new_driver_time = dwParam2;

        if ((dwParam1 & 0x80) == 0) {
            /* not a status byte -- ignore it. This happened running the
               sysex.c test under Win2K with MidiMan USB 1x1 interface,
               but I can't reproduce it. -RBD
             */
            /* printf("non-status byte found\n"); */
        } else { /* data to process */
            PmEvent event;
            if (midi->time_proc)
                dwParam2 = (*midi->time_proc)(midi->time_info);
            event.timestamp = dwParam2;
            event.message = dwParam1;
            pm_read_short(midi, &event);
        }
        LeaveCriticalSection(&m->lock);
        break;
    }
    case MIM_LONGDATA: {
        MIDIHDR *lpMidiHdr = (MIDIHDR *) dwParam1;
        unsigned char *data = (unsigned char *) lpMidiHdr->lpData;
        unsigned int processed = 0;
        int remaining = lpMidiHdr->dwBytesRecorded;

        EnterCriticalSection(&m->lock);
        /* printf("midi_in_callback -- lpMidiHdr %x, %d bytes, %2x...\n", 
                lpMidiHdr, lpMidiHdr->dwBytesRecorded, *data); */
        if (midi->time_proc)
            dwParam2 = (*midi->time_proc)(midi->time_info);
        /* can there be more than one message in one buffer? */
        /* assume yes and iterate through them */
        while (remaining > 0) {
            unsigned int amt = pm_read_bytes(midi, data + processed, 
                                             remaining, dwParam2);
            remaining -= amt;
            processed += amt;
        }

        /* when a device is closed, the pending MIM_LONGDATA buffers are
           returned to this callback with dwBytesRecorded == 0. In this
           case, we do not want to send them back to the interface (if
           we do, the interface will not close, and Windows OS may hang). */
        if (lpMidiHdr->dwBytesRecorded > 0) {
            MMRESULT rslt;
            lpMidiHdr->dwBytesRecorded = 0;
            lpMidiHdr->dwFlags = 0;
			
            /* note: no error checking -- can this actually fail? */
            rslt = midiInPrepareHeader(hMidiIn, lpMidiHdr, sizeof(MIDIHDR));
            assert(rslt == MMSYSERR_NOERROR);
            /* note: I don't think this can fail except possibly for
             * MMSYSERR_NOMEM, but the pain of reporting this
             * unlikely but probably catastrophic error does not seem
             * worth it.
             */
            rslt = midiInAddBuffer(hMidiIn, lpMidiHdr, sizeof(MIDIHDR));
            assert(rslt == MMSYSERR_NOERROR);
            LeaveCriticalSection(&m->lock);
        } else {
            midiInUnprepareHeader(hMidiIn,lpMidiHdr,sizeof(MIDIHDR));
            LeaveCriticalSection(&m->lock);
            pm_free(lpMidiHdr);
        }
        break;
    }
    case MIM_OPEN:
        break;
    case MIM_CLOSE:
        break;
    case MIM_ERROR:
        /* printf("MIM_ERROR\n"); */
        break;
    case MIM_LONGERROR:
        /* printf("MIM_LONGERROR\n"); */
        break;
    default:
        break;
    }
}

/*
=========================================================================================
begin midi output implementation
=========================================================================================
*/

/* begin helper routines used by midiOutStream interface */

/* add_to_buffer -- adds timestamped short msg to buffer, returns fullp */
static int add_to_buffer(midiwinmm_type m, LPMIDIHDR hdr,
                         unsigned long delta, unsigned long msg)
{
    unsigned long *ptr = (unsigned long *)
                         (hdr->lpData + hdr->dwBytesRecorded);
    *ptr++ = delta; /* dwDeltaTime */
    *ptr++ = 0;     /* dwStream */
    *ptr++ = msg;   /* dwEvent */
    hdr->dwBytesRecorded += 3 * sizeof(long);
    /* if the addition of three more words (a message) would extend beyond
       the buffer length, then return TRUE (full)
     */
    return hdr->dwBytesRecorded + 3 * sizeof(long) > hdr->dwBufferLength;
}


static PmTimestamp pm_time_get(midiwinmm_type m)
{
    MMTIME mmtime;
    MMRESULT wRtn;
    mmtime.wType = TIME_TICKS;
    mmtime.u.ticks = 0;
    wRtn = midiStreamPosition(m->handle.stream, &mmtime, sizeof(mmtime));
    assert(wRtn == MMSYSERR_NOERROR);
    return mmtime.u.ticks;
}


/* end helper routines used by midiOutStream interface */


static PmError winmm_out_open(PmInternal *midi, void *driverInfo)
{
    DWORD dwDevice;
    int i = midi->device_id;
    midiwinmm_type m;
    MIDIPROPTEMPO propdata;
    MIDIPROPTIMEDIV divdata;
    int max_sysex_len = midi->buffer_len * 4;
    int output_buffer_len;
    int num_buffers;
    dwDevice = (DWORD) descriptors[i].descriptor;

    /* create system dependent device data */
    m = (midiwinmm_type) pm_alloc(sizeof(midiwinmm_node)); /* create */
    midi->descriptor = m;
    if (!m) goto no_memory;
    m->handle.out = NULL;
    m->buffers = NULL;
    m->num_buffers = 0;
    m->max_buffers = 0;
    m->buffers_expanded = FALSE;
    m->next_buffer = 0;
#ifdef USE_SYSEX_BUFFERS
    m->sysex_buffers[0] = NULL;
    m->sysex_buffers[1] = NULL;
    m->next_sysex_buffer = 0;
#endif
    m->last_time = 0;
    m->first_message = TRUE; /* we treat first message as special case */
    m->sysex_mode = FALSE;
    m->sysex_word = 0;
    m->sysex_byte_count = 0;
    m->hdr = NULL;
    m->sync_time = 0;
    m->delta = 0;
    m->error = MMSYSERR_NOERROR;

    /* create a signal */
    m->buffer_signal = CreateEvent(NULL, FALSE, FALSE, NULL);

    /* this should only fail when there are very serious problems */
    assert(m->buffer_signal);

    /* open device */
    if (midi->latency == 0) {
        /* use simple midi out calls */
        pm_hosterror = midiOutOpen(
                (LPHMIDIOUT) & m->handle.out,  /* device Handle */
		dwDevice,  /* device ID  */
		/* note: same callback fn as for StreamOpen: */
		(DWORD_PTR) winmm_streamout_callback, /* callback fn */
		(DWORD_PTR) midi,  /* callback instance data */
		CALLBACK_FUNCTION); /* callback type */
    } else {
        /* use stream-based midi output (schedulable in future) */
        pm_hosterror = midiStreamOpen(
	        &m->handle.stream,  /* device Handle */
		(LPUINT) & dwDevice,  /* device ID pointer */
		1,  /* reserved, must be 1 */
		(DWORD_PTR) winmm_streamout_callback,
		(DWORD_PTR) midi,  /* callback instance data */
		CALLBACK_FUNCTION);
    }
    if (pm_hosterror != MMSYSERR_NOERROR) {
        goto free_descriptor;
    }

    if (midi->latency == 0) {
        num_buffers = NUM_SIMPLE_SYSEX_BUFFERS;
        output_buffer_len = max_sysex_len / num_buffers;
        if (output_buffer_len < MIN_SIMPLE_SYSEX_LEN)
            output_buffer_len = MIN_SIMPLE_SYSEX_LEN;
    } else {
        long dur = 0;
        num_buffers = max(midi->buffer_len, midi->latency / 2);
        if (num_buffers < MIN_STREAM_BUFFERS)
            num_buffers = MIN_STREAM_BUFFERS;
        output_buffer_len = STREAM_BUFFER_LEN;

        propdata.cbStruct = sizeof(MIDIPROPTEMPO);
        propdata.dwTempo = 480000; /* microseconds per quarter */
        pm_hosterror = midiStreamProperty(m->handle.stream,
                                          (LPBYTE) & propdata,
                                          MIDIPROP_SET | MIDIPROP_TEMPO);
        if (pm_hosterror) goto close_device;

        divdata.cbStruct = sizeof(MIDIPROPTEMPO);
        divdata.dwTimeDiv = 480;   /* divisions per quarter */
        pm_hosterror = midiStreamProperty(m->handle.stream,
                                          (LPBYTE) & divdata,
                                          MIDIPROP_SET | MIDIPROP_TIMEDIV);
        if (pm_hosterror) goto close_device;
    }
    /* allocate buffers */
    if (allocate_buffers(m, output_buffer_len, num_buffers)) 
        goto free_buffers;
    /* start device */
    if (midi->latency != 0) {
        pm_hosterror = midiStreamRestart(m->handle.stream);
        if (pm_hosterror != MMSYSERR_NOERROR) goto free_buffers;
    }
    return pmNoError;

free_buffers:
    /* buffers are freed below by winmm_out_delete */
close_device:
    midiOutClose(m->handle.out);
free_descriptor:
    midi->descriptor = NULL;
    winmm_out_delete(midi); /* frees buffers and m */
no_memory:
    if (pm_hosterror) {
        int err = midiOutGetErrorText(pm_hosterror, (char *) pm_hosterror_text,
                                      PM_HOST_ERROR_MSG_LEN);
        assert(err == MMSYSERR_NOERROR);
        return pmHostError;
    }
    return pmInsufficientMemory;
}


/* winmm_out_delete -- carefully free data associated with midi */
/**/
static void winmm_out_delete(PmInternal *midi)
{
    int i;
    /* delete system dependent device data */
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    if (m) {
        if (m->buffer_signal) {
            /* don't report errors -- better not to stop cleanup */
            CloseHandle(m->buffer_signal);
        }
        /* if using stream output, free buffers */
        for (i = 0; i < m->num_buffers; i++) {
            if (m->buffers[i]) pm_free(m->buffers[i]);
        }
        m->num_buffers = 0;
        pm_free(m->buffers);
        m->max_buffers = 0;
#ifdef USE_SYSEX_BUFFERS
        /* free sysex buffers */
        for (i = 0; i < NUM_SYSEX_BUFFERS; i++) {
            if (m->sysex_buffers[i]) pm_free(m->sysex_buffers[i]);
        }
#endif
    }
    midi->descriptor = NULL;
    pm_free(m); /* delete */
}


/* see comments for winmm_in_close */
static PmError winmm_out_close(PmInternal *midi)
{
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    if (m->handle.out) {
        /* device to close */
        if (midi->latency == 0) {
            pm_hosterror = midiOutClose(m->handle.out);
        } else {
            pm_hosterror = midiStreamClose(m->handle.stream);
        }
        /* regardless of outcome, free memory */
        winmm_out_delete(midi);
    }
    if (pm_hosterror) {
        int err = midiOutGetErrorText(pm_hosterror,
                                      (char *) pm_hosterror_text,
                                      PM_HOST_ERROR_MSG_LEN);
        assert(err == MMSYSERR_NOERROR);
        return pmHostError;
    }
    return pmNoError;
}


static PmError winmm_out_abort(PmInternal *midi)
{
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    m->error = MMSYSERR_NOERROR;

    /* only stop output streams */
    if (midi->latency > 0) {
        m->error = midiStreamStop(m->handle.stream);
    }
    return m->error ? pmHostError : pmNoError;
}


static PmError winmm_write_flush(PmInternal *midi, PmTimestamp timestamp)
{
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    assert(m);
    if (m->hdr) {
        m->error = midiOutPrepareHeader(m->handle.out, m->hdr, 
                                        sizeof(MIDIHDR));
        if (m->error) {
            /* do not send message */
        } else if (midi->latency == 0) {
            /* As pointed out by Nigel Brown, 20Sep06, dwBytesRecorded
             * should be zero. This is set in get_free_sysex_buffer(). 
             * The msg length goes in dwBufferLength in spite of what
             * Microsoft documentation says (or doesn't say). */
            m->hdr->dwBufferLength = m->hdr->dwBytesRecorded;
            m->hdr->dwBytesRecorded = 0;
            m->error = midiOutLongMsg(m->handle.out, m->hdr, sizeof(MIDIHDR));
        } else {
            m->error = midiStreamOut(m->handle.stream, m->hdr, 
                                     sizeof(MIDIHDR));
        }
        midi->fill_base = NULL;
        m->hdr = NULL;
        if (m->error) {
            m->hdr->dwFlags = 0; /* release the buffer */
            return pmHostError;
        }
    }
    return pmNoError;
}



#ifdef GARBAGE
static PmError winmm_write_sysex_byte(PmInternal *midi, unsigned char byte)
{
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    unsigned char *msg_buffer;

    /* at the beginning of sysex, m->hdr is NULL */
    if (!m->hdr) { /* allocate a buffer if none allocated yet */
        m->hdr = get_free_output_buffer(midi);
        if (!m->hdr) return pmInsufficientMemory;
        m->sysex_byte_count = 0;
    }
    /* figure out where to write byte */
    msg_buffer = (unsigned char *) (m->hdr->lpData);
    assert(m->hdr->lpData == (char *) (m->hdr + 1));

    /* check for overflow */
    if (m->sysex_byte_count >= m->hdr->dwBufferLength) {
        /* allocate a bigger message -- double it every time */
        LPMIDIHDR big = allocate_buffer(m->sysex_byte_count * 2);
        /* printf("expand to %d bytes\n", m->sysex_byte_count * 2); */
        if (!big) return pmInsufficientMemory;
        m->error = midiOutPrepareHeader(m->handle.out, big,
                                        sizeof(MIDIHDR));
        if (m->error) {
            m->hdr = NULL;
            return pmHostError;
        }
        memcpy(big->lpData, msg_buffer, m->sysex_byte_count);
        msg_buffer = (unsigned char *) (big->lpData);
        if (m->buffers[0] == m->hdr) {
            m->buffers[0] = big;
            pm_free(m->hdr);
            /* printf("freed m->hdr\n"); */
        } else if (m->buffers[1] == m->hdr) {
            m->buffers[1] = big;
            pm_free(m->hdr);
            /* printf("freed m->hdr\n"); */
        }
        m->hdr = big;
    }

    /* append byte to message */
    msg_buffer[m->sysex_byte_count++] = byte;

    /* see if we have a complete message */
    if (byte == MIDI_EOX) {
        m->hdr->dwBytesRecorded = m->sysex_byte_count;
        /*
        { int i; int len = m->hdr->dwBytesRecorded;
          printf("OutLongMsg %d ", len);
          for (i = 0; i < len; i++) {
              printf("%2x ", msg_buffer[i]);
          }
        }
        */
        m->error = midiOutLongMsg(m->handle.out, m->hdr, sizeof(MIDIHDR));
        m->hdr = NULL; /* stop using this message buffer */
        if (m->error) return pmHostError;
    }
    return pmNoError;
}
#endif


static PmError winmm_write_short(PmInternal *midi, PmEvent *event)
{
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    PmError rslt = pmNoError;
    assert(m);

    if (midi->latency == 0) { /* use midiOut interface, ignore timestamps */
        m->error = midiOutShortMsg(m->handle.out, event->message);
        if (m->error) rslt = pmHostError;
    } else {  /* use midiStream interface -- pass data through buffers */
        unsigned long when = event->timestamp;
        unsigned long delta;
        int full;
        if (when == 0) when = midi->now;
        /* when is in real_time; translate to intended stream time */
        when = when + m->delta + midi->latency;
        /* make sure we don't go backward in time */
        if (when < m->last_time) when = m->last_time;
        delta = when - m->last_time;
        m->last_time = when;
        /* before we insert any data, we must have a buffer */
        if (m->hdr == NULL) {
            /* stream interface: buffers allocated when stream is opened */
            m->hdr = get_free_output_buffer(midi);
        }
        full = add_to_buffer(m, m->hdr, delta, event->message);
        if (full) rslt = winmm_write_flush(midi, when);
    }
    return rslt;
}

#define winmm_begin_sysex winmm_write_flush
#ifndef winmm_begin_sysex
static PmError winmm_begin_sysex(PmInternal *midi, PmTimestamp timestamp)
{
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    PmError rslt = pmNoError;

    if (midi->latency == 0) {
        /* do nothing -- it's handled in winmm_write_byte */
    } else {
        /* sysex expects an empty sysex buffer, so send whatever is here */
        rslt = winmm_write_flush(midi);
    }
    return rslt;
}
#endif

static PmError winmm_end_sysex(PmInternal *midi, PmTimestamp timestamp)
{
    /* could check for callback_error here, but I haven't checked
     * what happens if we exit early and don't finish the sysex msg
     * and clean up
     */
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    PmError rslt = pmNoError;
    LPMIDIHDR hdr = m->hdr;
    if (!hdr) return rslt; /* something bad happened earlier,
            do not report an error because it would have been 
            reported (at least) once already */
    /* a(n old) version of MIDI YOKE requires a zero byte after
     * the sysex message, but do not increment dwBytesRecorded: */
    hdr->lpData[hdr->dwBytesRecorded] = 0;
    if (midi->latency == 0) {
#ifdef DEBUG_PRINT_BEFORE_SENDING_SYSEX
        /* DEBUG CODE: */
        { int i; int len = m->hdr->dwBufferLength;
          printf("OutLongMsg %d ", len);
          for (i = 0; i < len; i++) {
              printf("%2x ", (unsigned char) (m->hdr->lpData[i]));
          }
        }
#endif
    } else {
        /* Using stream interface. There are accumulated bytes in m->hdr
           to send using midiStreamOut
         */
        /* add bytes recorded to MIDIEVENT length, but don't
           count the MIDIEVENT data (3 longs) */
        MIDIEVENT *evt = (MIDIEVENT *) (hdr->lpData);
        evt->dwEvent += hdr->dwBytesRecorded - 3 * sizeof(long);
        /* round up BytesRecorded to multiple of 4 */
        hdr->dwBytesRecorded = (hdr->dwBytesRecorded + 3) & ~3;
    }
    rslt = winmm_write_flush(midi, timestamp);
    return rslt;
}


static PmError winmm_write_byte(PmInternal *midi, unsigned char byte,
                                PmTimestamp timestamp)
{
    /* write a sysex byte */
    PmError rslt = pmNoError;
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    LPMIDIHDR hdr = m->hdr;
    unsigned char *msg_buffer;
    assert(m);
    if (!hdr) {
        m->hdr = hdr = get_free_output_buffer(midi);
        assert(hdr);
        midi->fill_base = (unsigned char *) m->hdr->lpData;
        midi->fill_offset_ptr = &(hdr->dwBytesRecorded);
        /* when buffer fills, Pm_WriteSysEx will revert to calling
         * pmwin_write_byte, which expect to have space, so leave
         * one byte free for pmwin_write_byte. Leave another byte
         * of space for zero after message to make early version of 
         * MIDI YOKE driver happy -- therefore dwBufferLength - 2 */
        midi->fill_length = hdr->dwBufferLength - 2;
        if (midi->latency != 0) {
            unsigned long when = (unsigned long) timestamp;
            unsigned long delta;
            unsigned long *ptr;
            if (when == 0) when = midi->now;
            /* when is in real_time; translate to intended stream time */
            when = when + m->delta + midi->latency;
            /* make sure we don't go backward in time */
            if (when < m->last_time) when = m->last_time;
            delta = when - m->last_time;
            m->last_time = when;

            ptr = (unsigned long *) hdr->lpData;
            *ptr++ = delta;
            *ptr++ = 0;
            *ptr = MEVT_F_LONG;
            hdr->dwBytesRecorded = 3 * sizeof(long);
            /* data will be added at an offset of dwBytesRecorded ... */
        }
    }
    /* add the data byte */
    msg_buffer = (unsigned char *) (hdr->lpData);
    msg_buffer[hdr->dwBytesRecorded++] = byte;

    /* see if buffer is full, leave one byte extra for pad */
    if (hdr->dwBytesRecorded >= hdr->dwBufferLength - 1) {
        /* write what we've got and continue */
        rslt = winmm_end_sysex(midi, timestamp); 
    }
    return rslt;
}

#ifdef EXPANDING_SYSEX_BUFFERS
note: this code is here as an aid in case you want sysex buffers
      to expand to hold large messages completely. If so, you
      will want to change SYSEX_BYTES_PER_BUFFER above to some
      variable that remembers the buffer size. A good place to 
      put this value would be in the hdr->dwUser field.

            rslt = resize_sysex_buffer(midi, m->sysex_byte_count, 
                                       m->sysex_byte_count * 2);

            if (rslt == pmBufferMaxSize) /* if the buffer can't be resized */
#endif
#ifdef EXPANDING_SYSEX_BUFFERS
            int bytesRecorded = hdr->dwBytesRecorded; /* this field gets wiped out, so we'll save it */
            rslt = resize_sysex_buffer(midi, bytesRecorded, 2 * bytesRecorded);
            hdr->dwBytesRecorded = bytesRecorded;

            if (rslt == pmBufferMaxSize) /* if buffer can't be resized */
#endif



static PmTimestamp winmm_synchronize(PmInternal *midi)
{
    midiwinmm_type m;
    unsigned long pm_stream_time_2;
    unsigned long real_time;
    unsigned long pm_stream_time;

    /* only synchronize if we are using stream interface */
    if (midi->latency == 0) return 0;

    /* figure out the time */
    m = (midiwinmm_type) midi->descriptor;
    pm_stream_time_2 = pm_time_get(m);

    do {
        /* read real_time between two reads of stream time */
        pm_stream_time = pm_stream_time_2;
        real_time = (*midi->time_proc)(midi->time_info);
        pm_stream_time_2 = pm_time_get(m);
        /* repeat if more than 1ms elapsed */
    } while (pm_stream_time_2 > pm_stream_time + 1);
    m->delta = pm_stream_time - real_time;
    m->sync_time = real_time;
    return real_time;
}

#ifdef USE_SYSEX_BUFFERS
/* winmm_out_callback -- recycle sysex buffers */
static void CALLBACK winmm_out_callback(HMIDIOUT hmo, UINT wMsg,
                                        DWORD dwInstance, DWORD dwParam1, 
                                        DWORD dwParam2)
{
    PmInternal *midi = (PmInternal *) dwInstance;
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    LPMIDIHDR hdr = (LPMIDIHDR) dwParam1;
    int err = 0;  /* set to 0 so that no buffer match will also be an error */

    /* Future optimization: eliminate UnprepareHeader calls -- they aren't
       necessary; however, this code uses the prepared-flag to indicate which
       buffers are free, so we need to do something to flag empty buffers if
       we leave them prepared
     */
    /*
    printf("out_callback: hdr %x, wMsg %x, MOM_DONE %x\n", 
           hdr, wMsg, MOM_DONE);
    */
    if (wMsg == MOM_DONE) {
        MMRESULT ret = midiOutUnprepareHeader(m->handle.out, hdr, 
                                              sizeof(MIDIHDR));
        assert(ret == MMSYSERR_NOERROR);
    }
    /* notify waiting sender that a buffer is available */
    err = SetEvent(m->buffer_signal);
    assert(err); /* false -> error */
}
#endif

/* winmm_streamout_callback -- unprepare (free) buffer header */
static void CALLBACK winmm_streamout_callback(HMIDIOUT hmo, UINT wMsg,
        DWORD dwInstance, DWORD dwParam1, DWORD dwParam2)
{
    PmInternal *midi = (PmInternal *) dwInstance;
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    LPMIDIHDR hdr = (LPMIDIHDR) dwParam1;
    int err;

    /* Even if an error is pending, I think we should unprepare msgs and
       signal their arrival
     */
    /* printf("streamout_callback: hdr %x, wMsg %x, MOM_DONE %x\n", 
           hdr, wMsg, MOM_DONE); */
    if (wMsg == MOM_DONE) {
        MMRESULT ret = midiOutUnprepareHeader(m->handle.out, hdr, 
                                              sizeof(MIDIHDR));
        assert(ret == MMSYSERR_NOERROR);
    }
    /* signal client in case it is blocked waiting for buffer */
    err = SetEvent(m->buffer_signal);
    assert(err); /* false -> error */
}


/*
=========================================================================================
begin exported functions
=========================================================================================
*/

#define winmm_in_abort pm_fail_fn
pm_fns_node pm_winmm_in_dictionary = {
                                         none_write_short,
                                         none_sysex,
                                         none_sysex,
                                         none_write_byte,
                                         none_write_short,
                                         none_write_flush,
                                         winmm_synchronize,
                                         winmm_in_open,
                                         winmm_in_abort,
                                         winmm_in_close,
                                         winmm_in_poll,
                                         winmm_has_host_error,
                                         winmm_get_host_error
                                     };

pm_fns_node pm_winmm_out_dictionary = {
                                          winmm_write_short,
                                          winmm_begin_sysex,
                                          winmm_end_sysex,
                                          winmm_write_byte,
                                          winmm_write_short,  /* short realtime message */
                                          winmm_write_flush,
                                          winmm_synchronize,
                                          winmm_out_open,
                                          winmm_out_abort,
                                          winmm_out_close,
                                          none_poll,
                                          winmm_has_host_error,
                                          winmm_get_host_error
                                      };


/* initialize winmm interface. Note that if there is something wrong
   with winmm (e.g. it is not supported or installed), it is not an
   error. We should simply return without having added any devices to
   the table. Hence, no error code is returned. Furthermore, this init
   code is called along with every other supported interface, so the
   user would have a very hard time figuring out what hardware and API
   generated the error. Finally, it would add complexity to pmwin.c to
   remember where the error code came from in order to convert to text.
 */
void pm_winmm_init( void )
{
    pm_winmm_mapper_input();
    pm_winmm_mapper_output();
    pm_winmm_general_inputs();
    pm_winmm_general_outputs();
}


/* no error codes are returned, even if errors are encountered, because
   there is probably nothing the user could do (e.g. it would be an error
   to retry.
 */
void pm_winmm_term( void )
{
    int i;
#ifdef DEBUG
    char msg[PM_HOST_ERROR_MSG_LEN];
#endif
    int doneAny = 0;
#ifdef DEBUG
    printf("pm_winmm_term called\n");
#endif
    for (i = 0; i < pm_descriptor_index; i++) {
        PmInternal * midi = descriptors[i].internalDescriptor;
        if (midi) {
            midiwinmm_type m = (midiwinmm_type) midi->descriptor;
            if (m->handle.out) {
                /* close next open device*/
#ifdef DEBUG
                if (doneAny == 0) {
                    printf("begin closing open devices...\n");
                    doneAny = 1;
                }
                /* report any host errors; this EXTEREMELY useful when
                   trying to debug client app */
                if (winmm_has_host_error(midi)) {
                    winmm_get_host_error(midi, msg, PM_HOST_ERROR_MSG_LEN);
                    printf("%s\n", msg);
                }
#endif
                /* close all open ports */
                (*midi->dictionary->close)(midi);
            }
        }
    }
    if (midi_in_caps) {
        pm_free(midi_in_caps);
        midi_in_caps = NULL;
    }
    if (midi_out_caps) {
        pm_free(midi_out_caps);
        midi_out_caps = NULL;
    }
#ifdef DEBUG
    if (doneAny) {
        printf("warning: devices were left open. They have been closed.\n");
    }
    printf("pm_winmm_term exiting\n");
#endif
    pm_descriptor_index = 0;
}
