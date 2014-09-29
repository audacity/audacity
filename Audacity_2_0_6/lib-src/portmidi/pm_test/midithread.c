/* midithread.c -- example program showing how to do midi processing 
                   in a preemptive thread

  Notes: if you handle midi I/O from your main program, there will be
  some delay before handling midi messages whenever the program is
  doing something like file I/O, graphical interface updates, etc.

  To handle midi with minimal delay, you should do all midi processing
  in a separate, high priority thread. A convenient way to get a high
  priority thread in windows is to use the timer callback provided by
  the PortTime library. That is what we show here.

  If the high priority thread writes to a file, prints to the console,
  or does just about anything other than midi processing, this may 
  create delays, so all this processing should be off-loaded to the
  "main" process or thread. Communication between threads can be tricky.
  If one thread is writing at the same time the other is reading, very
  tricky race conditions can arise, causing programs to behave
  incorrectly, but only under certain timing conditions -- a terrible
  thing to debug. Advanced programmers know this as a synchronization
  problem. See any operating systems textbook for the complete story.

  To avoid synchronization problems, a simple, reliable approach is
  to communicate via messages. PortMidi offers a message queue as a
  datatype, and operations to insert and remove messages. Use two 
  queues as follows: midi_to_main transfers messages from the midi
  thread to the main thread, and main_to_midi transfers messages from
  the main thread to the midi thread. Queues are safe for use between
  threads as long as ONE thread writes and ONE thread reads. You must 
  NEVER allow two threads to write to the same queue.

  This program transposes incoming midi data by an amount controlled
  by the main program. To change the transposition, type an integer
  followed by return. The main program sends this via a message queue
  to the midi thread. To quit, type 'q' followed by return.

  The midi thread can also send a pitch to the main program on request.
  Type 'm' followed by return to wait for the next midi message and
  print the pitch.

  This program illustrates:
    Midi processing in a high-priority thread.
    Communication with a main process via message queues.

 */

#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "assert.h"
#include "portmidi.h"
#include "pmutil.h"
#include "porttime.h"

/* if INPUT_BUFFER_SIZE is 0, PortMidi uses a default value */
#define INPUT_BUFFER_SIZE 0

#define OUTPUT_BUFFER_SIZE 100
#define DRIVER_INFO NULL
#define TIME_PROC NULL
#define TIME_INFO NULL
/* use zero latency because we want output to be immediate */
#define LATENCY 0

#define STRING_MAX 80

/**********************************/
/* DATA USED ONLY BY process_midi */
/* (except during initialization) */
/**********************************/

int active = FALSE;
int monitor = FALSE;
int midi_thru = TRUE;

int transpose;
PmStream *midi_in;
PmStream *midi_out;

/****************************/
/* END OF process_midi DATA */
/****************************/

/* shared queues */
PmQueue *midi_to_main;
PmQueue *main_to_midi;

#define QUIT_MSG 1000
#define MONITOR_MSG 1001
#define THRU_MSG 1002

/* timer interrupt for processing midi data */
void process_midi(PtTimestamp timestamp, void *userData)
{
    PmError result;
    PmEvent buffer; /* just one message at a time */
    int32_t msg;

    /* do nothing until initialization completes */
    if (!active) 
        return;

    /* check for messages */
    do { 
        result = Pm_Dequeue(main_to_midi, &msg); 
        if (result) {
            if (msg >= -127 && msg <= 127) 
                transpose = msg;
            else if (msg == QUIT_MSG) {
                /* acknowledge receipt of quit message */
                Pm_Enqueue(midi_to_main, &msg);
                active = FALSE;
                return;
            } else if (msg == MONITOR_MSG) {
                /* main has requested a pitch. monitor is a flag that
                 * records the request:
                 */
                monitor = TRUE;
            } else if (msg == THRU_MSG) {
                /* toggle Thru on or off */
                midi_thru = !midi_thru;
            }
        }
    } while (result);         
    
    /* see if there is any midi input to process */
    do {
		result = Pm_Poll(midi_in);
        if (result) {
            int status, data1, data2;
            if (Pm_Read(midi_in, &buffer, 1) == pmBufferOverflow) 
                continue;
            if (midi_thru) 
                Pm_Write(midi_out, &buffer, 1);
            /* unless there was overflow, we should have a message now */
            status = Pm_MessageStatus(buffer.message);
            data1 = Pm_MessageData1(buffer.message);
            data2 = Pm_MessageData2(buffer.message);
            if ((status & 0xF0) == 0x90 ||
                (status & 0xF0) == 0x80) {
                
                /* this is a note-on or note-off, so transpose and send */
                data1 += transpose;
                
                /* keep within midi pitch range, keep proper pitch class */
                while (data1 > 127) 
                    data1 -= 12;
                while (data1 < 0) 
                    data1 += 12;
                
                /* send the message */
                buffer.message = Pm_Message(status, data1, data2);
                Pm_Write(midi_out, &buffer, 1);
                
                /* if monitor is set, send the pitch to the main thread */
                if (monitor) {
                    Pm_Enqueue(midi_to_main, &data1);
                    monitor = FALSE; /* only send one pitch per request */
                }
            }
        }
    } while (result);
}

void exit_with_message(char *msg)
{
    char line[STRING_MAX];
    printf("%s\n", msg);
    fgets(line, STRING_MAX, stdin);
    exit(1);
}

int main()
{
    int id;
    int32_t n;
    const PmDeviceInfo *info;
    char line[STRING_MAX];
    int spin;
    int done = FALSE;

    /* determine what type of test to run */
    printf("begin PortMidi multithread test...\n");
	
    /* note that it is safe to call PortMidi from the main thread for
       initialization and opening devices. You should not make any
       calls to PortMidi from this thread once the midi thread begins.
       to make PortMidi calls.
     */

    /* make the message queues */
    /* messages can be of any size and any type, but all messages in
     * a given queue must have the same size. We'll just use int32_t's
     * for our messages in this simple example
     */
    midi_to_main = Pm_QueueCreate(32, sizeof(int32_t));
    assert(midi_to_main != NULL);
    main_to_midi = Pm_QueueCreate(32, sizeof(int32_t));
    assert(main_to_midi != NULL);

    /* a little test of enqueue and dequeue operations. Ordinarily, 
     * you would call Pm_Enqueue from one thread and Pm_Dequeue from
     * the other. Since the midi thread is not running, this is safe.
     */
    n = 1234567890;
    Pm_Enqueue(midi_to_main, &n);
    n = 987654321;
    Pm_Enqueue(midi_to_main, &n);
	Pm_Dequeue(midi_to_main, &n);
	if (n != 1234567890) {
        exit_with_message("Pm_Dequeue produced unexpected result.");
    }
    Pm_Dequeue(midi_to_main, &n);
	if(n != 987654321) {
        exit_with_message("Pm_Dequeue produced unexpected result.");
    }

    /* always start the timer before you start midi */
    Pt_Start(1, &process_midi, 0); /* start a timer with millisecond accuracy */
    /* the timer will call our function, process_midi() every millisecond */
    
	Pm_Initialize();

    id = Pm_GetDefaultOutputDeviceID();
    info = Pm_GetDeviceInfo(id);
    if (info == NULL) {
        printf("Could not open default output device (%d).", id);
        exit_with_message("");
    }
    printf("Opening output device %s %s\n", info->interf, info->name);

    /* use zero latency because we want output to be immediate */
    Pm_OpenOutput(&midi_out, 
                  id, 
                  DRIVER_INFO,
                  OUTPUT_BUFFER_SIZE,
                  TIME_PROC,
                  TIME_INFO,
                  LATENCY);

    id = Pm_GetDefaultInputDeviceID();
    info = Pm_GetDeviceInfo(id);
    if (info == NULL) {
        printf("Could not open default input device (%d).", id);
        exit_with_message("");
    }
    printf("Opening input device %s %s\n", info->interf, info->name);
    Pm_OpenInput(&midi_in, 
                 id, 
                 DRIVER_INFO,
                 INPUT_BUFFER_SIZE,
                 TIME_PROC,
                 TIME_INFO);

    active = TRUE; /* enable processing in the midi thread -- yes, this
                      is a shared variable without synchronization, but
                      this simple assignment is safe */

    printf("Enter midi input; it will be transformed as specified by...\n");
    printf("%s\n%s\n%s\n",
           "Type 'q' to quit, 'm' to monitor next pitch, t to toggle thru or",
           "type a number to specify transposition.",
		   "Must terminate with [ENTER]");

    while (!done) {
        int32_t msg;
        int input;
        int len;
        fgets(line, STRING_MAX, stdin);
        /* remove the newline: */
        len = strlen(line);
        if (len > 0) line[len - 1] = 0; /* overwrite the newline char */
        if (strcmp(line, "q") == 0) {
            msg = QUIT_MSG;
            Pm_Enqueue(main_to_midi, &msg);
            /* wait for acknowlegement */
            do {
                spin = Pm_Dequeue(midi_to_main, &msg);
            } while (spin == 0); /* spin */ ;
            done = TRUE; /* leave the command loop and wrap up */
        } else if (strcmp(line, "m") == 0) {
            msg = MONITOR_MSG;
            Pm_Enqueue(main_to_midi, &msg);
            printf("Waiting for note...\n");
            do {
                spin = Pm_Dequeue(midi_to_main, &msg);
            } while (spin == 0); /* spin */ ;
            // convert int32_t to long for safe printing
            printf("... pitch is %ld\n", (long) msg);
        } else if (strcmp(line, "t") == 0) {
            /* reading midi_thru asynchronously could give incorrect results,
               e.g. if you type "t" twice before the midi thread responds to
               the first one, but we'll do it this way anyway. Perhaps a more
               correct way would be to wait for an acknowledgement message
               containing the new state. */
            printf("Setting THRU %s\n", (midi_thru ? "off" : "on"));
            msg = THRU_MSG;
            Pm_Enqueue(main_to_midi, &msg);
        } else if (sscanf(line, "%d", &input) == 1) {
            if (input >= -127 && input <= 127) {
                /* send transposition value, make sur */
                printf("Transposing by %d\n", input);
                msg = (int32_t) input;
                Pm_Enqueue(main_to_midi, &msg);
            } else {
                printf("Transposition must be within -127...127\n");
            }
        } else {
            printf("%s\n%s\n",
              "Type 'q[ENTER]' to quit, 'm[ENTER]' to monitor next pitch, or",
              "enter a number to specify transposition.");
        }
    }

    /* at this point, midi thread is inactive and we need to shut down
     * the midi input and output
     */
    Pt_Stop(); /* stop the timer */
    Pm_QueueDestroy(midi_to_main);
    Pm_QueueDestroy(main_to_midi);

    /* Belinda! if close fails here, some memory is deleted, right??? */
    Pm_Close(midi_in);
    Pm_Close(midi_out);
    
    printf("finished portMidi multithread test...enter any character to quit [RETURN]...");
    fgets(line, STRING_MAX, stdin);
    return 0;
}
