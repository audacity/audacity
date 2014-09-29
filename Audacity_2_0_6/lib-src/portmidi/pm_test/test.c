#include "portmidi.h"
#include "porttime.h"
#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "assert.h"

#define INPUT_BUFFER_SIZE 100
#define OUTPUT_BUFFER_SIZE 0
#define DRIVER_INFO NULL
#define TIME_PROC ((int32_t (*)(void *)) Pt_Time)
#define TIME_INFO NULL
#define TIME_START Pt_Start(1, 0, 0) /* timer started w/millisecond accuracy */

#define STRING_MAX 80 /* used for console input */

int32_t latency = 0;

/* crash the program to test whether midi ports are closed */
/**/
void doSomethingReallyStupid() {
	int * tmp = NULL;
	*tmp = 5;
}


/* exit the program without any explicit cleanup */
/**/
void doSomethingStupid() {
	assert(0);
}


/* read a number from console */
/**/
int get_number(char *prompt)
{
    char line[STRING_MAX];
    int n = 0, i;
    printf(prompt);
    while (n != 1) {
        n = scanf("%d", &i);
        fgets(line, STRING_MAX, stdin);

    }
    return i;
}


/*
 * the somethingStupid parameter can be set to simulate a program crash.
 * We want PortMidi to close Midi ports automatically in the event of a
 * crash because Windows does not (and this may cause an OS crash)
 */
void main_test_input(unsigned int somethingStupid) {
    PmStream * midi;
    PmError status, length;
    PmEvent buffer[1];
    int num = 10;
    int i = get_number("Type input number: ");
    /* It is recommended to start timer before Midi; otherwise, PortMidi may
       start the timer with its (default) parameters
     */
    TIME_START;

    /* open input device */
    Pm_OpenInput(&midi, 
                 i,
                 DRIVER_INFO, 
                 INPUT_BUFFER_SIZE, 
                 TIME_PROC, 
                 TIME_INFO);

    printf("Midi Input opened. Reading %d Midi messages...\n", num);
    Pm_SetFilter(midi, PM_FILT_ACTIVE | PM_FILT_CLOCK | PM_FILT_SYSEX);
    /* empty the buffer after setting filter, just in case anything
       got through */
    while (Pm_Poll(midi)) {
        Pm_Read(midi, buffer, 1);
    }
    /* now start paying attention to messages */
    i = 0; /* count messages as they arrive */
    while (i < num) {
        status = Pm_Poll(midi);
        if (status == TRUE) {
            length = Pm_Read(midi,buffer, 1);
            if (length > 0) {
                printf("Got message %d: time %ld, %2lx %2lx %2lx\n",
                       i,
                       (long) buffer[0].timestamp,
                       (long) Pm_MessageStatus(buffer[0].message),
                       (long) Pm_MessageData1(buffer[0].message),
                       (long) Pm_MessageData2(buffer[0].message));
                i++;
            } else {
                assert(0);
            }
        }
        /* simulate crash if somethingStupid is 1 or 2 */
        if ((i > (num/2)) && (somethingStupid == 1)) {
            doSomethingStupid();
        } else if ((i > (num/2)) && (somethingStupid == 2)) {
            doSomethingReallyStupid();
        }
    }

    /* close device (this not explicitly needed in most implementations) */
    printf("ready to close...");

    Pm_Close(midi);
    printf("done closing...");
}



void main_test_output() {
    PmStream * midi;
	char line[80];
    int32_t off_time;
    int chord[] = { 60, 67, 76, 83, 90 };
    #define chord_size 5 
    PmEvent buffer[chord_size];
    PmTimestamp timestamp;

    /* determine which output device to use */
    int i = get_number("Type output number: ");

    /* It is recommended to start timer before PortMidi */
    TIME_START;

    /* open output device -- since PortMidi avoids opening a timer
       when latency is zero, we will pass in a NULL timer pointer
       for that case. If PortMidi tries to access the time_proc,
       we will crash, so this test will tell us something. */
    Pm_OpenOutput(&midi, 
                  i, 
                  DRIVER_INFO,
                  OUTPUT_BUFFER_SIZE, 
                  (latency == 0 ? NULL : TIME_PROC),
                  (latency == 0 ? NULL : TIME_INFO), 
                  latency);
    printf("Midi Output opened with %ld ms latency.\n", (long) latency);

    /* output note on/off w/latency offset; hold until user prompts */
    printf("ready to send program 1 change... (type RETURN):");
    fgets(line, STRING_MAX, stdin);
    /* if we were writing midi for immediate output, we could always use
       timestamps of zero, but since we may be writing with latency, we
       will explicitly set the timestamp to "now" by getting the time.
       The source of timestamps should always correspond to the TIME_PROC
       and TIME_INFO parameters used in Pm_OpenOutput(). */
    buffer[0].timestamp = TIME_PROC(TIME_INFO);
    /* Send a program change to increase the chances we will hear notes */
    /* Program 0 is usually a piano, but you can change it here: */
#define PROGRAM 0
    buffer[0].message = Pm_Message(0xC0, PROGRAM, 0);
    Pm_Write(midi, buffer, 1);

    printf("ready to note-on... (type RETURN):");
    fgets(line, STRING_MAX, stdin);
    buffer[0].timestamp = TIME_PROC(TIME_INFO);
    buffer[0].message = Pm_Message(0x90, 60, 100);
    Pm_Write(midi, buffer, 1);
    printf("ready to note-off... (type RETURN):");
    fgets(line, STRING_MAX, stdin);
    buffer[0].timestamp = TIME_PROC(TIME_INFO);
    buffer[0].message = Pm_Message(0x90, 60, 0);
    Pm_Write(midi, buffer, 1);

    /* output short note on/off w/latency offset; hold until user prompts */
    printf("ready to note-on (short form)... (type RETURN):");
    fgets(line, STRING_MAX, stdin);
    Pm_WriteShort(midi, TIME_PROC(TIME_INFO),
                  Pm_Message(0x90, 60, 100));
    printf("ready to note-off (short form)... (type RETURN):");
    fgets(line, STRING_MAX, stdin);
    Pm_WriteShort(midi, TIME_PROC(TIME_INFO),
                  Pm_Message(0x90, 60, 0));

    /* output several note on/offs to test timing. 
       Should be 1s between notes */
    printf("chord will arpeggiate if latency > 0\n");
    printf("ready to chord-on/chord-off... (type RETURN):");
    fgets(line, STRING_MAX, stdin);
    timestamp = TIME_PROC(TIME_INFO);
    for (i = 0; i < chord_size; i++) {
        buffer[i].timestamp = timestamp + 1000 * i;
        buffer[i].message = Pm_Message(0x90, chord[i], 100);
    }
    Pm_Write(midi, buffer, chord_size);

    off_time = timestamp + 1000 + chord_size * 1000; 
    while (TIME_PROC(TIME_INFO) < off_time) 
		/* busy wait */;
    for (i = 0; i < chord_size; i++) {
        buffer[i].timestamp = timestamp + 1000 * i;
        buffer[i].message = Pm_Message(0x90, chord[i], 0);
    }
    Pm_Write(midi, buffer, chord_size);    

    /* close device (this not explicitly needed in most implementations) */
    printf("ready to close and terminate... (type RETURN):");
    fgets(line, STRING_MAX, stdin);
	
    Pm_Close(midi);
    Pm_Terminate();
    printf("done closing and terminating...\n");
}


void main_test_both()
{
    int i = 0;
    int in, out;
    PmStream * midi, * midiOut;
    PmEvent buffer[1];
    PmError status, length;
    int num = 10;
    
    in = get_number("Type input number: ");
    out = get_number("Type output number: ");

    /* In is recommended to start timer before PortMidi */
    TIME_START;

    Pm_OpenOutput(&midiOut, 
                  out, 
                  DRIVER_INFO,
                  OUTPUT_BUFFER_SIZE, 
                  TIME_PROC,
                  TIME_INFO, 
                  latency);
    printf("Midi Output opened with %ld ms latency.\n", (long) latency);
    /* open input device */
    Pm_OpenInput(&midi, 
                 in,
                 DRIVER_INFO, 
                 INPUT_BUFFER_SIZE, 
                 TIME_PROC, 
                 TIME_INFO);
    printf("Midi Input opened. Reading %d Midi messages...\n",num);
    Pm_SetFilter(midi, PM_FILT_ACTIVE | PM_FILT_CLOCK);
    /* empty the buffer after setting filter, just in case anything
       got through */
    while (Pm_Poll(midi)) {
        Pm_Read(midi, buffer, 1);
    }
    i = 0;
    while (i < num) {
        status = Pm_Poll(midi);
        if (status == TRUE) {
            length = Pm_Read(midi,buffer,1);
            if (length > 0) {
                Pm_Write(midiOut, buffer, 1);
                printf("Got message %d: time %ld, %2lx %2lx %2lx\n",
                       i,
                       (long) buffer[0].timestamp,
                       (long) Pm_MessageStatus(buffer[0].message),
                       (long) Pm_MessageData1(buffer[0].message),
                       (long) Pm_MessageData2(buffer[0].message));
                i++;
            } else {
                assert(0);
            }
        }
    }

    /* close midi devices */
    Pm_Close(midi);
    Pm_Close(midiOut);
    Pm_Terminate(); 
}


/* main_test_stream exercises windows winmm API's stream mode */
/*    The winmm stream mode is used for latency>0, and sends
   timestamped messages. The timestamps are relative (delta) 
   times, whereas PortMidi times are absolute. Since peculiar
   things happen when messages are not always sent in advance,
   this function allows us to exercise the system and test it.
 */
void main_test_stream() {
    PmStream * midi;
	char line[80];
    PmEvent buffer[16];

	/* determine which output device to use */
    int i = get_number("Type output number: ");

	latency = 500; /* ignore LATENCY for this test and
				      fix the latency at 500ms */

    /* It is recommended to start timer before PortMidi */
    TIME_START;

	/* open output device */
    Pm_OpenOutput(&midi, 
                  i, 
                  DRIVER_INFO,
                  OUTPUT_BUFFER_SIZE, 
                  TIME_PROC,
                  TIME_INFO, 
                  latency);
    printf("Midi Output opened with %ld ms latency.\n", (long) latency);

    /* output note on/off w/latency offset; hold until user prompts */
    printf("ready to send output... (type RETURN):");
    fgets(line, STRING_MAX, stdin);

    /* if we were writing midi for immediate output, we could always use
       timestamps of zero, but since we may be writing with latency, we
       will explicitly set the timestamp to "now" by getting the time.
       The source of timestamps should always correspond to the TIME_PROC
       and TIME_INFO parameters used in Pm_OpenOutput(). */
    buffer[0].timestamp = TIME_PROC(TIME_INFO);
    buffer[0].message = Pm_Message(0xC0, 0, 0);
	buffer[1].timestamp = buffer[0].timestamp;
	buffer[1].message = Pm_Message(0x90, 60, 100);
	buffer[2].timestamp = buffer[0].timestamp + 1000;
	buffer[2].message = Pm_Message(0x90, 62, 100);
	buffer[3].timestamp = buffer[0].timestamp + 2000;
	buffer[3].message = Pm_Message(0x90, 64, 100);
	buffer[4].timestamp = buffer[0].timestamp + 3000;
	buffer[4].message = Pm_Message(0x90, 66, 100);
	buffer[5].timestamp = buffer[0].timestamp + 4000;
	buffer[5].message = Pm_Message(0x90, 60, 0);
	buffer[6].timestamp = buffer[0].timestamp + 4000;
	buffer[6].message = Pm_Message(0x90, 62, 0);
	buffer[7].timestamp = buffer[0].timestamp + 4000;
	buffer[7].message = Pm_Message(0x90, 64, 0);
	buffer[8].timestamp = buffer[0].timestamp + 4000;
	buffer[8].message = Pm_Message(0x90, 66, 0);

    Pm_Write(midi, buffer, 9);
#ifdef SEND8
	/* Now, we're ready for the real test.
	   Play 4 notes at now, now+500, now+1000, and now+1500
	   Then wait until now+2000.
	   Play 4 more notes as before.
	   We should hear 8 evenly spaced notes. */
	now = TIME_PROC(TIME_INFO);
	for (i = 0; i < 4; i++) {
		buffer[i * 2].timestamp = now + (i * 500);
		buffer[i * 2].message = Pm_Message(0x90, 60, 100);
		buffer[i * 2 + 1].timestamp = now + 250 + (i * 500);
		buffer[i * 2 + 1].message = Pm_Message(0x90, 60, 0);
	}
    Pm_Write(midi, buffer, 8);

    while (Pt_Time() < now + 2500) 
		/* busy wait */;
	/* now we are 500 ms behind schedule, but since the latency
	   is 500, the delay should not be audible */
	now += 2000;
	for (i = 0; i < 4; i++) {
		buffer[i * 2].timestamp = now + (i * 500);
		buffer[i * 2].message = Pm_Message(0x90, 60, 100);
		buffer[i * 2 + 1].timestamp = now + 250 + (i * 500);
		buffer[i * 2 + 1].message = Pm_Message(0x90, 60, 0);
	}
    Pm_Write(midi, buffer, 8);
#endif
    /* close device (this not explicitly needed in most implementations) */
    printf("ready to close and terminate... (type RETURN):");
    fgets(line, STRING_MAX, stdin);
	
    Pm_Close(midi);
    Pm_Terminate();
    printf("done closing and terminating...\n");
}


void show_usage()
{
    printf("Usage: test [-h] [-l latency-in-ms]\n");
    exit(0);
}

int main(int argc, char *argv[])
{
    int default_in;
    int default_out;
    int i = 0, n = 0;
    char line[STRING_MAX];
    int test_input = 0, test_output = 0, test_both = 0, somethingStupid = 0;
    int stream_test = 0;
    int latency_valid = FALSE;
    
    if (sizeof(void *) == 8) 
        printf("Apparently this is a 64-bit machine.\n");
    else if (sizeof(void *) == 4) 
        printf ("Apparently this is a 32-bit machine.\n");
    
    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-h") == 0) {
            show_usage();
        } else if (strcmp(argv[i], "-l") == 0 && (i + 1 < argc)) {
            i = i + 1;
            latency = atoi(argv[i]);
            printf("Latency will be %ld\n", (long) latency);
            latency_valid = TRUE;
        } else {
            show_usage();
        }
    }

    while (!latency_valid) {
        int lat; // declared int to match "%d"
        printf("Latency in ms: ");
        if (scanf("%d", &lat) == 1) {
            latency = (int32_t) lat; // coerce from "%d" to known size
	    latency_valid = TRUE;
        }
    }

    /* determine what type of test to run */
    printf("begin portMidi test...\n");
    printf("%s%s%s%s%s",
           "enter your choice...\n    1: test input\n",
           "    2: test input (fail w/assert)\n",
           "    3: test input (fail w/NULL assign)\n",
           "    4: test output\n    5: test both\n",
           "    6: stream test\n");
    while (n != 1) {
        n = scanf("%d", &i);
        fgets(line, STRING_MAX, stdin);
        switch(i) {
        case 1: 
            test_input = 1;
            break;
        case 2: 
            test_input = 1;
            somethingStupid = 1;
            break;
        case 3: 
            test_input = 1;
            somethingStupid = 2;
            break;
        case 4: 
            test_output = 1;
            break;
        case 5:
            test_both = 1;
            break;
		case 6:
			stream_test = 1;
			break;
        default:
            printf("got %d (invalid input)\n", n);
            break;
        }
    }
    
    /* list device information */
    default_in = Pm_GetDefaultInputDeviceID();
    default_out = Pm_GetDefaultOutputDeviceID();
    for (i = 0; i < Pm_CountDevices(); i++) {
        char *deflt;
        const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
        if (((test_input  | test_both) & info->input) |
            ((test_output | test_both | stream_test) & info->output)) {
            printf("%d: %s, %s", i, info->interf, info->name);
            if (info->input) {
                deflt = (i == default_in ? "default " : "");
                printf(" (%sinput)", deflt);
            }
            if (info->output) {
                deflt = (i == default_out ? "default " : "");
                printf(" (%soutput)", deflt);
            }
            printf("\n");
        }
    }
    
    /* run test */
    if (stream_test) {
        main_test_stream();
    } else if (test_input) {
        main_test_input(somethingStupid);
    } else if (test_output) {
        main_test_output();
    } else if (test_both) {
        main_test_both();
    }
    
    printf("finished portMidi test...type ENTER to quit...");
    fgets(line, STRING_MAX, stdin);
    return 0;
}
