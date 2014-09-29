/* sysex.c -- example program showing how to send and receive sysex
    messages

   Messages are stored in a file using 2-digit hexadecimal numbers,
   one per byte, separated by blanks, with up to 32 numbers per line:
   F0 14 A7 4B ...

 */

#include "stdio.h"
#include "stdlib.h"
#include "assert.h"
#include "portmidi.h"
#include "porttime.h"
#include "string.h"
#ifdef WIN32
// need to get declaration for Sleep()
#include "windows.h"
#else
#include <unistd.h>
#define Sleep(n) usleep(n * 1000)
#endif

#define MIDI_SYSEX 0xf0
#define MIDI_EOX 0xf7

#define STRING_MAX 80

#ifndef true
#define true 1
#define false 0
#endif

int latency = 0;

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


/* loopback test -- send/rcv from 2 to 1000 bytes of random midi data */
/**/
void loopback_test()
{
    int outp;
    int inp;
    PmStream *midi_in;
    PmStream *midi_out;
    unsigned char msg[1024];
    char line[80];
    int32_t len;
    int i;
    int data;
    PmEvent event;
    int shift;
    long total_bytes = 0;
    int32_t begin_time;

    Pt_Start(1, 0, 0);
    
    printf("Connect a midi cable from an output port to an input port.\n");
    printf("This test will send random data via sysex message from output\n");
    printf("to input and check that the correct data was received.\n");
    outp = get_number("Type output device number: ");
    /* Open output with 1ms latency -- when latency is non-zero, the Win32
       implementation supports sending sysex messages incrementally in a 
       series of buffers. This is nicer than allocating a big buffer for the
       message, and it also seems to work better. Either way works.
     */
    while ((latency = get_number(
                     "Latency in milliseconds (0 to send data immediatedly,\n"
                     "  >0 to send timestamped messages): ")) < 0);
    Pm_OpenOutput(&midi_out, outp, NULL, 0, NULL, NULL, latency);
    inp = get_number("Type input device number: ");
    /* since we are going to send and then receive, make sure the input buffer
       is large enough for the entire message */
    Pm_OpenInput(&midi_in, inp, NULL, 512, NULL, NULL);

    srand((unsigned int) Pt_Time()); /* seed for random numbers */

    begin_time = Pt_Time();
    while (1) {
        PmError count;
        int32_t start_time;
        int error_position = -1; /* 0; -1; -1 for continuous */ 
        int expected = 0;
        int actual = 0;
        /* this modification will run until an error is detected */
        /* set error_position above to 0 for interactive, -1 for */
        /* continuous */
        if (error_position >= 0) {
            printf("Type return to send message, q to quit: ");
            fgets(line, STRING_MAX, stdin);
            if (line[0] == 'q') goto cleanup;
        }

        /* compose the message */
        len = rand() % 998 + 2; /* len only counts data bytes */
        msg[0] = (char) MIDI_SYSEX; /* start of SYSEX message */
        /* data bytes go from 1 to len */
        for (i = 0; i < len; i++) {
/* pick whether data is sequential or random... (docs say random) */
#define DATA_EXPR (i+1)
// #define DATA_EXPR rand()
            msg[i + 1] = DATA_EXPR & 0x7f; /* MIDI data */
        }
        /* final EOX goes in len+1, total of len+2 bytes in msg */
        msg[len + 1] = (char) MIDI_EOX;

        /* sanity check: before we send, there should be no queued data */
        count = Pm_Read(midi_in, &event, 1);

        if (count != 0) {
            printf("Before sending anything, a MIDI message was found in\n");
            printf("the input buffer. Please try again.\n");
            break;
		}

        /* send the message */
        printf("Sending %d byte sysex message.\n", len + 2);
        Pm_WriteSysEx(midi_out, 0, msg);

        /* receive the message and compare to msg[] */
        data = 0;
        shift = 0;
        i = 0;
        start_time = Pt_Time();
        error_position = -1;
        /* allow up to 2 seconds for transmission */
        while (data != MIDI_EOX && start_time + 2000 > Pt_Time()) {
            count = Pm_Read(midi_in, &event, 1);
            if (count == 0) {
                Sleep(1); /* be nice: give some CPU time to the system */
                continue; /* continue polling for input */
            }
            
            /* printf("read %lx ", event.message);
               fflush(stdout); */
            
            /* compare 4 bytes of data until you reach an eox */
            for (shift = 0; shift < 32 && (data != MIDI_EOX); shift += 8) {
                data = (event.message >> shift) & 0xFF;
                if (data != msg[i] && error_position < 0) {
                    error_position = i;
                    expected = msg[i];
                    actual = data;
                }
                i++;
            }
        }
        if (error_position >= 0) {
            printf("Error at byte %d: sent %x recd %x.\n", error_position, 
                   expected, actual);
            break;
        } else if (i != len + 2) {
            printf("Error: byte %d not received.\n", i);
            break;
        } else {
            int seconds = (Pt_Time() - begin_time) / 1000;
	    if (seconds == 0) seconds = 1;
            printf("Correctly received %d byte sysex message.\n", i);
	    total_bytes += i;
	    printf("Cummulative bytes/sec: %d\n", total_bytes / seconds);
        }
    }
cleanup:
    Pm_Close(midi_out);
    Pm_Close(midi_in);
    return;
}


/* send_multiple test -- send many sysex messages */
/**/
void send_multiple_test()
{
    int outp;
    int length;
    int num_msgs;
    PmStream *midi_out;
    unsigned char msg[1024];
    int i;
    PtTimestamp start_time;
    PtTimestamp stop_time;

    Pt_Start(1, 0, 0);
    
    printf("This is for performance testing. You should be sending to this\n");
    printf("program running the receive multiple test. Do NOT send to\n");
    printf("a synthesizer or you risk reprogramming it\n");
    outp = get_number("Type output device number: ");
    while ((latency = get_number(
                     "Latency in milliseconds (0 to send data immediatedly,\n"
                     "  >0 to send timestamped messages): ")) < 0);
    Pm_OpenOutput(&midi_out, outp, NULL, 0, NULL, NULL, latency);
    while ((length = get_number("Message length (7 - 1024): ")) < 7 ||
           length > 1024) ;
    while ((num_msgs = get_number("Number of messages: ")) < 1);
    /* latency, length, and num_msgs should now all be valid */
    /* compose the message except for sequence number in first 5 bytes */
    msg[0] = (char) MIDI_SYSEX;
    for (i = 6; i < length - 1; i++) {
        msg[i] = i % 128; /* this is just filler */
    }
    msg[length - 1] = (char) MIDI_EOX;

    start_time = Pt_Time();
    /* send the messages */
    for (i = num_msgs; i > 0; i--) {
        /* insert sequence number into first 5 data bytes */
        /* sequence counts down to zero */
        int j;
        int count = i;
        /* 7 bits of message count i goes into each data byte */
        for (j = 1; j <= 5; j++) {
            msg[j] = count & 127;
            count >>= 7;
        }
        /* send the message */
        Pm_WriteSysEx(midi_out, 0, msg);
    }
    stop_time = Pt_Time();
    Pm_Close(midi_out);
    return;
}

#define MAX_MSG_LEN 1024
static unsigned char receive_msg[MAX_MSG_LEN];
static int receive_msg_index;
static int receive_msg_length;
static int receive_msg_count;
static int receive_msg_error;
static int receive_msg_messages;
static PmStream *receive_msg_midi_in;
static int receive_poll_running;

/* receive_poll -- callback function to check for midi input */
/**/
void receive_poll(PtTimestamp timestamp, void *userData)
{
    PmError count;
    PmEvent event;
    int shift;
    int data = 0;
    int i;
    
    if (!receive_poll_running) return; /* wait until midi device is opened */
    shift = 0;
    while (data != MIDI_EOX) {
        count = Pm_Read(receive_msg_midi_in, &event, 1);
        if (count == 0) return;

        /* compare 4 bytes of data until you reach an eox */
        for (shift = 0; shift < 32 && (data != MIDI_EOX); shift += 8) {
            receive_msg[receive_msg_index++] = data = 
                (event.message >> shift) & 0xFF;
            if (receive_msg_index >= MAX_MSG_LEN) {
                printf("error: incoming sysex too long\n");
                goto error;
            }
        }
    }
    /* check the message */
    if (receive_msg_length == 0) {
        receive_msg_length = receive_msg_index;
    }
    if (receive_msg_length != receive_msg_index) {
        printf("error: incoming sysex wrong length\n");
        goto error;
    }
    if (receive_msg[0] != MIDI_SYSEX) {
        printf("error: incoming sysex missing status byte\n");
        goto error;
    }
    /* get and check the count */
    count = 0;
    for (i = 0; i < 5; i++) {
        count += receive_msg[i + 1] << (7 * i);
    }
    if (receive_msg_count == -1) {
        receive_msg_count = count;
        receive_msg_messages = count;
    }
    if (receive_msg_count != count) {
        printf("error: incoming sysex has wrong count\n");
        goto error;
    }
    for (i = 6; i < receive_msg_index - 1; i++) {
        if (receive_msg[i] != i % 128) {
            printf("error: incoming sysex has bad data\n");
            goto error;
        }
    }
    if (receive_msg[receive_msg_length - 1] != MIDI_EOX) goto error;
    receive_msg_index = 0; /* get ready for next message */
    receive_msg_count--;
    return;
 error:
    receive_msg_error = 1;
    return;
}


/* receive_multiple_test -- send/rcv from 2 to 1000 bytes of random midi data */
/**/
void receive_multiple_test()
{
    PmError err;
    int inp;
    
    printf("This test expects to receive data sent by the send_multiple test\n");
    printf("The test will check that correct data is received.\n");

    /* Important: start PortTime first -- if it is not started first, it will
       be started by PortMidi, and then our attempt to open again will fail */
    receive_poll_running = false;
    if ((err = Pt_Start(1, receive_poll, 0))) {
        printf("PortTime error code: %d\n", err);
        goto cleanup;
    }
    inp = get_number("Type input device number: ");
    Pm_OpenInput(&receive_msg_midi_in, inp, NULL, 512, NULL, NULL);
    receive_msg_index = 0;
    receive_msg_length = 0;
    receive_msg_count = -1;
    receive_msg_error = 0;
    receive_poll_running = true;
    while ((!receive_msg_error) && (receive_msg_count != 0)) {
#ifdef WIN32
        Sleep(1000);
#else
        sleep(1); /* block and wait */
#endif
    }
    if (receive_msg_error) {
        printf("Receive_multiple test encountered an error\n");
    } else {
        printf("Receive_multiple test successfully received %d sysex messages\n", 
               receive_msg_messages);
    }
cleanup:
    receive_poll_running = false;
    Pm_Close(receive_msg_midi_in);
    Pt_Stop();
    return;
}


#define is_real_time_msg(msg) ((0xF0 & Pm_MessageStatus(msg)) == 0xF8)


void receive_sysex()
{
    char line[80];
    FILE *f;
    PmStream *midi;
    int shift = 0;
    int data = 0;
    int bytes_on_line = 0;
    PmEvent msg;

    /* determine which output device to use */
    int i = get_number("Type input device number: ");

    /* open input device */
    Pm_OpenInput(&midi, i, NULL, 512, NULL, NULL);
    printf("Midi Input opened, type file for sysex data: ");

    /* open file */
    fgets(line, STRING_MAX, stdin);
    /* remove the newline character */
    if (strlen(line) > 0) line[strlen(line) - 1] = 0;
    f = fopen(line, "w");
    if (!f) {
        printf("Could not open %s\n", line);
        Pm_Close(midi);
        return;
    }

    printf("Ready to receive a sysex message\n");

    /* read data and write to file */
    while (data != MIDI_EOX) {
        PmError count;
        count = Pm_Read(midi, &msg, 1);
        /* CAUTION: this causes busy waiting. It would be better to 
           be in a polling loop to avoid being compute bound. PortMidi
           does not support a blocking read since this is so seldom
           useful.
         */
        if (count == 0) continue;
        /* ignore real-time messages */
        if (is_real_time_msg(Pm_MessageStatus(msg.message))) continue;

        /* write 4 bytes of data until you reach an eox */
        for (shift = 0; shift < 32 && (data != MIDI_EOX); shift += 8) {
            data = (msg.message >> shift) & 0xFF;
            /* if this is a status byte that's not MIDI_EOX, the sysex
               message is incomplete and there is no more sysex data */
            if (data & 0x80 && data != MIDI_EOX) break;
            fprintf(f, "%2x ", data);
            if (++bytes_on_line >= 16) {
                fprintf(f, "\n");
                bytes_on_line = 0;
            }
        }
    }
    fclose(f);
    Pm_Close(midi);
}


void send_sysex()
{
    char line[80];
    FILE *f;
    PmStream *midi;
    int data;
    int shift = 0;
    PmEvent msg;

	/* determine which output device to use */
    int i = get_number("Type output device number: ");
    while ((latency = get_number(
                     "Latency in milliseconds (0 to send data immediatedly,\n"
                     "  >0 to send timestamped messages): ")) < 0);

    msg.timestamp = 0; /* no need for timestamp */

	/* open output device */
    Pm_OpenOutput(&midi, i, NULL, 0, NULL, NULL, latency);
	printf("Midi Output opened, type file with sysex data: ");

    /* open file */
    fgets(line, STRING_MAX, stdin);
    /* remove the newline character */
    if (strlen(line) > 0) line[strlen(line) - 1] = 0;
    f = fopen(line, "r");
    if (!f) {
        printf("Could not open %s\n", line);
        Pm_Close(midi);
        return;
    }

    /* read file and send data */
    msg.message = 0;
    while (1) {
        /* get next byte from file */

        if (fscanf(f, "%x", &data) == 1) {
            /* printf("read %x, ", data); */
            /* OR byte into message at proper offset */
            msg.message |= (data << shift);
            shift += 8;
        }
        /* send the message if it's full (shift == 32) or if we are at end */
        if (shift == 32 || data == MIDI_EOX) {
            /* this will send sysex data 4 bytes at a time -- it would
               be much more efficient to send multiple PmEvents at once
               but this method is simpler. See Pm_WriteSysEx for a more
               efficient code example.
             */
            Pm_Write(midi, &msg, 1);
            msg.message = 0;
            shift = 0;
        }
        if (data == MIDI_EOX) { /* end of message */
            fclose(f);
            Pm_Close(midi);
            return;
        }
    }
}


int main()
{
    int i;
    char line[80];
    
    /* list device information */
    for (i = 0; i < Pm_CountDevices(); i++) {
        const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
        printf("%d: %s, %s", i, info->interf, info->name);
        if (info->input) printf(" (input)");
        if (info->output) printf(" (output)");
        printf("\n");
    }
    while (1) {
        printf("Type r to receive sysex, s to send,"
               " l for loopback test, m to send multiple,"
               " n to receive multiple, q to quit: ");
        fgets(line, STRING_MAX, stdin);
        switch (line[0]) {
          case 'r':
            receive_sysex();
            break;
          case 's':
            send_sysex();
            break;
          case 'l':
            loopback_test();
            break;
          case 'm':
            send_multiple_test();
            break;
          case 'n':
            receive_multiple_test();
            break;
          case 'q':
            exit(0);
          default:
            break;
        }
    }
    return 0;
}
