/* nosc-server.c -- an OSC server for Nyquist */
/*
 * this enables OSC clients to set slider values in Nyquist
 * for security reasons, OSC clients cannot invoke Lisp expressions
 * the only operation allowed is to set a value in a Lisp array
 *
 * The API is:
 *
 * int nosc_init() -- initialize the server, return error, 0 means none
 * int nosc_poll() -- poll for messages and process them, return error, 0 means none
 * void nosc_finish() -- free data structures, return error, 0 means none
 */

#ifdef OSC
#ifdef WIN32
#include <winsock2.h>
#include <malloc.h>
#include <process.h>
#else
#include <stdlib.h>
#include <sys/time.h>
#include <sys/types.h>
#include <strings.h>
#include <unistd.h>
#include <stdio.h>
#endif
#include "xlisp.h"
#include "sound.h" /* to get nosc_enabled */
#include "lo/lo.h"
#include "sliders.h"

static lo_server the_server = NULL;
static int lo_fd;

static void error(int num, const char *msg, const char *path)
{
    char s[256];
    sprintf(s, "liblo server error %d in path %s: %s\n", num, path, msg);
    stdputstr(s);
}


static int slider_handler(const char *path, const char *types, lo_arg **argv, 
			  int argc, void *data, void *user_data)
{
    // printf("%s <- %d, %g\n", path, argv[0]->i, argv[1]->f);
    // fflush(stdout);
    set_slider(argv[0]->i, argv[1]->f);
    return 0;
}

// wii_orientation_handler -- controls sliders 0 and 1 in range [0, 1]
//    using wii orientation messages from OSC
static int wii_orientation_handler(const char *path, const char *types, 
                                   lo_arg **argv, int argc, void *data, 
                                   void *user_data)
{
    set_slider(0, min(1.0F, max(0.0F, (argv[0]->f / 180) + 0.5)));
    set_slider(1, min(1.0F, max(0.0F, (argv[1]->f / 180) + 0.5)));
    return 0;
}


int nosc_init()
{
    the_server = lo_server_new("7770", error);
    /* add method that will match the path /slider, with two numbers, coerced
     * to int and float */
    lo_server_add_method(the_server, "/slider", "if", slider_handler, NULL);
    lo_server_add_method(the_server, "/wii/orientation", "ff", 
                         wii_orientation_handler, NULL);
    lo_fd = lo_server_get_socket_fd(the_server);
    nosc_enabled = true;
    return 0;
}


int nosc_poll()
{
    fd_set rfds;
    struct timeval tv;
    int retval;

    // loop, receiving all pending OSC messages
    while (true) {
        FD_ZERO(&rfds);
        FD_SET(lo_fd, &rfds);
	    tv.tv_sec = 0;
        tv.tv_usec = 0;

        retval = select(lo_fd + 1, &rfds, NULL, NULL, &tv); 
	    if (retval == -1) {
            stdputstr("select() error in nosc_poll\n");
		    return -1;
        } else if (retval > 0 && FD_ISSET(lo_fd, &rfds)) {
            /* printf("lo_server_recv_noblock 1\n"); */
            lo_server_recv_noblock(the_server, 0);
        } else {
        	return 0;
        }
    }
}


void nosc_finish()
{
    lo_server_free(the_server);
    nosc_enabled = false;
}

#endif

