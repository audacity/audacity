/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ipc-types.h

  @author Vitaly Sverchinsky

  Part of lib-ipc library

**********************************************************************/

#pragma once

#ifdef _WIN32
#include <WinSock2.h>
#include <WS2tcpip.h>
#define CLOSE_SOCKET closesocket
#define NFDS(x) (0)//not used on winsock2
#else
#include <sys/select.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <poll.h>
#include <fcntl.h>

#define INVALID_SOCKET -1
#define SOCKET_ERROR -1
#define SOCKET int
#define CLOSE_SOCKET close
#define NFDS(x) ((x) + 1)
#endif
