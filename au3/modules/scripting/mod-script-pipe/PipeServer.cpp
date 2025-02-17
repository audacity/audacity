#if defined(WIN32)

#define WIN32_LEAN_AND_MEAN  // Exclude rarely-used stuff from Windows headers
#include <windows.h>
#include <stdio.h>
#include <tchar.h>

const int nBuff = 1024;

extern "C" int DoSrv(char* pIn);
extern "C" int DoSrvMore(char* pOut, size_t nMax);

void PipeServer()
{
    HANDLE hPipeToSrv;
    HANDLE hPipeFromSrv;

    static const TCHAR pipeNameToSrv[] = _T("\\\\.\\pipe\\ToSrvPipe");

    hPipeToSrv = CreateNamedPipe(
        pipeNameToSrv,
        PIPE_ACCESS_DUPLEX,
        PIPE_TYPE_MESSAGE | PIPE_READMODE_MESSAGE | PIPE_WAIT | PIPE_REJECT_REMOTE_CLIENTS,
        PIPE_UNLIMITED_INSTANCES,
        nBuff,
        nBuff,
        50,//Timeout - always send straight away.
        NULL);
    if (hPipeToSrv == INVALID_HANDLE_VALUE) {
        return;
    }

    static const TCHAR pipeNameFromSrv[] = __T("\\\\.\\pipe\\FromSrvPipe");

    hPipeFromSrv = CreateNamedPipe(
        pipeNameFromSrv,
        PIPE_ACCESS_DUPLEX,
        PIPE_TYPE_MESSAGE | PIPE_READMODE_MESSAGE | PIPE_WAIT | PIPE_REJECT_REMOTE_CLIENTS,
        PIPE_UNLIMITED_INSTANCES,
        nBuff,
        nBuff,
        50,//Timeout - always send straight away.
        NULL);
    if (hPipeFromSrv == INVALID_HANDLE_VALUE) {
        return;
    }

    BOOL bConnected;
    BOOL bSuccess;
    DWORD cbBytesRead;
    DWORD cbBytesWritten;
    CHAR chRequest[ nBuff ];
    CHAR chResponse[ nBuff ];

    int jj=0;

    for (;;) {
        // open to (incoming) pipe first.
        printf("Obtaining pipe\n");
        bConnected = ConnectNamedPipe(hPipeToSrv, NULL)
                     ? TRUE : (GetLastError() == ERROR_PIPE_CONNECTED);
        printf("Obtained to-srv %i\n", bConnected);

        // open from (outgoing) pipe second.  This could block if there is no reader.
        bConnected = ConnectNamedPipe(hPipeFromSrv, NULL)
                     ? TRUE : (GetLastError() == ERROR_PIPE_CONNECTED);
        printf("Obtained from-srv %i\n", bConnected);

        if (bConnected) {
            for (;;) {
                printf("About to read\n");
                bSuccess = ReadFile(hPipeToSrv, chRequest, nBuff, &cbBytesRead, NULL);

                chRequest[ cbBytesRead] = '\0';

                if (!bSuccess || cbBytesRead == 0) {
                    break;
                }

                printf("Rxd %s\n", chRequest);

                DoSrv(chRequest);
                jj++;
                while (true)
                {
                    int nWritten = DoSrvMore(chResponse, nBuff);
                    if (nWritten <= 1) {
                        break;
                    }
                    WriteFile(hPipeFromSrv, chResponse, nWritten - 1, &cbBytesWritten, NULL);
                }
                //FlushFileBuffers( hPipeFromSrv );
            }
            FlushFileBuffers(hPipeToSrv);
            DisconnectNamedPipe(hPipeToSrv);
            FlushFileBuffers(hPipeFromSrv);
            DisconnectNamedPipe(hPipeFromSrv);
            break;
        } else {
            CloseHandle(hPipeToSrv);
            CloseHandle(hPipeFromSrv);
        }
    }
    CloseHandle(hPipeToSrv);
    CloseHandle(hPipeFromSrv);
}

#else

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

const char fifotmpl[] = "/tmp/audacity_script_pipe.%s.%d";

const int nBuff = 1024;

extern "C" int DoSrv(char* pIn);
extern "C" int DoSrvMore(char* pOut, size_t nMax);

void PipeServer()
{
    FILE* fromFifo = NULL;
    FILE* toFifo = NULL;
    int rc;
    char buf[nBuff];
    char toFifoName[nBuff];
    char fromFifoName[nBuff];

    sprintf(toFifoName, fifotmpl, "to", getuid());
    sprintf(fromFifoName, fifotmpl, "from", getuid());

    unlink(toFifoName);
    unlink(fromFifoName);

    // TODO avoid symlink security issues?

    rc = mkfifo(fromFifoName, S_IRWXU) & mkfifo(toFifoName, S_IRWXU);
    if (rc < 0) {
        perror("Unable to create fifos");
        printf("Ignoring...");
//      return;
    }

    // open to (incoming) pipe first.
    toFifo = fopen(toFifoName, "r");
    if (toFifo == NULL) {
        perror("Unable to open fifo to server from script");
        if (fromFifo != NULL) {
            fclose(fromFifo);
        }
        return;
    }

    // open from (outgoing) pipe second.  This could block if there is no reader.
    fromFifo = fopen(fromFifoName, "w");
    if (fromFifo == NULL) {
        perror("Unable to open fifo from server to script");
        return;
    }

    while (fgets(buf, sizeof(buf), toFifo) != NULL)
    {
        int len = strlen(buf);
        if (len <= 1) {
            continue;
        }

        buf[len - 1] = '\0';

        printf("Server received %s\n", buf);
        DoSrv(buf);

        while (true)
        {
            len = DoSrvMore(buf, nBuff);
            if (len <= 1) {
                break;
            }
            printf("Server sending %s", buf);

            // len - 1 because we do not send the null character
            fwrite(buf, 1, len - 1, fromFifo);
        }
        fflush(fromFifo);
    }

    printf("Read failed on fifo, quitting\n");

    if (toFifo != NULL) {
        fclose(toFifo);
    }

    if (fromFifo != NULL) {
        fclose(fromFifo);
    }

    unlink(toFifoName);
    unlink(fromFifoName);
}

#endif
