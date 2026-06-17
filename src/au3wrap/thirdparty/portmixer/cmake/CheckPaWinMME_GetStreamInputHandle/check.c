#include <Windows.h>
#include <portaudio.h>
#include <pa_win_wmme.h>

int main() 
{
    PaWinMME_GetStreamInputHandle(0, 0);
    PaWinMME_GetStreamOutputHandle(0, 0);

    return 0;
}