#include <Windows.h>
#include <portaudio.h>
#include <pa_win_ds.h>

int main() 
{
    PaWinDS_GetDeviceGUID(0, 0);

    return 0;
}