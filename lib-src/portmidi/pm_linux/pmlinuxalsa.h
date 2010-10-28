/* pmlinuxalsa.h -- system-specific definitions */

PmError pm_linuxalsa_init(void);
void pm_linuxalsa_term(void);

PmDeviceID find_default_device(char *path, int input, PmDeviceID id);

