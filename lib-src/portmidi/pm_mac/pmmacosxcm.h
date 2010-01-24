/* system-specific definitions */

PmError pm_macosxcm_init(void);
void pm_macosxcm_term(void);

PmDeviceID find_default_device(char *path, int input, PmDeviceID id);
