/* MacCommandWin.h -- headers for more mac stuff */

void SetSelection (short start, short end);
void macputc(int ch);
void macputs(char *s);
void PrepareForInput(void);
void InitalizeCmdWindow(void);
void UpdateCmdWindow(void);
void StopPasting(void);
void DeleteRange(void);
void scrflush(void);
void SetScrollRect(void);
void AdjustCursor(Point theLoc, RgnHandle theRgn);
void DoKeyPress(EventRecord *theEvent);
void ActivateCmdWindow(void);
void DeactivateCmdWindow(void);
void CleanupCmdWindow(void);
