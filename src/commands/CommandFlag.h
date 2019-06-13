//
//  CommandFlag.h
//  Audacity
//
//  Created by Paul Licameli on 11/22/16.
//
//

#ifndef __AUDACITY_COMMAND_FLAG__
#define __AUDACITY_COMMAND_FLAG__

// Flags used in command handling.

// These flags represent the majority of the states that affect
// whether or not items in menus are enabled or disabled.
enum CommandFlag : unsigned long long
{
   AlwaysEnabledFlag      = 0x00000000,

   AudioIONotBusyFlag     = 0x00000001,
   TimeSelectedFlag       = 0x00000002, // This is equivalent to check if there is a valid selection, so it's used for Zoom to Selection too
   TracksSelectedFlag     = 0x00000004,
   TracksExistFlag        = 0x00000008,
   LabelTracksExistFlag   = 0x00000010,
   WaveTracksSelectedFlag = 0x00000020,

   UnsavedChangesFlag     = 0x00000080,
   HasLastEffectFlag      = 0x00000100,
   UndoAvailableFlag      = 0x00000200,
   RedoAvailableFlag      = 0x00000400,
   ZoomInAvailableFlag    = 0x00000800,
   ZoomOutAvailableFlag   = 0x00001000,
   StereoRequiredFlag     = 0x00002000,  //lda

   TrackPanelHasFocus     = 0x00008000,  //lll

   LabelsSelectedFlag     = 0x00020000,
   AudioIOBusyFlag        = 0x00040000,  //lll
   PlayRegionLockedFlag   = 0x00080000,  //msmeyer
   PlayRegionNotLockedFlag= 0x00100000,  //msmeyer
   CutCopyAvailableFlag   = 0x00200000,
   WaveTracksExistFlag    = 0x00400000,
   NoteTracksExistFlag    = 0x00800000,  //gsw
   NoteTracksSelectedFlag = 0x01000000,  //gsw

   IsNotSyncLockedFlag    = 0x04000000,  //awd
   IsSyncLockedFlag       = 0x08000000,  //awd
   IsRealtimeNotActiveFlag= 0x10000000,  //lll
   CaptureNotBusyFlag     = 0x20000000,
   CanStopAudioStreamFlag = 0x40000000,

   NotMinimizedFlag      = 0x100000000ULL, // prl
   PausedFlag            = 0x200000000ULL, // jkc

   HasWaveDataFlag       = 0x800000000ULL, // jkc
   PlayableTracksExistFlag = 0x1000000000ULL,
   AudioTracksSelectedFlag = 0x2000000000ULL,
   NoAutoSelect            = 0x4000000000ULL, // jkc

   NoFlagsSpecified        = ~0ULL
};

// Prevent accidental misuse with narrower types

bool operator == (CommandFlag, unsigned long) PROHIBITED;
bool operator == (CommandFlag, long) PROHIBITED;
bool operator == (unsigned long, CommandFlag) PROHIBITED;
bool operator == (long, CommandFlag) PROHIBITED;

bool operator != (CommandFlag, unsigned long) PROHIBITED;
bool operator != (CommandFlag, long) PROHIBITED;
bool operator != (unsigned long, CommandFlag) PROHIBITED;
bool operator != (long, CommandFlag) PROHIBITED;

CommandFlag operator & (CommandFlag, unsigned long) PROHIBITED;
CommandFlag operator & (CommandFlag, long) PROHIBITED;
CommandFlag operator & (unsigned long, CommandFlag) PROHIBITED;
CommandFlag operator & (long, CommandFlag) PROHIBITED;

CommandFlag operator | (CommandFlag, unsigned long) PROHIBITED;
CommandFlag operator | (CommandFlag, long) PROHIBITED;
CommandFlag operator | (unsigned long, CommandFlag) PROHIBITED;
CommandFlag operator | (long, CommandFlag) PROHIBITED;

CommandFlag operator ^ (CommandFlag, unsigned long) PROHIBITED;
CommandFlag operator ^ (CommandFlag, long) PROHIBITED;
CommandFlag operator ^ (unsigned long, CommandFlag) PROHIBITED;
CommandFlag operator ^ (long, CommandFlag) PROHIBITED;

bool operator == (CommandFlag, unsigned int) PROHIBITED;
bool operator == (CommandFlag, int) PROHIBITED;
bool operator == (unsigned int, CommandFlag) PROHIBITED;
bool operator == (int, CommandFlag) PROHIBITED;

bool operator != (CommandFlag, unsigned int) PROHIBITED;
bool operator != (CommandFlag, int) PROHIBITED;
bool operator != (unsigned int, CommandFlag) PROHIBITED;
bool operator != (int, CommandFlag) PROHIBITED;

CommandFlag operator & (CommandFlag, unsigned int) PROHIBITED;
CommandFlag operator & (CommandFlag, int) PROHIBITED;
CommandFlag operator & (unsigned int, CommandFlag) PROHIBITED;
CommandFlag operator & (int, CommandFlag) PROHIBITED;

CommandFlag operator | (CommandFlag, unsigned int) PROHIBITED;
CommandFlag operator | (CommandFlag, int) PROHIBITED;
CommandFlag operator | (unsigned int, CommandFlag) PROHIBITED;
CommandFlag operator | (int, CommandFlag) PROHIBITED;

CommandFlag operator ^ (CommandFlag, unsigned int) PROHIBITED;
CommandFlag operator ^ (CommandFlag, int) PROHIBITED;
CommandFlag operator ^ (unsigned int, CommandFlag) PROHIBITED;
CommandFlag operator ^ (int, CommandFlag) PROHIBITED;

// Supply the bitwise operations

inline constexpr CommandFlag operator ~ (CommandFlag flag)
{
   return static_cast<CommandFlag>( ~ static_cast<unsigned long long> (flag) );
}
inline constexpr CommandFlag operator & (CommandFlag lhs, CommandFlag rhs)
{
   return static_cast<CommandFlag> (
      static_cast<unsigned long long>(lhs) &
      static_cast<unsigned long long>(rhs)
   );
}
inline constexpr CommandFlag operator | (CommandFlag lhs, CommandFlag rhs)
{
   return static_cast<CommandFlag> (
      static_cast<unsigned long long>(lhs) |
      static_cast<unsigned long long>(rhs)
   );
}
inline CommandFlag & operator |= (CommandFlag &lhs, CommandFlag rhs)
{
   lhs = lhs | rhs;
   return lhs;
}

using CommandMask = CommandFlag;

#endif
