/*********************************************************/
/*
  Definition of SKINI Message Types and Special Symbols
     Synthesis toolKit Instrument Network Interface

  These symbols should have the form __SK_<name>_

  Where <name> is the string used in the SKINI stream.

  by Perry R. Cook, 1995 - 2002.
*/
/*********************************************************/

namespace Nyq
{

/***** MIDI COMPATIBLE MESSAGES *****/
/***** Status Bytes Have Channel=0 **/

#define NOPE    -32767
#define YEP     1
#define SK_DBL  -32766
#define SK_INT  -32765
#define SK_STR  -32764

#define __SK_NoteOff_                128
#define __SK_NoteOn_                 144
#define __SK_PolyPressure_           160
#define __SK_ControlChange_          176
#define __SK_ProgramChange_          192
#define __SK_AfterTouch_             208
#define __SK_ChannelPressure_        __SK_AfterTouch_
#define __SK_PitchWheel_             224
#define __SK_PitchBend_              __SK_PitchWheel_
#define __SK_PitchChange_            249

#define __SK_Clock_                  248
#define __SK_SongStart_              250
#define __SK_Continue_               251
#define __SK_SongStop_               252
#define __SK_ActiveSensing_          254
#define __SK_SystemReset_            255

#define __SK_Volume_                 7
#define __SK_ModWheel_               1
#define __SK_Modulation_             __SK_ModWheel_
#define __SK_Breath_                 2
#define __SK_FootControl_            4
#define __SK_Portamento_             65
#define __SK_Balance_                8
#define __SK_Pan_                    10
#define __SK_Sustain_                64
#define __SK_Damper_                 __SK_Sustain_
#define __SK_Expression_             11 

#define __SK_AfterTouch_Cont_        128
#define __SK_ModFrequency_           __SK_Expression_

#define __SK_ProphesyRibbon_         16
#define __SK_ProphesyWheelUp_        2
#define __SK_ProphesyWheelDown_      3
#define __SK_ProphesyPedal_          18
#define __SK_ProphesyKnob1_          21
#define __SK_ProphesyKnob2_          22

/***  Instrument Family Specific ***/

#define __SK_NoiseLevel_             __SK_FootControl_

#define __SK_PickPosition_           __SK_FootControl_
#define __SK_StringDamping_          __SK_Expression_
#define __SK_StringDetune_           __SK_ModWheel_
#define __SK_BodySize_               __SK_Breath_
#define __SK_BowPressure_            __SK_Breath_
#define __SK_BowPosition_            __SK_PickPosition_
#define __SK_BowBeta_                __SK_BowPosition_

#define __SK_ReedStiffness_          __SK_Breath_
#define __SK_ReedRestPos_            __SK_FootControl_

#define __SK_FluteEmbouchure_        __SK_Breath_
#define __SK_JetDelay_               __SK_FluteEmbouchure_

#define __SK_LipTension_             __SK_Breath_
#define __SK_SlideLength_            __SK_FootControl_

#define __SK_StrikePosition_         __SK_PickPosition_
#define __SK_StickHardness_          __SK_Breath_

#define __SK_TrillDepth_             1051
#define __SK_TrillSpeed_             1052
#define __SK_StrumSpeed_             __SK_TrillSpeed_
#define __SK_RollSpeed_              __SK_TrillSpeed_

#define __SK_FilterQ_                __SK_Breath_
#define __SK_FilterFreq_             1062
#define __SK_FilterSweepRate_        __SK_FootControl_

#define __SK_ShakerInst_             1071 
#define __SK_ShakerEnergy_           __SK_Breath_
#define __SK_ShakerDamping_          __SK_ModFrequency_
#define __SK_ShakerNumObjects_       __SK_FootControl_

#define __SK_Strumming_              1090
#define __SK_NotStrumming_           1091
#define __SK_Trilling_               1092
#define __SK_NotTrilling_            1093
#define __SK_Rolling_                __SK_Strumming_
#define __SK_NotRolling_             __SK_NotStrumming_

#define __SK_PlayerSkill_            2001
#define __SK_Chord_                  2002
#define __SK_ChordOff_               2003

#define __SK_SINGER_FilePath_        3000
#define __SK_SINGER_Frequency_       3001
#define __SK_SINGER_NoteName_        3002
#define __SK_SINGER_Shape_           3003
#define __SK_SINGER_Glot_            3004
#define __SK_SINGER_VoicedUnVoiced_  3005
#define __SK_SINGER_Synthesize_      3006
#define __SK_SINGER_Silence_         3007
#define __SK_SINGER_VibratoAmt_      __SK_ModWheel_
#define __SK_SINGER_RndVibAmt_       3008
#define __SK_SINGER_VibFreq_         __SK_Expression_

} // namespace Nyq
