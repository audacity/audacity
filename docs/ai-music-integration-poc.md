# AI Music Studio integration proof of concept

The first AI Music Studio command is available at **Analyze → AI Music: Analyze selection**.

It requires a non-empty selection of up to 30 seconds on exactly one audio track. Audacity decodes the selected channel to a temporary float32 file, launches the bundled native `ai_music_analysis_helper`, sends the audio path plus selection metadata, receives a JSON response, and displays the result.

This deliberately proves the integration boundary without adding a model download, network dependency, Python runtime, or destructive audio edit. The helper returns decoded frame count, RMS level, a zero-crossing pitch estimate for simple monophonic material, and analysis from the bundled QM Vamp Plugins DLL:

- Median BPM calculated from QM beat timestamps, with a timing-consistency score.
- The modal QM key class and the proportion of analysis frames that agree with it.
- Beat timestamps displayed as compact native point ticks on a new **AI Music: Tempo & Beats** track.
- A sparse **AI Music: Key Map** track created from detected key-change events. These are key regions, not chord names.
- Chordino chord-change events displayed as labelled spans on a new **AI Music: Chords** track.
- QM Segmenter structural changes displayed as neutral, editable section spans on a new **AI Music: Arrangement** track. The labels describe detected section types; they do not claim a verse/chorus interpretation.

Audio is resampled to mono 44.1 kHz inside the helper before analysis. The plugin remains out of the Audacity process and is discovered only in the helper's `bin/vamp` folder through `VAMP_PATH`.

When the detected BPM is in the 40–300 range and has at least 50% timing consistency, the command automatically assigns that tempo to the selected track's clips and updates the project tempo. This updates the timing grid without time-stretching the audio. The project tempo change is recorded as an ordinary Audacity undo step.

After a confident timing update, valid beat timestamps within the decoded selection become compact point ticks. Times are offset by the actual decoded selection start, including when the selected range begins before the audio. The key detector also creates a sparse Key Map lane from its change events, Chordino creates labelled chord spans, and QM Segmenter creates neutral arrangement spans. New analysis lanes are placed at the top in a fixed order: Tempo & Beats, Chords, Key Map, Arrangement. Each run creates separate analysis tracks and preserves existing labels. The analysis-map track additions are one undo step, separate from the preceding tempo change. Empty or low-confidence results create no map track. Stop playback and recording before running the command.

QM Vamp Plugins is GPL-2.0. Chordino (from NNLS Chroma) is GPL-2.0-or-later. The build packages both plug-ins and their notices in the install tree. Any distribution must retain the applicable source and licence obligations.
