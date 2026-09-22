### NNLS Chroma ###

System identifier – vamp:nnls-chroma:nnls-chroma
RDF URI – http://vamp-plugins.org/rdf/plugins/nnls-chroma#nnls-chroma

#### General Description ####

NNLS Chroma analyses a single channel of audio using frame-wise spectral input from the Vamp host. The plugin was originally developed to extract treble and bass chromagrams for subsequent use in chord extraction methods. The spectrum is transformed to a log-frequency spectrum (constant-Q) with three bins per semitone. On this representation, two processing steps are performed:
* tuning, after which each centre bin (i.e. bin 2, 5, 8, ...) corresponds to a semitone, even if the tuning of the piece deviates from 440 Hz standard pitch.
* running standardisation: subtraction of the running mean, division by the running standard deviation. This has a spectral whitening effect.

The processed log-frequency spectrum is then used as an input for NNLS approximate transcription (using a dictionary of harmonic notes with geometrically decaying harmonics magnitudes). The output of the NNLS approximate transcription is semitone-spaced. To get the chroma, this semitone spectrum is multiplied (element-wise) with the desired profile (chroma or bass chroma) and then mapped to 12 bins. The resulting chroma frames can be normalised by (dividing by) their norm (L1, L2 and maximum norm available).

#### Parameters ####

The default settings (in brackets, below) are those used for Matthias Mauch's 2010 MIREX submissions.

* use approximate transcription (NNLS) (on or off; default: on): toggle between NNLS approximate transcription and linear spectral mapping.
* spectral roll on spectral roll on (0 % -- 5 %; default: 0 %): consider the cumulative energy spectrum (from low to high frequencies). All bins below the first bin whose cumulative energy exceeds the quantile [spectral roll on] x [total energy] will be set to 0. A value of 0 means that no bins will be changed.
* tuning mode (global or local; default: global): local uses a local average for tuning, global uses all audio frames. Local tuning is only advisable when the tuning is likely to change over the audio, for example in podcasts, or in a cappella singing.
* spectral whitening (0.0 -- 1.0; default: 1.0): determines how much the log-frequency spectrum is whitened. A value of 0.0 means no whitening. For values other than 0.0 the log-freq spectral bins are divided by  [standard deviation of their neighbours]^[spectral whitening], where "^" means "to the power of".
* spectral shape (0.5 -- 0.9; default: 0.7): the shape of the notes in the NNLS dictionary. Their harmonic amplitude follows a geometrically decreasing pattern, in which the i-th harmonic has an amplitude of [spectral shape]^[i-1], where "^" means "to the power of".
* chroma normalisation (none, maximum norm, L1 norm, L2 norm; default: none): determines whether or how the chromagrams are normalised. If the setting is not 'none', then each chroma frame separately is divided by the chosen vector norm. Note that normalisation implies that the joint 24-dim. "Chroma and Bass Chromagram" output will be different from the individual 12-dim. "Chromagram" and "Bass Chromagram" outputs.

#### Outputs ####

* Log-frequency Spectrum: a spectrum similar to the well-known constant Q spectrum, in which bins are linear in log-frequency. Three bins per semitone.
* Tuned Log-frequency Spectrum: has the same format as Log-frequency Spectrum, but has been processed by the following processes: tuning, subtraction of background spectrum, spectral whitening.
* Semitone Spectrum: a spectral representation with one bin per semitone. If NNLS is selected in the parameters, this is the note activation, otherwise just a linear mapping to semitones.
* Bass Chromagram: a 12-dimensional chromagram, restricted to the bass range. At each frame the Semitone Spectrum is multiplied by a bass pattern and then mapped to the 12 chroma bins. 
* Chromagram: a 12-dimensional chromagram, restricted with mid-range emphasis. At each frame the Semitone Spectrum is multiplied by a mid-range pattern and then mapped to the 12 chroma bins.
* Chromagram and Bass Chromagram: a 24-dimensional chromagram, consisting of the both Bass Chromgram and Chromagram, see above. When normalisation is used, this representation will however be scaled differently, and hence be different from the individual chromagrams.
* Consonance estimate: A simple consonance value based on the convolution of a consonance profile with the Semitone Spectrum. Experimental status. Compare two pieces of audio in terms of consonance if the instrumentation is similar. Instruments with fluctuating pitches (also: voice) will decrease the consonance value.

### Chordino ###

System identifier – vamp:nnls-chroma:chordino
RDF URI – http://vamp-plugins.org/rdf/plugins/nnls-chroma#chordino

#### General Description ####

Chordino provides a simple chord transcription based on NNLS Chroma (described above). Chord profiles given by the user in the file "chord.dict" are used to calculate frame-wise chord similarities. Two simple (non-state-of-the-art!) algorithms are available that smooth these to provide a chord transcription: a simple chord change method, and a standard HMM/Viterbi approach.

#### Parameters ####

* use approximate transcription (NNLS) (on or off; default: on): toggle between NNLS approximate transcription and linear spectral mapping.
* HMM (Viterbi decoding) (on or off; default: on): uses HMM/Viterbi smoothing. Otherwise: heuristic chord change smoothing.
* spectral roll on (0 % -- 5 %; default: 0 %): consider the cumulative energy spectrum (from low to high frequencies). All bins below the first bin whose cumulative energy exceeds the quantile [spectral roll on] x [total energy] will be set to 0. A value of 0 means that no bins will be changed.
* tuning mode (global or local; default: global): local uses a local average for tuning. Local tuning is only advisable when the tuning is likely to change over the audio, for example in podcasts, or in a cappella singing.
* spectral whitening (0.0 -- 1.0; default: 1.0): determines how much the log-frequency spectrum is whitened. A value of 0.0 means no whitening. For values other than 0.0 the log-freq spectral bins are divided by  [standard deviation of their neighbours]^[spectral whitening], where "^" means "to the power of".
* spectral shape (0.5 -- 0.9; default: 0.7): the shape of the notes in the NNLS dictionary. Their harmonic amplitude follows a geometrically decreasing pattern, in which the i-th harmonic has an amplitude of [spectral shape]^[i-1], where "^" means "to the power of".
* chroma normalisation (none, maximum norm, L1 norm, L2 norm; default: none): determines whether or how the chromagrams are normalised. If the setting is not 'none', then each chroma frame separately is divided by the chosen vector norm. Note that normalisation implies that the joint 24-dim. "Chroma and Bass Chromagram" output will be different from the individual 12-dim. "Chromagram" and "Bass Chromagram" outputs.
* boost likelihood of the N (no chord) label (0.0 -- 1.0; default: 0.1): leads to greater values in the profile of the "no chord" chord, hence non-harmonic parts of audio files are more likely to be recogised as such. Warning: for values above the default, it quickly leads to many chords being misclassified as N.

#### Outputs ####

* Chord Estimate: estimated chord times and labels.
* Harmonic Change Value: an indication of the likelihood of harmonic change. Depends on the chord dictionary. Calculation is different depending on whether the Viterbi algorithm is used for chord estimation, or the simple chord estimate.
* Note Representation of Chord Estimate: a simple MIDI-like represenation of the estimated chord with bass note (if applicable) and chord notes. Can be used, for example, to export MIDI chords from Sonic Visuliser.

### Tuning ###

System identifier – vamp:nnls-chroma:tuning
RDF URI – http://vamp-plugins.org/rdf/plugins/nnls-chroma#tuning

#### General Description ####

The tuning plugin can estimate the local and global tuning of piece. The same tuning method is used for the NNLS Chroma and Chordino plugins.

#### Parameter ####

* spectral roll on spectral roll on (0 % -- 5 %; default: 0 %): consider the cumulative energy spectrum (from low to high frequencies). All bins below the first bin whose cumulative energy exceeds the quantile [spectral roll on] x [total energy] will be set to 0. A value of 0 means that no bins will be changed.

#### Outputs ####

* Tuning: returns a single label (at time 0 seconds) containing an estimate of the concert pitch in Hz.
* Local Tuning: returns a tuning estimate at every analysis frame, an average of the (recent) previous frame-wise estimates of the concert pitch in Hz.

### References and Credits ###

If you make use of this software for any public or commercial purpose,
we ask you to kindly mention the authors and Queen Mary, University of
London in your user-visible documentation. We're very happy to see
this sort of use, but would much appreciate being credited, separately
from the requirements of the software license itself (see below).

If you make use of this software for academic purposes, please cite:

Mauch, Matthias and Dixon, Simon: [*Approximate Note Transcription for the Improved Identification of Difficult Chords*](http://schall-und-mauch.de/artificialmusicality/?p=89), Proceedings of the 11th International Society for Music Information Retrieval Conference (ISMIR 2010), 2010.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
