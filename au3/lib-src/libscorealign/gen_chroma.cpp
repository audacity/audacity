
#ifdef _WIN32
    #include "malloc.h"
#endif
#include "stdlib.h" // for OSX compatibility, malloc.h -> stdlib.h
#include "stdio.h"
#include "assert.h"
#include "string.h"
#include "math.h"
#include <fstream>
#include "allegro.h"
#include "fft3/FFT3.h"
#include "audioreader.h"
#include "scorealign.h"
#include "gen_chroma.h"
#include "comp_chroma.h"
#include "mfmidi.h"
#include "sautils.h"
#ifdef SA_VERBOSE
#include <iostream> // cout
#endif
using namespace std;

#ifdef min
#undef min
#endif
#define min(x,y) ((x)<(y)?(x):(y))

#ifdef max
#undef max
#endif
#define max(x,y) ((x)>(y)?(x):(y))

//if 1, causes printing internally
#define PRINT_BIN_ENERGY 1

#define p1 0.0577622650466621
#define p2 2.1011784386926213

// each row is one chroma vector, 
// data is stored as an array of chroma vectors:
// vector 1, vector 2, ...

float hz_to_step(float hz)
{
    return float((log(hz) - p2) / p1);
}

/*				GEN_MAGNITUDE
   given the real and imaginary portions of a complex FFT function, compute 
   the magnitude of the fft bin.
   
   NOTE: out should be length n
*/
void gen_Magnitude(float* inR, float* inI, int low, int hi, float* out)
{
    int i;
    
    for (i = low; i < hi; i++) {
      float magVal = sqrt(inR[i] * inR[i] + inI[i] * inI[i]);
      //printf("   %d: sqrt(%g^2+%g^2)=%g\n",i,inR[i],inI[i+1],magVal);
      out[i]= magVal;
#ifdef SA_VERBOSE
      if (i == 1000) fprintf(dbf, "gen_Magnitude: %d %g\n", i, magVal);
#endif
    }
}


/*				PRINT_BINS
    This function is intended for debugging purposes.
    pass in an array representing the "mid point"
    of each bin, and the number of bins.  The
    function will print out:
    i value
    index falue
    low range of the bin
    middle of the bin
    high range of the bin
*/
void print_Bins(float* bins, int numBins){
    printf("BINS: \n");
    int i;
    for (i=0; i<numBins; i++) {
      int index = i % numBins;
      int indexNext = (index + 1) % numBins;
      int indexPrev = (index - 1) % numBins;
      
      float maxValue =(bins[index]+bins[indexNext])/2;
      float minValue=(bins[index]+bins[indexPrev])/2;
      
      if(index == 1)
        maxValue =bins[index]+(bins[index]-((bins[index]+bins[indexPrev])/2));
      if(index == 2)
        minValue =bins[index]-(((bins[index]+bins[indexNext])/2)-bins[index]);
      
      printf("%d (%d) %g||%g||%g\n",i,index,minValue,bins[i],maxValue);
    }		
}

/*				MIN_BIN_NUM
    Returns the index in the array of bins
    of the "smallest" bin.  aka, the bin
    whose midpoint is the smallest.
*/
int min_Bin_Num(float* bins, int numBins){
    
    int i;
    int minIndex=0;
    float minValue=bins[0];
    for (i = 0; i < numBins; i++) {   
      if (minValue > bins[i]) {
        minValue = bins[i];
        minIndex = i;
      }
    }
    return minIndex;
}


/*				GEN_HAMMING
    given data from reading in a section of a sound file
    applies the hamming function to each sample.
    n specifies the length of in and out.
*/
void gen_Hamming(float* h, int n)
{
    int k;
    for (k = 0; k < n; k++) {
      float cos_value = (float) cos(2.0 * M_PI * k * (1.0 / n));
        h[k] = 0.54F + (-0.46F * cos_value);
    }
}

/*				NEXTPOWEROF2
    given an int n, finds the next power of 2 larger than
    or equal to n.
*/
int nextPowerOf2(int n)
{
    int result = 1;
    while (result < n) result = (result << 1);
    return result;
}


// normalize a chroma vector (from audio or midi) to have
// mean of 0 and std. dev. of 1
//
static void normalize(float *cv)
{
    float avg = 0;

    for (int i = 0; i < CHROMA_BIN_COUNT; i++) {
        avg += cv[i];
    }
    avg /= CHROMA_BIN_COUNT;

    /* Normalize this frame to avg. 0 */
    for (int i = 0; i < CHROMA_BIN_COUNT; i++)
        cv[i] -= avg;

    /* Calculate std. dev. for this frame */
    float sum = 0;
    for (int i = 0; i < CHROMA_BIN_COUNT; i++) {
        float x = cv[i];
        sum += x * x;
    }
    float dev = sqrt(sum / CHROMA_BIN_COUNT);
    if (dev == 0.0) dev = 1.0F; /* don't divide by zero */

    /* Normalize this frame to std. dev. 1*/
    for (int i = 0; i < CHROMA_BIN_COUNT; i++) cv[i] /= dev;
}


/* GEN_CHROMA_AUDIO -- compute chroma for an audio file 
 */
/*
    generates the chroma energy for a given sequence
    with a low cutoff and high cutoff.  
    The chroma energy is placed in the float *chrom_energy.
    this 2D is an array of pointers.
    The function returns the number of frames 
    (aka the length of the 1st dimention of chrom_energy)
*/
int Scorealign::gen_chroma_audio(Audio_reader &reader, int hcutoff, 
        int lcutoff, float **chrom_energy, double *actual_frame_period, 
        int id)
{
    int i;
    double sample_rate = reader.get_sample_rate();
    float reg11[CHROMA_BIN_COUNT]; // temp storage1;
    float reg12[CHROMA_BIN_COUNT]; // temp storage2;

    if (verbose) {
        printf ("==============FILE %d====================\n", id);
        reader.print_info();
    }
#if DEBUG_LOG
    fprintf(dbf, "******** BEGIN AUDIO CHROMA COMPUTATION *********\n");
#endif
    // this seems like a poor way to set actual_frame_period_0 or _1 in 
    // the Scorealign object, but I'm not sure what would be better:
    *actual_frame_period = float(reader.actual_frame_period);

    for (i = 0; i < CHROMA_BIN_COUNT; i++) {
        reg11[i] = -999;
      }
    for (i = 0; i < CHROMA_BIN_COUNT; i++){
        reg12[i] = 0;
      }

   /*=============================================================*/

    // allocate some buffers for use in the loop
    int full_data_size = nextPowerOf2(reader.samples_per_frame);
    if (verbose) {
        printf("   samples per frame is %ld \n", reader.samples_per_frame);
        printf("   total chroma frames %ld\n", reader.frame_count); 
        // printf("   Window size  %g second \n", reader.window_size);
        printf("   hopsize in samples %ld \n", reader.hop_samples);
        printf("   fft size %d\n", full_data_size);
    }

    float *full_data = ALLOC(float, full_data_size);
    float *fft_dataR = ALLOC(float, full_data_size);
    float *fft_dataI = ALLOC(float, full_data_size);	
    //set to zero
    memset(full_data, 0, full_data_size * sizeof(float));
    memset(fft_dataR, 0, full_data_size * sizeof(float));	
    memset(fft_dataI, 0, full_data_size * sizeof(float));
    //check to see if memory has been allocated
    assert(full_data != NULL);
    assert(fft_dataR != NULL);
    assert(fft_dataI != NULL);
   
    int *bin_map = ALLOC(int, full_data_size);
	
    //set up the chrom_energy array;
    *chrom_energy = ALLOC(float, reader.frame_count * (CHROMA_BIN_COUNT + 1));
    int cv_index = 0;

    // set up mapping from spectral bins to chroma bins
    // ordinarily, we would add 0.5 to round to nearest bin, but we also
    // want to subtract 0.5 because the bin has a width of +/- 0.5. These
    // two cancel out, so we can just round down and get the right answer.
    int num_bins_to_use = (int) (hcutoff * full_data_size / sample_rate);
    // But then we want to add 1 because the loops will only go to 
    // high_bin - 1:
    int high_bin = min(num_bins_to_use + 1, full_data_size);
    //printf("center freq of high bin is %g\n", (high_bin - 1) * sample_rate / 
    //    full_data_size);
    //printf("high freq of high bin is %g\n", 
    //     (high_bin - 1 + 0.5) * sample_rate / full_data_size);
    // If we add 0.5, we'll round to nearest bin center frequency, but
    // bin covers a frequency range that goes 0.5 bin width lower, so we
    // add 1 before rounding.
    int low_bin = (int) (lcutoff * full_data_size / sample_rate);
    //printf("center freq of low bin is %g\n", low_bin * sample_rate / 
    //    full_data_size);
    //printf("low freq of low bin is %g\n", (low_bin - 0.5) * sample_rate / 
    //    full_data_size);
    //printf("frequency spacing of bins is %g\n", 
    //     sample_rate / full_data_size);
    double freq = low_bin * sample_rate / full_data_size;
    for (i = low_bin; i < high_bin; i++) {
        float raw_bin = hz_to_step(float(freq));
        int round_bin = (int) (raw_bin + 0.5F);
        int mod_bin = round_bin % 12;
        bin_map[i] = mod_bin;
        freq += sample_rate / full_data_size;
    }
    // printf("BIN_COUNT is !!!!!!!!!!!!!   %d\n",CHROMA_BIN_COUNT);

    // create Hamming window data
    float *hamming = ALLOC(float, reader.samples_per_frame);
    gen_Hamming(hamming, reader.samples_per_frame);

    while (reader.read_window(full_data)) {
        //fill out array with 0's till next power of 2
#ifdef SA_VERBOSE
        fprintf(dbf, "samples_per_frame %d sample %g\n", 
                reader.samples_per_frame, full_data[0]);
#endif
        for (i = reader.samples_per_frame; i < full_data_size; i++) 
            full_data[i] = 0;

#ifdef SA_VERBOSE
        fprintf(dbf, "preFFT: full_data[1000] %g\n", full_data[1000]);
#endif

        // compute the RMS, then apply the Hamming window to the data
        float rms = 0.0f;
        for (i = 0; i < reader.samples_per_frame; i++) {
            float x = full_data[i];
            rms += x * x;
            full_data[i] = x * hamming[i];
        }
        rms = sqrt(rms / reader.samples_per_frame);

#ifdef SA_VERBOSE
        fprintf(dbf, "preFFT: hammingData[1000] %g\n", 
                full_data[1000]);
#endif
        FFT3(full_data_size, 0, full_data, NULL, fft_dataR, fft_dataI); //fft3
      
        //given the fft, compute the energy of each point
        gen_Magnitude(fft_dataR, fft_dataI, low_bin, high_bin, full_data);
      
        /*-------------------------------------
          GENERATE BINS AND PUT
          THE CORRECT ENERGY IN
          EACH BIN, CORRESPONDING
          TO THE CORRECT PITCH
          -------------------------------------*/

        float binEnergy[CHROMA_BIN_COUNT];
        int binCount[CHROMA_BIN_COUNT];

        for (i = 0; i < CHROMA_BIN_COUNT; i++) {
            binCount[i] = 0; 
            binEnergy[i] = 0.0;
        }
      
        for (i = low_bin; i < high_bin; i++) {
            int mod_bin = bin_map[i];
            binEnergy[mod_bin] += full_data[i];
            binCount[mod_bin]++;
        }

        /*-------------------------------------
          END OF BIN GENERATION
          -------------------------------------*/
        /* THE FOLLOWING LOOKS LIKE SOME OLD CODE TO COMPUTE
         * CHROMA FLUX, BUT IT IS NOT IN USE NOW 
         
        if (PRINT_BIN_ENERGY) {
            float mao1;
            float sum=0.;
         
            for (i = 0; i < CHROMA_BIN_COUNT; i++) {
                reg12[i]=binEnergy[i] / binCount[i];
            }
       
            if (reg11[0]==-999){
                printf("Chroma Flux \n\n");
            } else {
                for (i = 0; i < CHROMA_BIN_COUNT; i++) {
                }
                for (int k = 0; k < CHROMA_BIN_COUNT; k++) {
                    float x = reg11[k];
                    float y = reg12[k];
                    float diff = x - y;
                    sum += diff * diff;
                }
                mao1 = sqrt(sum);         
                sequence++;      
                sum = 0.;
                mao1 = 0.;
            }
            for (i = 0; i < CHROMA_BIN_COUNT; i++) {
                reg11[i]=reg12[i];
            }
            //fclose(Pointer);
          }
        */
        //put chrom energy into the returned array

#ifdef SA_VERBOSE
        fprintf(dbf, "cv_index %d\n", cv_index);
#endif
        assert(cv_index < reader.frame_count);
        float *cv = AREF1(*chrom_energy, cv_index);
        for (i = 0;  i < CHROMA_BIN_COUNT; i++) {
            cv[i] = binEnergy[i] / binCount[i];
        }
        if (rms < silence_threshold) {
            // "silence" flag
            cv[CHROMA_BIN_COUNT] = 1.0f;
        } else {
            cv[CHROMA_BIN_COUNT] = 0.0f;
            // normalize the non-silent frames
            normalize(cv);
        }
#if DEBUG_LOG
        fprintf(dbf, "%d@%g) ", cv_index, cv_index * reader.actual_frame_period);
        for (int i = 0; i < CHROMA_BIN_COUNT; i++) {
          fprintf(dbf, "%d:%g ", i, cv[i]);
        }
        fprintf(dbf, " sil?:%g\n\n", cv[CHROMA_BIN_COUNT]);
#endif
        cv_index++;
        if (progress && cv_index % 10 == 0 && 
            !progress->set_feature_progress(
                    float(cv_index * reader.actual_frame_period))) {
            break;
        }
    } // end of while ((readcount = read_mono_floats...

    free(hamming);
    free(fft_dataI);
    free(fft_dataR);
    free(full_data);
    if (verbose)
        printf("\nGenerated Chroma. file%d_frames is %i\n", id, file0_frames);
    return cv_index;
}


class Event_list {
public:
	Alg_note_ptr note;
	Event_list *next;

	Event_list(Alg_event_ptr event_, Event_list *next_) {
		note = (Alg_note_ptr) event_;
		next = next_;
	}

	~Event_list() {
	}
};
typedef Event_list *Event_list_ptr;


/* gen_chroma_midi -- generate chroma vectors for midi file */
/*
    generates the chroma energy for a given sequence
    with a low cutoff and high cutoff.  
    The chroma energy is placed in the float *chrom_energy.
    this 2D is an array of pointers.
    The function returns the number of frames 
    (aka the length of the 1st dimension of chrom_energy)
 *
 *
  Notes: keep a list of notes that are sounding.
  For each frame, 
    zero the vector
    while next note starts before end of frame, insert note in list
	  for each note in list, compute weight and add to vector. Remove
	  if note ends before frame start time.	 
  How many frames? 
 */

int Scorealign::gen_chroma_midi(Alg_seq &seq, float dur, int nnotes,
                    int hcutoff, int lcutoff,
                    float **chrom_energy, double *actual_frame_period,
                    int id)
{	
    // silence_threshold is compared to the *average* of chroma bins.
    // Rather than divide the sum by CHROMA_BIN_COUNT to compute the
    // average, just compute the sum and compare to silence_threshold * 12
    float threshold = (float) (silence_threshold * CHROMA_BIN_COUNT);

    if (verbose) {
        printf ("==============FILE %d====================\n", id);
        SA_V(seq.write(cout, true));
    }
#if DEBUG_LOG
    fprintf(dbf, "******** BEGIN MIDI CHROMA COMPUTATION *********\n");
#endif    /*=============================================================*/

    *actual_frame_period = frame_period; // since we don't quantize to samples
	
    /*=============================================================*/
    
    seq.convert_to_seconds();
    ///* find duration */
    //float dur = 0.0F;
    //int nnotes = 0;
    //nnotes = find_midi_duration(seq, &dur); 

    /*================================================================*/
	
    int frame_count= (int)ceil(((float)dur/ frame_period + 1)); 	
	
    /*================================================================*/
	
    if (verbose) {
        printf("   note count = %d\n", nnotes);
        printf("   duration in sec = %f\n", dur); 
        printf("   chroma frames %d\n", frame_count);
    }

    //set up the chrom_energy array;
    (*chrom_energy) = ALLOC(float, frame_count * (CHROMA_BIN_COUNT + 1));
    Event_list_ptr list = NULL;
    Alg_iterator iterator(&seq, false);
    iterator.begin();
    Alg_event_ptr event = iterator.next();
    int cv_index;
    for (cv_index = 0; cv_index < frame_count; cv_index++) {
		
        /*====================================================*/

        float frame_begin = (float) max(cv_index * frame_period - 
                                        window_size / 2.0, 0.0); 
        //chooses zero if negative

        float frame_end = (float) (cv_index * frame_period + window_size / 2.0);
	/*============================================================*/
        float *cv = AREF1(*chrom_energy, cv_index);
        /* zero the vector */
        for (int i = 0; i < CHROMA_BIN_COUNT + 1; i++) cv[i] = 0;
        /* add new notes that are in the frame */
        while (event && event->time < frame_end) {
            if (event->is_note()) {
                list = new Event_list(event, list);
            }
            event = iterator.next();
        }
        /* remove notes that are no longer sounding */
        Event_list_ptr *ptr = &list;
        while (*ptr) {
            while ((*ptr) && 
                   (*ptr)->note->time + (*ptr)->note->dur < frame_begin) {
                Event_list_ptr temp = *ptr;
                *ptr = (*ptr)->next;
                delete temp;
            }
            if (*ptr) ptr = &((*ptr)->next);
        }
        float sum = 0.0;
        for (Event_list_ptr item = list; item; item = item->next) {
            /* compute duration of overlap */
            float overlap = 
                min(frame_end, (float) (item->note->time + item->note->dur)) - 
                max(frame_begin, (float) item->note->time);
            float velocity = item->note->loud;
            float weight = overlap * velocity;
#if DEBUG_LOG
            fprintf(dbf, "%3d pitch %g starting %g key %ld overlap %g velocity %g\n", 
                    cv_index, item->note->pitch, item->note->time, 
                    item->note->get_identifier(), overlap, velocity);
#endif
            cv[(int) item->note->pitch % 12] += weight;
            sum += weight;
        }


        if (sum < threshold) {
            cv[CHROMA_BIN_COUNT] = 1.0;
        } else {
            normalize(cv);
        }
          

#if DEBUG_LOG
        fprintf(dbf, "%d@%g) ", cv_index, frame_begin);
        for (int i = 0; i < CHROMA_BIN_COUNT; i++) {
            fprintf(dbf, "%d:%g ", i, cv[i]);
        }
        fprintf(dbf, " sil?:%g\n\n", cv[CHROMA_BIN_COUNT]);
#endif
        if (cv_index % 10 == 0 && progress && 
            !progress->set_feature_progress(
                    float(cv_index * *actual_frame_period))) {
            break;
        }
    }
    while (list) {
        Event_list_ptr temp = list;
        list = list->next;
        delete temp;
    }
    iterator.end();
    if (verbose)
        printf("\nGenerated Chroma. file%d_frames is %i\n", id, file0_frames);
    return frame_count;
}
