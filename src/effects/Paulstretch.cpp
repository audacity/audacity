/**********************************************************************

Audacity: A Digital Audio Editor

Paulstretch.cpp

Nasca Octavian Paul (Paul Nasca)
Some GUI code was taken from the Echo effect

 *******************************************************************/

/**

  \class EffectPaulstretch
  \brief An Extreme Time Stretch and Time Smear effect

*//****************************************************************/

/**

\class PaulstretchDialog
\brief PaulstretchDialog used with EffectPaulstretch

*/

/*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/validate.h>
#include <wx/valtext.h>

#include <wx/generic/textdlgg.h>
#include <wx/intl.h>
#include <math.h>
#include <stdlib.h>

#include "Paulstretch.h"
#include "../WaveTrack.h"
#include "../FFT.h"


EffectPaulstretch::EffectPaulstretch(){
   amount=10.0;
   time_resolution=0.25;
};

wxString EffectPaulstretch::GetEffectDescription(){
   // Note: This is useful only after values have been set.
   return wxString::Format(_("Applied effect: %s stretch factor = %f times, time resolution = %f seconds"),
         this->GetEffectName().c_str(), amount,time_resolution);

};

bool EffectPaulstretch::PromptUser(){
   PaulstretchDialog dlog(this, mParent);
   dlog.amount = amount;
   dlog.time_resolution = time_resolution;
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   amount = dlog.amount;
   time_resolution = dlog.time_resolution;

   return true;
};

bool EffectPaulstretch::TransferParameters(Shuttle &shuttle){
   shuttle.TransferFloat(wxT("Stretch Factor"),amount,10.0);
   shuttle.TransferFloat(wxT("Time Resolution"),time_resolution,0.25);

   return true;
};


bool EffectPaulstretch::Process(){
   CopyInputTracks();
   SelectedTrackListOfKindIterator iter(Track::Wave, mOutputTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   m_t1=mT1;
   int count=0;
   while (track) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      double t1 = mT1 > trackEnd? trackEnd: mT1;

      if (t1 > t0) {
         if (!ProcessOne(track, t0,t1,count))
            return false;
      }

      track = (WaveTrack *) iter.Next();
      count++;
   }
   mT1=m_t1;

   ReplaceProcessedTracks(true);

   return true;
}

class PaulStretch{
   public:
      PaulStretch(float rap_,int in_bufsize_,float samplerate_);
      //in_bufsize is also a half of a FFT buffer (in samples)
      virtual ~PaulStretch();

      void process(float *smps,int nsmps);

      int in_bufsize;
      int poolsize;//how many samples are inside the input_pool size (need to know how many samples to fill when seeking)

      int out_bufsize;
      float *out_buf;

      int get_nsamples();//how many samples are required to be added in the pool next time
      int get_nsamples_for_fill();//how many samples are required to be added for a complete buffer refill (at start of the song or after seek)

      void set_rap(float newrap);//set the current stretch value


   protected:
      virtual void process_spectrum(float *WXUNUSED(freq)){};
      float samplerate;
   private:
      float *in_pool;//de marimea in_bufsize
      float rap;
      float *old_out_smp_buf;

      float *fft_smps,*fft_c,*fft_s,*fft_freq,*fft_tmp;

      double remained_samples;//how many fraction of samples has remained (0..1)
};


bool EffectPaulstretch::ProcessOne(WaveTrack *track,double t0,double t1,int count){

   int stretch_buf_size;//must be power of 2 (because Audacity's fft requires it)
   if (time_resolution<0.001) time_resolution=0.001f;
   {
      float tmp=track->GetRate()*time_resolution*0.5;
      tmp=log(tmp)/log(2.0);
      tmp=pow(2.0,floor(tmp+0.5));
      stretch_buf_size=(int)tmp;
   };
   if (stretch_buf_size<128) stretch_buf_size=128;
   double amount=this->amount;
   if (amount<1.0) amount=1.0;

   sampleCount start = track->TimeToLongSamples(t0);
   sampleCount end = track->TimeToLongSamples(t1);
   sampleCount len = (sampleCount)(end - start);

   m_t1=mT1;

   if (len<=(stretch_buf_size*2+1)){//error because the selection is too short
      /* i18n-hint: This is an effect error message, for the effect named Paulstretch.
       * Time Resolution is a parameter of the effect, the translation should match
       */
      ::wxMessageBox(_("Error in Paulstretch:\nThe selection is too short.\n It must be much longer than the Time Resolution."));
      return false;
   };


   double adjust_amount=(double)len/((double)len-((double)stretch_buf_size*2.0));
   amount=1.0+(amount-1.0)*adjust_amount;

   WaveTrack * outputTrack = mFactory->NewWaveTrack(track->GetSampleFormat(),track->GetRate());

   PaulStretch *stretch=new PaulStretch(amount,stretch_buf_size,track->GetRate());

   sampleCount nget=stretch->get_nsamples_for_fill();

   int bufsize=stretch->poolsize;
   float *buffer0=new float[bufsize];
   float *bufferptr0=buffer0;
   sampleCount outs=0;
   bool first_time=true;

   int fade_len=100;
   if (fade_len>(bufsize/2-1)) fade_len=bufsize/2-1;
   float *fade_track_smps=new float[fade_len];
   sampleCount s=0;
   bool cancelled=false;

   while (s<len){
      track->Get((samplePtr)bufferptr0,floatSample,start+s,nget);
      stretch->process(buffer0,nget);

      if (first_time) {
         stretch->process(buffer0,0);
      };

      outs+=stretch->out_bufsize;
      s+=nget;

      if (first_time){//blend the the start of the selection
         track->Get((samplePtr)fade_track_smps,floatSample,start,fade_len);
         first_time=false;
         for (int i=0;i<fade_len;i++){
            float fi=(float)i/(float)fade_len;
            stretch->out_buf[i]=stretch->out_buf[i]*fi+(1.0-fi)*fade_track_smps[i];
         };
      };
      if (s>=len){//blend the end of the selection
         track->Get((samplePtr)fade_track_smps,floatSample,end-fade_len,fade_len);
         for (int i=0;i<fade_len;i++){
            float fi=(float)i/(float)fade_len;
            int i2=bufsize/2-1-i;
            stretch->out_buf[i2]=stretch->out_buf[i2]*fi+(1.0-fi)*fade_track_smps[fade_len-1-i];
         };
      };

      outputTrack->Append((samplePtr)stretch->out_buf,floatSample,stretch->out_bufsize);

      nget=stretch->get_nsamples();
      if (TrackProgress(count, (s / (double) len))) {
         cancelled=true;
         break;
      };
   };

   delete [] fade_track_smps;
   outputTrack->Flush();



   track->Clear(t0,t1);
   track->Paste(t0,outputTrack);
   if (!cancelled){
      double flen=t1-t0;
      if (s>0) m_t1=t0+flen*(double)outs/(double)(s);
   };


   delete stretch;
   delete []buffer0;

   delete outputTrack;
   return !cancelled;
};



/*************************************************************/




PaulStretch::PaulStretch(float rap_,int in_bufsize_,float samplerate_){
   samplerate=samplerate_;
   rap=rap_;
   in_bufsize=in_bufsize_;
   if (rap<1.0) rap=1.0;
   out_bufsize=in_bufsize;
   if (out_bufsize<8) out_bufsize=8;

   out_buf=new float[out_bufsize];
   old_out_smp_buf=new float[out_bufsize*2];for (int i=0;i<out_bufsize*2;i++) old_out_smp_buf[i]=0.0;

   poolsize=in_bufsize_*2;
   in_pool=new float[poolsize];for (int i=0;i<poolsize;i++) in_pool[i]=0.0;

   remained_samples=0.0;

   fft_smps=new float[poolsize];
   fft_s=new float[poolsize];
   fft_c=new float[poolsize];
   fft_freq=new float[poolsize];
   fft_tmp=new float[poolsize];
   for (int i=0;i<poolsize;i++) {
      fft_smps[i]=0.0;
      fft_c[i]=0.0;
      fft_s[i]=0.0;
      fft_freq[i]=0.0;
   };

};

PaulStretch::~PaulStretch(){
   delete [] out_buf;
   delete [] old_out_smp_buf;
   delete [] in_pool;
   delete [] fft_smps;
   delete [] fft_c;
   delete [] fft_s;
   delete [] fft_freq;
   delete [] fft_tmp;
};

void PaulStretch::set_rap(float newrap){
   if (rap>=1.0) rap=newrap;
   else rap=1.0;
};

void PaulStretch::process(float *smps,int nsmps){
   //add new samples to the pool
   if ((smps!=NULL)&&(nsmps!=0)){
      if (nsmps>poolsize){
         nsmps=poolsize;
      };
      int nleft=poolsize-nsmps;

      //move left the samples from the pool to make room for new samples
      for (int i=0;i<nleft;i++) in_pool[i]=in_pool[i+nsmps];

      //add new samples to the pool
      for (int i=0;i<nsmps;i++) in_pool[i+nleft]=smps[i];
   };

   //get the samples from the pool
   for (int i=0;i<poolsize;i++) fft_smps[i]=in_pool[i];
   WindowFunc(3,poolsize,fft_smps);

   RealFFT(poolsize,fft_smps,fft_c,fft_s);

   for (int i=0;i<poolsize/2;i++) fft_freq[i]=sqrt(fft_c[i]*fft_c[i]+fft_s[i]*fft_s[i]);
   process_spectrum(fft_freq);


   //put randomize phases to frequencies and do a IFFT
   float inv_2p15_2pi=1.0/16384.0*(float)M_PI;
   for (int i=1;i<poolsize/2;i++){
      unsigned int random=(rand())&0x7fff;
      float phase=random*inv_2p15_2pi;
      float s=fft_freq[i]*sin(phase);
      float c=fft_freq[i]*cos(phase);


      fft_c[i]=fft_c[poolsize-i]=c;

      fft_s[i]=s;fft_s[poolsize-i]=-s;
   };
   fft_c[0]=fft_s[0]=0.0;
   fft_c[poolsize/2]=fft_s[poolsize/2]=0.0;

   FFT(poolsize,true,fft_c,fft_s,fft_smps,fft_tmp);

   float max=0.0,max2=0.0;
   for (int i=0;i<poolsize;i++){
      float a=fabs(fft_tmp[i]);
      if (a>max) max=a;
      float b=fabs(fft_smps[i]);
      if (b>max2) max2=b;
   };


   //make the output buffer
   float tmp=1.0/(float) out_bufsize*M_PI;
   float hinv_sqrt2=0.853553390593f;//(1.0+1.0/sqrt(2))*0.5;

   float ampfactor=1.0;
   if (rap<1.0) ampfactor=rap*0.707;
   else ampfactor=(out_bufsize/(float)poolsize)*4.0;

   for (int i=0;i<out_bufsize;i++) {
      float a=(0.5+0.5*cos(i*tmp));
      float out=fft_smps[i+out_bufsize]*(1.0-a)+old_out_smp_buf[i]*a;
      out_buf[i]=out*(hinv_sqrt2-(1.0-hinv_sqrt2)*cos(i*2.0*tmp))*ampfactor;
   };

   //copy the current output buffer to old buffer
   for (int i=0;i<out_bufsize*2;i++) old_out_smp_buf[i]=fft_smps[i];

};


int PaulStretch::get_nsamples(){
   double r=out_bufsize/rap;
   int ri=(int)floor(r);
   double rf=r-floor(r);

   remained_samples+=rf;
   if (remained_samples>=1.0){
      ri+=(int)floor(remained_samples);
      remained_samples=remained_samples-floor(remained_samples);
   };

   if (ri>poolsize){
      ri=poolsize;
   };

   return ri;
};

int PaulStretch::get_nsamples_for_fill(){
   return poolsize;
};

   BEGIN_EVENT_TABLE(PaulstretchDialog, EffectDialog)
   EVT_BUTTON(ID_EFFECT_PREVIEW, PaulstretchDialog::OnPreview)
END_EVENT_TABLE()

   PaulstretchDialog::PaulstretchDialog(EffectPaulstretch *effect, wxWindow *parent):EffectDialog(parent,wxT("Paulstretch")){
      m_bLoopDetect=false;
      m_pEffect=effect;

      m_pTextCtrl_Amount=NULL;
      m_pTextCtrl_TimeResolution=NULL;

      amount=10.0;
      time_resolution=0.25;

      Init();
   };


void PaulstretchDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      /* i18n-hint: This is how many times longer the sound will be, e.g. applying
       * the effect to a 1-second sample, with the default Stretch Factor of 10.0
       * will give an (approximately) 10 second sound
       */
      m_pTextCtrl_Amount = S.AddTextBox(_("Stretch Factor:"),wxT("10.0"),10);
      m_pTextCtrl_Amount->SetValidator(wxTextValidator(wxFILTER_NUMERIC));

      m_pTextCtrl_TimeResolution= S.AddTextBox(_("Time Resolution (seconds):"), wxT("0.25"),10);
      m_pTextCtrl_TimeResolution->SetValidator(wxTextValidator(wxFILTER_NUMERIC));
   }
   S.EndMultiColumn();

};

bool PaulstretchDialog::TransferDataToWindow(){
   m_bLoopDetect = true;

   wxString str;
   if (m_pTextCtrl_Amount) {
      str.Printf(wxT("%g"), amount);
      m_pTextCtrl_Amount->SetValue(str);
   }
   if (m_pTextCtrl_TimeResolution) {
      str.Printf(wxT("%g"), time_resolution);
      m_pTextCtrl_TimeResolution->SetValue(str);
   }

   m_bLoopDetect = false;
   return true;
}
bool PaulstretchDialog::TransferDataFromWindow(){
   double newValue;
   wxString str;
   if (m_pTextCtrl_Amount) {
      str = m_pTextCtrl_Amount->GetValue();
      str.ToDouble(&newValue);
      amount = (float)(newValue);
   }
   if (m_pTextCtrl_TimeResolution) {
      str = m_pTextCtrl_TimeResolution->GetValue();
      str.ToDouble(&newValue);
      time_resolution = (float)(newValue);
   }
   return true;
}

void PaulstretchDialog::OnPreview(wxCommandEvent & WXUNUSED(event)){
   TransferDataFromWindow();

   // Save & restore parameters around Preview, because we didn't do OK.
   float oldAmount = m_pEffect->amount;
   float oldTimeResolution = m_pEffect->time_resolution;

   m_pEffect->amount = amount;
   m_pEffect->time_resolution = time_resolution;

   m_pEffect->Preview();

   m_pEffect->amount = oldAmount;
   m_pEffect->time_resolution = oldTimeResolution;
}



