/********************************************************************
 *                                                                  *
 * THIS FILE IS PART OF THE OggVorbis SOFTWARE CODEC SOURCE CODE.   *
 * USE, DISTRIBUTION AND REPRODUCTION OF THIS LIBRARY SOURCE IS     *
 * GOVERNED BY A BSD-STYLE SOURCE LICENSE INCLUDED WITH THIS SOURCE *
 * IN 'COPYING'. PLEASE READ THESE TERMS BEFORE DISTRIBUTING.       *
 *                                                                  *
 * THE OggVorbis SOURCE CODE IS (C) COPYRIGHT 1994-2007             *
 * by the Xiph.Org Foundation http://www.xiph.org/                  *
 *                                                                  *
 ********************************************************************

 function: illustrate seeking, and test it too
 last mod: $Id: seeking_example.c 17561 2010-10-23 10:34:24Z xiphmont $

 ********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include "vorbis/codec.h"
#include "vorbis/vorbisfile.h"

#ifdef _WIN32 /* We need the following two to set stdin/stdout to binary */
# include <io.h>
# include <fcntl.h>
#endif

void _verify(OggVorbis_File *ov,
             ogg_int64_t val,ogg_int64_t pcmval,double timeval,
             ogg_int64_t pcmlength,
             char *bigassbuffer){
  off_t i;
  int j;
  long bread;
  char buffer[4096];
  int dummy;
  ogg_int64_t pos;
  int hs = ov_halfrate_p(ov);

  /* verify the raw position, the pcm position and position decode */
  if(val!=-1 && ov_raw_tell(ov)<val){
    fprintf(stderr,"raw position out of tolerance: requested %ld, got %ld\n",
           (long)val,(long)ov_raw_tell(ov));
    exit(1);
  }
  if(pcmval!=-1 && ov_pcm_tell(ov)>pcmval){
    fprintf(stderr,"pcm position out of tolerance: requested %ld, got %ld\n",
           (long)pcmval,(long)ov_pcm_tell(ov));
    exit(1);
  }
  if(timeval!=-1 && ov_time_tell(ov)>timeval){
    fprintf(stderr,"time position out of tolerance: requested %f, got %f\n",
           timeval,ov_time_tell(ov));
    exit(1);
  }
  pos=ov_pcm_tell(ov);
  if(pos<0 || pos>pcmlength){
    fprintf(stderr,"pcm position out of bounds: got %ld\n",(long)pos);
    exit(1);
  }
  bread=ov_read(ov,buffer,4096,1,1,1,&dummy);
  for(j=0;j<bread;j++){
    if(buffer[j]!=bigassbuffer[j+((pos>>hs)*2)]){
      fprintf(stderr,"data after seek doesn't match declared pcm position %ld\n",(long)pos);

      for(i=0;i<(pcmlength>>hs)*2-bread;i++){
        for(j=0;j<bread;j++)
          if(buffer[j] != bigassbuffer[i+j])break;
        if(j==bread){
          fprintf(stderr,"data after seek appears to match position %ld\n",(long)((i/2)<<hs));
        }
      }
      {
        FILE *f=fopen("a.m","w");
        for(j=0;j<bread;j++)fprintf(f,"%d %d\n",j,(int)buffer[j]);
        fclose(f);
        f=fopen("b.m","w");
        for(j=-4096;j<bread+4096;j++)
          if(j+((pos*2)>>hs)>=0 && (j+((pos*2)>>hs))<(pcmlength>>hs)*2)
             fprintf(f,"%d %d\n",j,(int)bigassbuffer[j+((pos*2)>>hs)]);
        fclose(f);
      }

      exit(1);
    }
  }
}

int main(){
  OggVorbis_File ov;
  int i,ret;
  ogg_int64_t pcmlength;
  double timelength;
  char *bigassbuffer;
  int dummy;
  int hs=0;

#ifdef _WIN32 /* We need to set stdin/stdout to binary mode. Damn windows. */
  _setmode( _fileno( stdin ), _O_BINARY );
#endif


  /* open the file/pipe on stdin */
  if(ov_open_callbacks(stdin,&ov,NULL,-1,OV_CALLBACKS_NOCLOSE)<0){
    fprintf(stderr,"Could not open input as an OggVorbis file.\n\n");
    exit(1);
  }

#if 0 /*enable this code to test seeking with halfrate decode */
  if(ov_halfrate(&ov,1)){
    fprintf(stderr,"Sorry; unable to set half-rate decode.\n\n");
    exit(1);
  }else
    hs=1;
#endif

  if(ov_seekable(&ov)){

    /* to simplify our own lives, we want to assume the whole file is
       stereo.  Verify this to avoid potentially mystifying users
       (pissing them off is OK, just don't confuse them) */
    for(i=0;i<ov.links;i++){
      vorbis_info *vi=ov_info(&ov,i);
      if(vi->channels!=2){
        fprintf(stderr,"Sorry; right now seeking_test can only use Vorbis files\n"
               "that are entirely stereo.\n\n");
        exit(1);
      }
    }
    
    /* because we want to do sample-level verification that the seek
       does what it claimed, decode the entire file into memory */
    pcmlength=ov_pcm_total(&ov,-1);
    timelength=ov_time_total(&ov,-1);
    bigassbuffer=malloc((pcmlength>>hs)*2); /* w00t */
    i=0;
    while(i<(pcmlength>>hs)*2){
      int ret=ov_read(&ov,bigassbuffer+i,((pcmlength>>hs)*2)-i,1,1,1,&dummy);
      if(ret<0){
        fprintf(stderr,"Error reading file.\n");
        exit(1);
      }
      if(ret){
        i+=ret;
      }else{
        pcmlength=(i/2)<<hs;
      }
      fprintf(stderr,"\rloading.... [%ld left]              ",
              (long)((pcmlength>>hs)*2-i));
    }
    
    {
      ogg_int64_t length=ov.end;
      fprintf(stderr,"\rtesting raw seeking to random places in %ld bytes....\n",
             (long)length);
    
      for(i=0;i<1000;i++){
        ogg_int64_t val=(double)rand()/RAND_MAX*length;
        fprintf(stderr,"\r\t%d [raw position %ld]...     ",i,(long)val);
        ret=ov_raw_seek(&ov,val);
        if(ret<0){
          fprintf(stderr,"seek failed: %d\n",ret);
          exit(1);
        }

        _verify(&ov,val,-1,-1.,pcmlength,bigassbuffer);

      }
    }

    fprintf(stderr,"\r");
    {
      fprintf(stderr,"testing pcm page seeking to random places in %ld samples....\n",
             (long)pcmlength);
    
      for(i=0;i<1000;i++){
        ogg_int64_t val=(double)rand()/RAND_MAX*pcmlength;
        fprintf(stderr,"\r\t%d [pcm position %ld]...     ",i,(long)val);
        ret=ov_pcm_seek_page(&ov,val);
        if(ret<0){
          fprintf(stderr,"seek failed: %d\n",ret);
          exit(1);
        }

        _verify(&ov,-1,val,-1.,pcmlength,bigassbuffer);

      }
    }

    fprintf(stderr,"\r");
    {
      fprintf(stderr,"testing pcm exact seeking to random places in %ld samples....\n",
             (long)pcmlength);
    
      for(i=0;i<1000;i++){
        ogg_int64_t val=(double)rand()/RAND_MAX*pcmlength;
        fprintf(stderr,"\r\t%d [pcm position %ld]...     ",i,(long)val);
        ret=ov_pcm_seek(&ov,val);
        if(ret<0){
          fprintf(stderr,"seek failed: %d\n",ret);
          exit(1);
        }
        if(ov_pcm_tell(&ov)!=((val>>hs)<<hs)){
          fprintf(stderr,"Declared position didn't perfectly match request: %ld != %ld\n",
                 (long)val,(long)ov_pcm_tell(&ov));
          exit(1);
        }

        _verify(&ov,-1,val,-1.,pcmlength,bigassbuffer);

      }
    }

    fprintf(stderr,"\r");
    {
      fprintf(stderr,"testing time page seeking to random places in %f seconds....\n",
             timelength);
    
      for(i=0;i<1000;i++){
        double val=(double)rand()/RAND_MAX*timelength;
        fprintf(stderr,"\r\t%d [time position %f]...     ",i,val);
        ret=ov_time_seek_page(&ov,val);
        if(ret<0){
          fprintf(stderr,"seek failed: %d\n",ret);
          exit(1);
        }

        _verify(&ov,-1,-1,val,pcmlength,bigassbuffer);

      }
    }

    fprintf(stderr,"\r");
    {
      fprintf(stderr,"testing time exact seeking to random places in %f seconds....\n",
             timelength);
    
      for(i=0;i<1000;i++){
        double val=(double)rand()/RAND_MAX*timelength;
        fprintf(stderr,"\r\t%d [time position %f]...     ",i,val);
        ret=ov_time_seek(&ov,val);
        if(ret<0){
          fprintf(stderr,"seek failed: %d\n",ret);
          exit(1);
        }
        if(ov_time_tell(&ov)<val-1 || ov_time_tell(&ov)>val+1){
          fprintf(stderr,"Declared position didn't perfectly match request: %f != %f\n",
                 val,ov_time_tell(&ov));
          exit(1);
        }

        _verify(&ov,-1,-1,val,pcmlength,bigassbuffer);

      }
    }
    
    fprintf(stderr,"\r                                           \nOK.\n\n");


  }else{
    fprintf(stderr,"Standard input was not seekable.\n");
  }

  ov_clear(&ov);
  return 0;
}













