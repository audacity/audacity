/********************************************************************
 *                                                                  *
 * THIS FILE IS PART OF THE OggVorbis SOFTWARE CODEC SOURCE CODE.   *
 * USE, DISTRIBUTION AND REPRODUCTION OF THIS LIBRARY SOURCE IS     *
 * GOVERNED BY A BSD-STYLE SOURCE LICENSE INCLUDED WITH THIS SOURCE *
 * IN 'COPYING'. PLEASE READ THESE TERMS BEFORE DISTRIBUTING.       *
 *                                                                  *
 * THE OggVorbis SOURCE CODE IS (C) COPYRIGHT 1994-2010             *
 * by the Xiph.Org Foundation http://www.xiph.org/                  *
 *                                                                  *
 ********************************************************************

 function: utility functions for loading .vqh and .vqd files
 last mod: $Id: bookutil.c 16959 2010-03-10 16:03:11Z xiphmont $

 ********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <errno.h>
#include "bookutil.h"

int _best(codebook *book, float *a, int step){

  int dim=book->dim;
  int i,j,o;
  int minval=book->minval;
  int del=book->delta;
  int qv=book->quantvals;
  int ze=(qv>>1);
  int index=0;
  /* assumes integer/centered encoder codebook maptype 1 no more than dim 8 */

  if(del!=1){
    for(i=0,o=step*(dim-1);i<dim;i++,o-=step){
      int v = ((int)rint(a[o])-minval+(del>>1))/del;
      int m = (v<ze ? ((ze-v)<<1)-1 : ((v-ze)<<1));
      index = index*qv+ (m<0?0:(m>=qv?qv-1:m));
    }
  }else{
    for(i=0,o=step*(dim-1);i<dim;i++,o-=step){
      int v = (int)rint(a[o])-minval;
      int m = (v<ze ? ((ze-v)<<1)-1 : ((v-ze)<<1));
      index = index*qv+ (m<0?0:(m>=qv?qv-1:m));
    }
  }

  if(book->c->lengthlist[index]<=0){
    const static_codebook *c=book->c;
    int best=-1;
    /* assumes integer/centered encoder codebook maptype 1 no more than dim 8 */
    int e[8]={0,0,0,0,0,0,0,0};
    int maxval = book->minval + book->delta*(book->quantvals-1);
    for(i=0;i<book->entries;i++){
      if(c->lengthlist[i]>0){
        float this=0;
        for(j=0;j<dim;j++){
          float val=(e[j]-a[j*step]);
          this+=val*val;
        }
        if(best==-1 || this<best){
          best=this;
          index=i;
        }
      }
      /* assumes the value patterning created by the tools in vq/ */
      j=0;
      while(e[j]>=maxval)
        e[j++]=0;
      if(e[j]>=0)
        e[j]+=book->delta;
      e[j]= -e[j];
    }
  }

  return index;
}

/* A few little utils for reading files */
/* read a line.  Use global, persistent buffering */
static char *linebuffer=NULL;
static int  lbufsize=0;
char *get_line(FILE *in){
  long sofar=0;
  if(feof(in))return NULL;

  while(1){
    int gotline=0;

    while(!gotline){
      if(sofar+1>=lbufsize){
        if(!lbufsize){  
          lbufsize=1024;
          linebuffer=_ogg_malloc(lbufsize);
        }else{
          lbufsize*=2;
          linebuffer=_ogg_realloc(linebuffer,lbufsize);
        }
      }
      {
        long c=fgetc(in);
        switch(c){
        case EOF:
          if(sofar==0)return(NULL);
          /* fallthrough correct */
        case '\n':
          linebuffer[sofar]='\0';
          gotline=1;
          break;
        default:
          linebuffer[sofar++]=c;
          linebuffer[sofar]='\0';
          break;
        }
      }
    }
    
    if(linebuffer[0]=='#'){
      sofar=0;
    }else{
      return(linebuffer);
    }
  }
}

/* read the next numerical value from the given file */
static char *value_line_buff=NULL;

int get_line_value(FILE *in,float *value){
  char *next;

  if(!value_line_buff)return(-1);

  *value=strtod(value_line_buff, &next);
  if(next==value_line_buff){
    value_line_buff=NULL;
    return(-1);
  }else{
    value_line_buff=next;
    while(*value_line_buff>44)value_line_buff++;
    if(*value_line_buff==44)value_line_buff++;
    return(0);
  }
}

int get_next_value(FILE *in,float *value){
  while(1){
    if(get_line_value(in,value)){
      value_line_buff=get_line(in);
      if(!value_line_buff)return(-1);
    }else{
      return(0);
    }
  }
}

int get_next_ivalue(FILE *in,long *ivalue){
  float value;
  int ret=get_next_value(in,&value);
  *ivalue=value;
  return(ret);
}

static float sequence_base=0.f;
static int v_sofar=0;
void reset_next_value(void){
  value_line_buff=NULL;
  sequence_base=0.f;
  v_sofar=0;
}

char *setup_line(FILE *in){
  reset_next_value();
  value_line_buff=get_line(in);
  return(value_line_buff);
}


int get_vector(codebook *b,FILE *in,int start, int n,float *a){
  int i;
  const static_codebook *c=b->c;

  while(1){

    if(v_sofar==n || get_line_value(in,a)){
      reset_next_value();
      if(get_next_value(in,a))
        break;
      for(i=0;i<start;i++){
        sequence_base=*a;
        get_line_value(in,a);
      }
    }

    for(i=1;i<c->dim;i++)
      if(get_line_value(in,a+i))
        break;
    
    if(i==c->dim){
      float temp=a[c->dim-1];
      for(i=0;i<c->dim;i++)a[i]-=sequence_base;
      if(c->q_sequencep)sequence_base=temp;
      v_sofar++;
      return(0);
    }
    sequence_base=0.f;
  }

  return(-1);
}

/* read lines fromt he beginning until we find one containing the
   specified string */
char *find_seek_to(FILE *in,char *s){
  rewind(in);
  while(1){
    char *line=get_line(in);
    if(line){
      if(strstr(line,s))
        return(line);
    }else
      return(NULL);
  }
}


/* this reads the format as written by vqbuild/latticebuild; innocent
   (legal) tweaking of the file that would not affect its valid
   header-ness will break this routine */

codebook *codebook_load(char *filename){
  codebook *b=_ogg_calloc(1,sizeof(codebook));
  static_codebook *c=(static_codebook *)(b->c=_ogg_calloc(1,sizeof(static_codebook)));
  int quant_to_read=0;
  FILE *in=fopen(filename,"r");
  char *line;
  long i;

  if(in==NULL){
    fprintf(stderr,"Couldn't open codebook %s\n",filename);
    exit(1);
  }

  /* find the codebook struct */
  find_seek_to(in,"static const static_codebook ");

  /* get the major important values */
  line=get_line(in);
  if(sscanf(line,"%ld, %ld,",
            &(c->dim),&(c->entries))!=2){
    fprintf(stderr,"1: syntax in %s in line:\t %s",filename,line);
    exit(1);
  }
  line=get_line(in);
  line=get_line(in);
  if(sscanf(line,"%d, %ld, %ld, %d, %d,",
            &(c->maptype),&(c->q_min),&(c->q_delta),&(c->q_quant),
            &(c->q_sequencep))!=5){
    fprintf(stderr,"1: syntax in %s in line:\t %s",filename,line);
    exit(1);
  }
  
  switch(c->maptype){
  case 0:
    quant_to_read=0;
    break;
  case 1:
    quant_to_read=_book_maptype1_quantvals(c);
    break;
  case 2:
    quant_to_read=c->entries*c->dim;
    break;
  }
    
  /* load the quantized entries */
  find_seek_to(in,"static const long _vq_quantlist_");
  reset_next_value();
  c->quantlist=_ogg_malloc(sizeof(long)*quant_to_read);
  for(i=0;i<quant_to_read;i++)
    if(get_next_ivalue(in,c->quantlist+i)){
      fprintf(stderr,"out of data while reading codebook %s\n",filename);
      exit(1);
    }
  
  /* load the lengthlist */
  find_seek_to(in,"_lengthlist");
  reset_next_value();
  c->lengthlist=_ogg_malloc(sizeof(long)*c->entries);
  for(i=0;i<c->entries;i++)
    if(get_next_ivalue(in,c->lengthlist+i)){
      fprintf(stderr,"out of data while reading codebook %s\n",filename);
      exit(1);
    }

  /* got it all */
  fclose(in);
  
  vorbis_book_init_encode(b,c);
  b->valuelist=_book_unquantize(c,c->entries,NULL);

  return(b);
}

void spinnit(char *s,int n){
  static int p=0;
  static long lasttime=0;
  long test;
  struct timeval thistime;

  gettimeofday(&thistime,NULL);
  test=thistime.tv_sec*10+thistime.tv_usec/100000;
  if(lasttime!=test){
    lasttime=test;

    fprintf(stderr,"%s%d ",s,n);

    p++;if(p>3)p=0;
    switch(p){
    case 0:
      fprintf(stderr,"|    \r");
      break;
    case 1:
      fprintf(stderr,"/    \r");
      break;
    case 2:
      fprintf(stderr,"-    \r");
      break;
    case 3:
      fprintf(stderr,"\\    \r");
      break;
    }
    fflush(stderr);
  }
}

void build_tree_from_lengths(int vals, long *hist, long *lengths){
  int i,j;
  long *membership=_ogg_malloc(vals*sizeof(long));
  long *histsave=alloca(vals*sizeof(long));
  memcpy(histsave,hist,vals*sizeof(long));

  for(i=0;i<vals;i++)membership[i]=i;

  /* find codeword lengths */
  /* much more elegant means exist.  Brute force n^2, minimum thought */
  for(i=vals;i>1;i--){
    int first=-1,second=-1;
    long least=-1;
        
    spinnit("building... ",i);
    
    /* find the two nodes to join */
    for(j=0;j<vals;j++)
      if(least==-1 || hist[j]<=least){
        least=hist[j];
        first=membership[j];
      }
    least=-1;
    for(j=0;j<vals;j++)
      if((least==-1 || hist[j]<=least) && membership[j]!=first){
        least=hist[j];
        second=membership[j];
      }
    if(first==-1 || second==-1){
      fprintf(stderr,"huffman fault; no free branch\n");
      exit(1);
    }
    
    /* join them */
    least=hist[first]+hist[second];
    for(j=0;j<vals;j++)
      if(membership[j]==first || membership[j]==second){
        membership[j]=first;
        hist[j]=least;
        lengths[j]++;
      }
  }
  for(i=0;i<vals-1;i++)
    if(membership[i]!=membership[i+1]){
      fprintf(stderr,"huffman fault; failed to build single tree\n");
      exit(1);
    }

  /* for sanity check purposes: how many bits would it have taken to
     encode the training set? */
  {
    long bitsum=0;
    long samples=0;
    for(i=0;i<vals;i++){
      bitsum+=(histsave[i]-1)*lengths[i];
      samples+=histsave[i]-1;
    }

    if(samples){
      fprintf(stderr,"\rTotal samples in training set: %ld      \n",samples);
      fprintf(stderr,"\rTotal bits used to represent training set: %ld\n",
              bitsum);
    }
  }

  free(membership);
}

/* wrap build_tree_from_lengths to allow zero entries in the histogram */
void build_tree_from_lengths0(int vals, long *hist, long *lengths){

  /* pack the 'sparse' hit list into a dense list, then unpack
     the lengths after the build */

  int upper=0,i;
  long *lengthlist=_ogg_calloc(vals,sizeof(long));
  long *newhist=alloca(vals*sizeof(long));

  for(i=0;i<vals;i++)
    if(hist[i]>0)
      newhist[upper++]=hist[i];

  if(upper != vals){
    fprintf(stderr,"\rEliminating %d unused entries; %d entries remain\n",
            vals-upper,upper);
  }
    
  build_tree_from_lengths(upper,newhist,lengthlist);
      
  upper=0;
  for(i=0;i<vals;i++)
    if(hist[i]>0)
      lengths[i]=lengthlist[upper++];
    else
      lengths[i]=0;

  free(lengthlist);
}

void write_codebook(FILE *out,char *name,const static_codebook *c){
  int i,j,k;

  /* save the book in C header form */

  /* first, the static vectors, then the book structure to tie it together. */
  /* quantlist */
  if(c->quantlist){
    long vals=(c->maptype==1?_book_maptype1_quantvals(c):c->entries*c->dim);
    fprintf(out,"static const long _vq_quantlist_%s[] = {\n",name);
    for(j=0;j<vals;j++){
      fprintf(out,"\t%ld,\n",c->quantlist[j]);
    }
    fprintf(out,"};\n\n");
  }

  /* lengthlist */
  fprintf(out,"static const long _vq_lengthlist_%s[] = {\n",name);
  for(j=0;j<c->entries;){
    fprintf(out,"\t");
    for(k=0;k<16 && j<c->entries;k++,j++)
      fprintf(out,"%2ld,",c->lengthlist[j]);
    fprintf(out,"\n");
  }
  fprintf(out,"};\n\n");

  /* tie it all together */
  
  fprintf(out,"static const static_codebook %s = {\n",name);
  
  fprintf(out,"\t%ld, %ld,\n",c->dim,c->entries);
  fprintf(out,"\t(long *)_vq_lengthlist_%s,\n",name);
  fprintf(out,"\t%d, %ld, %ld, %d, %d,\n",
          c->maptype,c->q_min,c->q_delta,c->q_quant,c->q_sequencep);
  if(c->quantlist)
    fprintf(out,"\t(long *)_vq_quantlist_%s,\n",name);
  else
    fprintf(out,"\tNULL,\n");

  fprintf(out,"\t0\n};\n\n");
}
