/**********************************************************************

  Audacity: A Digital Audio Editor

  CrossFade.cpp

*******************************************************************//**

\class CrossFader
\brief Not used by Audacity (yet) apparently work in progress that has
been abandoned.

*//********************************************************************/

#include "CrossFade.h"

#include "SampleFormat.h"
#include "Resample.h"
#include "WaveClip.h"

#include <iostream>
#include <vector>

using std::vector;
using std::cout;
using std::endl;

CrossFader::CrossFader():
  mType(FT_MIX)
{
}



CrossFader::~CrossFader()
{
}



bool CrossFader::GetSamples(samplePtr buffer, sampleFormat format,
                            sampleCount start, sampleCount len)
{
   switch (mType)
   {
   case FT_MIX:
      return CrossFadeMix(buffer,format, start,len);
      break;
   case FT_TRIANGULAR:
      return CrossFadeMix(buffer,format, start,len);
      break;
   case FT_EXPONENTIAL:
   default:
      return CrossFadeMix(buffer,format, start,len);
      break;

   }

}

bool CrossFader::CrossFadeMix(samplePtr buffer, sampleFormat format, sampleCount start, sampleCount len)
{

  std::cout << "Crossfading from " << start << " to " << len+start << std::endl;

  // start refers to the position in the wave.


  //just mix the right things together.
  

  //For each relevant clip, we need to construct a buffer.
  //we should use one of the size len, because this has already
  //been determined to be good in Mixer

  //Get a pointer to the sequence in each clip.
  WaveClip * tmpclip = NULL;
  Sequence * tmp = NULL;
  
  WaveClipList::compatibility_iterator  it;
  
  
  //Go through each clip, adding it to the total in the appropriate way.

  //this could be 'optimized' by getting all of the sequences and then
  //iterating through each of them.
  
  int numclips = mClips.GetCount();
  
  //create vectors to store the important info for each clip.
  std::vector<sampleCount> clipStart(numclips);
  std::vector<sampleCount> clipLength(numclips);
  std::vector<Sequence*> tmpSequence(numclips);


  unsigned int i = 0;
  //Now, go through the clips and load up the vectors.
  for(it = mClips.GetFirst(); it; it = it->GetNext())
	{

	  tmpclip = it->GetData();
	  tmpSequence[i] = tmpclip->GetSequence();
	  

	  //start is the position of the beginning of the buffer
	  //relative to the beginning of the clip.  It could be negative.
	  clipStart[i]= start - tmpclip->GetStartSample();



	  //determine the index of the last sample to get, relative to the start of the clip

	  //it will be no longer than  the clip itself.
	  clipLength[i] = tmpclip->GetNumSamples()-clipStart[i];

	  std::cout << "X:" <<  " " << clipLength[i] << "  " << tmpclip->GetStartSample() <<  " ";
	  //if the buffer ends before the clip does, adjust the length
	  if(clipStart[i] + len   < clipLength[i])
		{
		  clipLength[i] = len  +  clipStart[i];
		}
	  std::cout << clipStart[i] << " " << clipLength[i] << " " << clipLength[i] - clipStart[i] <<  std::endl;
	}
  std::cout << "-------------\n";

  //Now, determine the sample format:
  switch(format) {
  case int16Sample: 
	{
	  std::cout << "int\n";
	  short *dest = (short *)buffer;
	  vector<short*> shortSeq;
	
	  //Copy the sequences over to the new vector, casting as you go. 
	  for(int i = 0; i < numclips; i++)
		shortSeq.push_back((short*)tmpSequence[i]);


	  int clips;
	  double f;
	  //now, shortSeq contains the samples to mix together.
	  for (int j = 0; j < len; j++) 
		{

		  //Go through each clip
		  for(int i = 0; i < numclips; i++)
			{
			  clips = 0;
			  f = 0;
			  if(j + clipStart[i] >= 0 &&
				 clipStart[i]+len < clipLength[i])//only copy if we are within the clip
				{
				  f += shortSeq[i][j+ clipStart[i]];
				  clips++;
				}

				  f/= clips;
				 
				  //Do bounds-checking
				  if (f > 32767)
					f = 32767;
				  if (f < -32768)
					f = -32768;

				  //Set value
				  *dest = (short)f;
			}
		  dest++;
		}
	}
	break;


  case int24Sample: 
	{
	  std::cout << "int24\n";
	  int *dest = (int *)buffer;
	  vector<int *> intSeq;
	  
	
	  //Copy the sequences over to the new vector, casting as you go. 
	  for(int i = 0; i < numclips; i++)
		intSeq.push_back((int*)tmpSequence[i]);
	  
	  int clips=0;
	  double f;
	  //Go through each sample position
	  for (int j = 0; j < len; j++) {

		//go through each clip.
		for(int i= 0; i < numclips; i++)
		  {
			clips = 0;
			f = 0;
			
			//only copy if we are within the clip
			if(j + clipStart[i] >= 0 && clipStart[i] + len < clipLength[i])  
			  {
				f+= intSeq[i][j+clipStart[i]];
				clips++;
			  }

			f /= clips;
			
			if (f > 8388607)
			  f = 8388607;
			if (f < -8388608)
			  f = -8388608;
			*dest = (int)f;
			
		  }
		dest ++;
	  }
	} 
	break;


  case floatSample: {
	std::cout << "float\n";
	float *dest = (float *)buffer;
	vector<float*> floatSeq;
	
	//Copy the sequences over to the new vector, casting as you go. 
	for(int i = 0; i < numclips; i++)
	  {
		float * tmp;
		//tmpSequence[i]->GetSamples(tmp,format,0,tmpSequence[i]->GetNumSamples());
		//floatSeq.push_back(tmp);
	  }
	int clips = 0;
	float f;

	//go through each sample position
	for (int j = 0; j < len; j++) {


		  clips = 0;
		  f = 0;

	  for(int i = 0; i < numclips; i++)
		{

		  cout << numclips << " " ;
		  
		  cout <<f << " " ;
		  
		  if(j + clipStart[i] >= 0 &&
			 clipStart[i] + j < clipLength[i])//only copy if we are within the clip
			{
			  f += floatSeq[i][j + clipStart[i]];			  
			  clips++;
			}
		  cout << f << " "<< i << " "<< floatSeq[i][j+clipStart[i]] << "|";
		}
	  if(clips == 0)
		*dest = 0.0f;
	  else
		{
	
		  f /= clips;
		  cout << f << "--";
		  // MM: XXX Should probably go outside the loop
		  if (f > 1.0f)
			*dest = 1.0f;
		  else if (f < -1.0f)
			*dest = -1.0f;
		  else
			*dest = (float)f;

		}
	  cout << *dest << endl;
	  dest++;
	}
  } break;
  } // switch
  

  return true;
}


void CrossFader::AddClip( WaveClip * clip)
{
  mClips.Append(clip);
}

void CrossFader::ClearClips()
{
  if(mClips.GetCount())
	mClips.Clear();
}
