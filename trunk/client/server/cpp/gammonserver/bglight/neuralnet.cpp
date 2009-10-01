#if defined( __GNUG__ )
#pragma implementation
#endif

/*
 * neuralnet.h
 *
 * by Joseph Heled, 2002
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include <memory.h>

#include "compiler.h"
#include "neuralnet.h"

static float e[100] = {
0.10000000000000001, 
0.11051709180756478, 
0.12214027581601698, 
0.13498588075760032, 
0.14918246976412702, 
0.16487212707001281, 
0.18221188003905089, 
0.20137527074704767, 
0.22255409284924679, 
0.245960311115695, 
0.27182818284590454, 
0.30041660239464335, 
0.33201169227365473, 
0.36692966676192446, 
0.40551999668446748, 
0.44816890703380646, 
0.49530324243951152, 
0.54739473917271997, 
0.60496474644129461, 
0.66858944422792688, 
0.73890560989306509, 
0.81661699125676512, 
0.90250134994341225, 
0.99741824548147184, 
1.1023176380641602, 
1.2182493960703473, 
1.3463738035001691, 
1.4879731724872838, 
1.6444646771097049, 
1.817414536944306, 
2.0085536923187668, 
2.2197951281441637, 
2.4532530197109352, 
2.7112638920657881, 
2.9964100047397011, 
3.3115451958692312, 
3.6598234443677988, 
4.0447304360067395, 
4.4701184493300818, 
4.9402449105530168, 
5.4598150033144233, 
6.034028759736195, 
6.6686331040925158, 
7.3699793699595784, 
8.1450868664968148, 
9.0017131300521811, 
9.9484315641933776, 
10.994717245212353, 
12.151041751873485, 
13.428977968493552, 
14.841315910257659, 
16.402190729990171, 
18.127224187515122, 
20.033680997479166, 
22.140641620418716, 
24.469193226422039, 
27.042640742615255, 
29.886740096706028, 
33.029955990964865, 
36.503746786532886, 
40.34287934927351, 
44.585777008251675, 
49.274904109325632, 
54.457191012592901, 
60.184503787208222, 
66.514163304436181, 
73.509518924197266, 
81.24058251675433, 
89.784729165041753, 
99.227471560502622, 
109.66331584284585, 
121.19670744925763, 
133.9430764394418, 
148.02999275845451, 
163.59844299959269, 
180.80424144560632, 
199.81958951041173, 
220.83479918872089, 
244.06019776244983, 
269.72823282685101, 
298.09579870417281, 
329.44680752838406, 
364.09503073323521, 
402.38723938223131, 
444.7066747699858, 
491.47688402991344, 
543.16595913629783, 
600.29122172610175, 
663.42440062778894, 
733.19735391559948, 
810.3083927575384, 
895.52927034825075, 
989.71290587439091, 
1093.8019208165192, 
1208.8380730216988, 
1335.9726829661872, 
1476.4781565577266, 
1631.7607198015421, 
1803.3744927828525, 
1993.0370438230298, 
};


static inline float
sigmoid(float const xin)
{
  float const x = xin < 0 ? -xin : xin;
  
  float x1 = 10 * x;

  unsigned int const i = (int)x1;
  if( i < 100 ) {
    x1 = e[i] * (10 - (int)i + x1);
  } else {
    x1 = 1993.0370438230298 * 10;
  }

  float const v = ( (xin < 0 ) ? x1 : 1.0 ) / (1.0 + x1);
  
  return v;
}


void
NeuralNet::destroy(void)
{
  delete [] savedBase; savedBase = 0;
  delete [] savedIBase; savedIBase = 0;
}

NeuralNet::~NeuralNet()
{
  destroy();
}

void 
NeuralNet::alloc(void)
{
  savedBase = new float [nHidden];
  savedIBase = new float [nInput];
}

NeuralNet::NeuralNet(uint          nInput_,
		     uint          nHidden_,
		     uint          nOutput_,
		     float         betaHidden_,
		     float         betaOutput_,
		     const float*  hiddenWeight_,
		     const float*  outputWeight_,
		     const float*  hiddenThreshold_,
		     const float*  outputThreshold_) :
  nInput(nInput_),
  nHidden(nHidden_),
  nOutput(nOutput_),
  betaHidden(betaHidden_),
  betaOutput(betaOutput_),

  hiddenWeight(hiddenWeight_),
  outputWeight(outputWeight_),
  hiddenThreshold(hiddenThreshold_),
  outputThreshold(outputThreshold_)
{
  alloc();
  
  for(uint i = 0; i < nHidden; ++i) {
    savedBase[i] = 0.0;
  }
  
  for(uint i = 0; i < nInput; ++i) {
    savedIBase[i] = 0.0;
  }
}


void
NeuralNet::evaluate(const float* const inp, float* ar, float* const out,
		    float* saveAr) const
{
  // Calculate activity at hidden nodes
  for(uint i = 0; i < nHidden; ++i) {
    ar[i] = hiddenThreshold[i];
  }

  const float* prWeight = hiddenWeight;
  
  for(uint i = 0; i < nInput; ++i) {
    float const ari = inp[i];

    if( ari == 0 ) {
      prWeight += nHidden;
    } else {
      float* pr = ar;
      
      if( ari == 1.0 ) {
	for(int j = nHidden; j; --j) {
	  *pr++ += *prWeight++;
	}
      }
      else {
	for(int j = nHidden; j; --j) {
	  *pr++ += *prWeight++ * ari;
	}
      }
    }
  }

  if( saveAr ) {
    memcpy(saveAr, ar, nHidden * sizeof(*saveAr));
  }
  
  for(uint i = 0; i < nHidden; ++i) {
    ar[i] = sigmoid( -betaHidden * ar[i] );
  }

  // Calculate activity at output nodes
  
  prWeight = outputWeight;

  for(uint i = 0; i < nOutput; ++i) {
    float r = outputThreshold[i];
    
    for(uint j = 0; j < nHidden; ++j) {
      r += ar[j] * *prWeight++;
    }

    out[i] = sigmoid(-betaOutput * r);
  }
}

void
NeuralNet::evaluateFromBase(const float* const inp, float* ar,
			    float* const out) const
{
  // Calculate activity at hidden nodes
  const float* prWeight = hiddenWeight;
  
  for(uint i = 0; i < nInput; ++i) {
    float const ari = inp[i];

    if( ari == 0 ) {
      prWeight += nHidden;
    } else {
      float* pr = ar;
      
      if( ari == 1.0 ) {
	for(int j = nHidden; j; --j) {
	  *pr++ += *prWeight++;
	}
      }
      else if( ari == -1.0 ) {
	for(int j = nHidden; j; --j) {
	  *pr++ -= *prWeight++;
	}
      } else {
	for(int j = nHidden; j; --j) {
	  *pr++ += *prWeight++ * ari;
	}
      }
    }
  }

  for(uint i = 0; i < nHidden; ++i) {
    ar[i] = sigmoid( -betaHidden * ar[i] );
  }

  // Calculate activity at output nodes
  
  prWeight = outputWeight;

  for(uint i = 0; i < nOutput; ++i) {
    float r = outputThreshold[i];
    
    for(uint j = 0; j < nHidden; ++j) {
      r += ar[j] * *prWeight++;
    }

    out[i] = sigmoid(-betaOutput * r);
  }
}

void
NeuralNet::evaluate(const float* const inp, float* const out) const
{
  LOCAL_ARRAY(float, ar, nHidden);

  evaluate(inp, ar, out, 0);
}

void
NeuralNet::evaluate(float* inp, float* out, EvalType const t) const
{
  LOCAL_ARRAY(float, ar, nHidden);
  
  switch( t ) {
    case NONE:
    {
      evaluate(inp, ar, out, 0);
      break;
    }
    case SAVE:
    {
      memcpy(savedIBase, inp, nInput * sizeof(*ar));
      evaluate(inp, ar, out, savedBase);
      break;
    }
    case FROMBASE:
    {
      memcpy(ar, savedBase, nHidden * sizeof(*ar));
      for(uint i = 0; i < nInput; ++i) {
	inp[i] -= savedIBase[i];
      }
      evaluateFromBase(inp, ar, out);
      break;
    }
  }
}

