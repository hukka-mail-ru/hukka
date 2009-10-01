// -*- C++ -*-
#if !defined( NEURALNET_H )
#define NEURALNET_H

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

#if defined( __GNUG__ )
#pragma interface
#endif

#ifdef WIN32
#define for if (1) for
#endif

#include "systypes.h"

/// Neural Net evaluator
//
class NeuralNet {
public:
  ///
  NeuralNet(uint          nInput,
	    uint          nHidden,
	    uint          nOutput,
	    float         betaHidden,
	    float         betaOutput,
	    const float*  hiddenWeight,
	    const float*  outputWeight,
	    const float*  hiddenThreshold,
	    const float*  outputThreshold);

  ~NeuralNet();

  ///
  uint		numInputs(void) const;
  ///
  uint		numOutputs(void) const;
  ///
  uint		numHidden(void) const;

  enum EvalType {
    NONE,
    SAVE,
    FROMBASE
  };

  ///
  void		evaluate(const float* inp, float* out) const;

  void		evaluate(float* inp, float* out, EvalType t) const;
  
private:
  void		evaluate(const float*  inp,
			 float*        ar,
			 float*        out,
			 float*        saveAr) const;

  
  void 		evaluateFromBase(const float* const  inp,
				 float*              ar,
				 float* const        out) const;
  void 		alloc(void);
  void		destroy(void);

  uint		nHiddenWeights(void) const;
  uint		nOutputWeights(void) const;
  
  uint		nInput;
  uint		nHidden;
  uint		nOutput;
  float 	betaHidden;
  float		betaOutput;
  
  const float*	hiddenWeight;
  const float*	outputWeight;
  const float*	hiddenThreshold;
  const float*	outputThreshold;

  float*	savedIBase;
  float*	savedBase;
};

inline uint
NeuralNet::numInputs(void) const
{
  return nInput;
}

inline uint
NeuralNet::numOutputs(void) const
{
  return nOutput;
}

inline uint
NeuralNet::numHidden(void) const
{
  return nHidden;
}

inline uint
NeuralNet::nHiddenWeights(void) const
{
  return nHidden * nInput;
}

inline uint
NeuralNet::nOutputWeights(void) const
{
  return nOutput * nHidden;
}

#endif
