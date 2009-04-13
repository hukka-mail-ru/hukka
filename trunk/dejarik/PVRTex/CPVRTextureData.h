/******************************************************************************
 * Name			: CPVRTextureData.h
 * Author		: PowerVR
 *
 * Created		: April 2006
 *
 *
 * Copyright	: 2005 by Imagination Technologies Limited. All rights reserved.
 *				: No part of this software, either material or conceptual
 *				: may be copied or distributed, transmitted, transcribed,
 *				: stored in a retrieval system or translated into any
 *				: human or computer language in any form by any means,
 *				: electronic, mechanical, manual or other-wise, or
 *				: disclosed to third parties without the express written
 *				: permission of VideoLogic Limited, Unit 8, HomePark
 *				: Industrial Estate, King's Langley, Hertfordshire,
 *				: WD4 8LZ, nU.K.
 *
 * Description	: Class to hold the data part of PVRTextures.
 *
 * Platform		: ANSI
 ******************************************************************************/

#ifndef CPVRTEXTUREDATA_H
#define CPVRTEXTUREDATA_H

#include "PVRTexLibGlobals.h"

namespace pvrtexlib
{

class CPVRTextureHeader;

class CPVRTextureData
{
public:


	// enums

	enum{
		CHAN_A=0,
		CHAN_R,
		CHAN_G,
		CHAN_B,
		CHAN_RGB,
		CHAN_ARGB
	};


	// constructors
	CPVRTextureData():m_pData(NULL),m_stDataSize(0){}
	CPVRTextureData(size_t stDataSize);
	CPVRTextureData(int8 *pTextureData, size_t stDataSize);

	CPVRTextureData(const CPVRTextureData& original);
	CPVRTextureData& operator=(const CPVRTextureData& sData);

	~CPVRTextureData(){PVRDELETEARRAY(m_pData);}

	/*******************************************************************************
	* Function Name  : getData
	* In/Outputs	  : 
	* Description    : returns pointer to the data buffer in this instance
	*******************************************************************************/
	int8* getData() const;

	/*******************************************************************************
	* Function Name  : getDataSize
	* In/Outputs	  : 
	* Description    : returns the recorded size of the data buffer in this instance
	*******************************************************************************/
	size_t getDataSize() const;

	/*******************************************************************************
	* Function Name  : clear
	* In/Outputs	  : 
	* Description    : releases any data stored and sets size to 0
	*******************************************************************************/
	void clear();

	/*******************************************************************************
	* Function Name  : ResizeBuffer
	* In/Outputs	  : stNewSize:		new size for the buffer
	* Description    : Resizes the buffer held by this texture. Copies across
	*					existing data to the new buffer, truncating it if the new
	*					buffer is smaller than the last.
	*******************************************************************************/
	void ResizeBuffer(size_t stNewSize);
	/*******************************************************************************
	* Function Name  : ConvertToFloat/Int16/Int8
	* Description    : Converts the data held by the buffer to float from int8/int16/float
	*					expanding the buffer.
	*					If the data is not int8/int16/float PVRTHROWS
	*******************************************************************************/
	void ConvertToFloat(const CPVRTextureHeader& sHeader);
	void ConvertToInt16(const CPVRTextureHeader& sHeader);
	void ConvertToInt8(const CPVRTextureHeader& sHeader);
	/*******************************************************************************
	* Function Name  : WriteToFile
	* Description    : Writes the data in this instance to the passed file. Returns
	*					the number of bytes written.
	*******************************************************************************/
	size_t			writeToFile(FILE* const fFile)const;
	/*******************************************************************************
	* Function Name  : writeToIncludeFile
	* Description    : Writes the data in this instance to the passed file as a series
	*					of unsigned integers compatible with a C++ header. Returns
	*					the number of bytes written.
	*******************************************************************************/
	size_t			writeToIncludeFile(FILE* const fFile)const;
	/*******************************************************************************
	* Function Name  : append
	* Description    : will append the data from newData to the existing data in this
	*					CPVRTextureData instance. It may be wise to check if the 
	*					pixel types of these two instances match. PVRTHROWs if
	*					unsuccessful.
	*******************************************************************************/
	void			append(const CPVRTextureData& newData);

	/*******************************************************************************
	* Function Name  : RGBToAlpha
	* Description    : Dumps the maximum value of the RGB values in the current data
						into the alpha channel. Leaves the other channels untouched.
						Needs int8 or PVRTHROWs
	*******************************************************************************/
	void			RGBToAlpha(	const unsigned int u32SurfaceNum,
									const CPVRTextureHeader& psInputHeader,
									const CPVRTextureData& sAlphaData);

	/*******************************************************************************
	* Function Name  : SwapChannels
	* Description    : Swaps the two specified channels.
	*					If the data is not int8/int16/float PVRTHROWs
	*******************************************************************************/
	void SwapChannelsInt8(unsigned int u32ChannelA, unsigned int u32ChannelB);
	void SwapChannelsInt16(unsigned int u32ChannelA, unsigned int u32ChannelB);
	void SwapChannelsFloat(unsigned int u32ChannelA, unsigned int u32ChannelB);

	/*******************************************************************************
	* Function Name  : ClearChannel
	* Description    : Clears the specified channel to white
	*******************************************************************************/
	void ClearChannel(const CPVRTextureHeader& sHeader,const unsigned int u32Channel);


	private:
	int8			*m_pData;			// pointer to image data
	size_t			m_stDataSize;		// size of data in bytes

};

}

#endif // CPVRTEXTUREDATA_H

/*************************************************************************
Revisions:
 * RCS History:	$Log: CPVRTextureData.h,v $
 * RCS History:	Revision 1.10  2007/11/28 10:11:25  gml
 * RCS History:	[EXTERNAL UTILITIES PVRTEXLIB] Added clear function to CPVRTextureData
 * RCS History:	
 * RCS History:	Revision 1.9  2007/08/15 16:52:06  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] further alterations for public release.
 * RCS History:	
 * RCS History:	Revision 1.8  2007/08/13 17:30:30  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] Removed friend. Uses PRtexLibGlobals as header
 * RCS History:	
 * RCS History:	Revision 1.7  2007/08/07 13:09:33  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] Added automatic revision logging, fixed a few comments.
 * RCS History:	
1.1		22/5/2007	Gordon MacLachlan
	Created. 
*************************************************************************/

/*****************************************************************************
 End of file (CPVRTextureData.h)
*****************************************************************************/

