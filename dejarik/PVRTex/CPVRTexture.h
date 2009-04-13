/******************************************************************************
 * Name			: CPVRTexture.h
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
 * Description	: Load + save Microsoft DDS files.
 *
 * Platform		: ANSI
 ******************************************************************************/

#ifndef CPVRTEXTURE_H
#define CPVRTEXTURE_H

#include "CPVRTextureHeader.h"
#include "CPVRTextureData.h"

namespace pvrtexlib
{

class CPVRTexture
{
public:
	/*******************************************************************************
	* Function Name  : CPVRTexture Constructors
	* Description    :  default
	*					from CPVRTextureHeader but no data
	*					from CPVRTextureHeader/CPVRTextureData pair
	*					from Header with blank data
	*					create Header for raw data
	*					from file
	*					from PVR data with embedded header (contents of PVR file)
	*******************************************************************************/
	CPVRTexture();								// default constructor
	CPVRTexture(CPVRTextureHeader&  sHeader);	// from CPVRTextureHeader but no data
	CPVRTexture(CPVRTextureHeader& sHeader,		// from CPVRTextureHeader/CPVRTextureData pair
		CPVRTextureData& sData);
	CPVRTexture(const unsigned int u32Width,	// create header info with no data
		const unsigned int	u32Height,
		const unsigned int	u32MipMapCount,
		const unsigned int	u32NumSurfaces,
		const bool			bBorder,
		const bool			bTwiddled,
		const bool			bCubeMap,
		const bool			bVolume,
		const bool			bFalseMips,
		const bool			bAlpha,
		const PixelType		ePixelType,
		const float			fNormalMap);
	CPVRTexture(const unsigned int u32Width,	// create header info for raw data
		const unsigned int	u32Height,
		const unsigned int	u32MipMapCount,
		const unsigned int	u32NumSurfaces,
		const bool			bBorder,
		const bool			bTwiddled,
		const bool			bCubeMap,
		const bool			bVolume,
		const bool			bFalseMips,
		const bool			bAlpha,
		const PixelType		ePixelType,
		const float			fNormalMap,
		int8				*pPixelData);
	CPVRTexture(const char* pszFilename);		// from .pvr or .dds file depending on extension passed
	CPVRTexture(const int8* const pPVRData);	// from PVR data with embedded header (contents of PVR file)

	/*******************************************************************************
	* Function Name  : getHeader
	* Description    : returns the CPVRTextureHeader instance from this instance
	*					note this returns a reference
	*******************************************************************************/
	CPVRTextureHeader&	getHeader();
	/*******************************************************************************
	* Function Name  : getData
	* Description    : returns the CPVRTextureData instance from this instance
	*					note this returns a reference
	*******************************************************************************/
	CPVRTextureData&	getData();
	void				setData(int8* pData);
	PixelType			getPixelType();
	void				setPixelType(PixelType ePixelType);
	unsigned int		getPrecMode() const;
	unsigned int		getWidth() const;
	void				setWidth(unsigned int u32Width);
	unsigned int		getHeight() const;
	void				setHeight(unsigned int u32Height);
	unsigned int		getMipMapCount() const;
	void				setMipMapCount(unsigned int u32MipMapCount);
	bool				hasMips() const;
	unsigned int		getNumSurfaces() const;
	void				setNumSurfaces(unsigned int u32NumSurfaces);

	/*******************************************************************************
	* Function Name  : Accessor functions for flag values
	* Description    : Border: a border around the texture
	*					in order to avoid artifacts. See the PVRTC document for more info
	*					Twiddled: Morton order for the texture
	*					CubeMap: does the texture constitute 6 surfaces facing a cube
	*					Volume:	is this a volume texture
	*					NormalMap: a value of 0.0f indicates not a Normal map texture
	*								a non-zero value is taken as the height factor
	*								when calculating the normal vectors
	*					FalseMips: artificially coloured MIP-map levels
	*******************************************************************************/
	bool			isBordered() const;
	void			setBorder(bool bBorder);
	bool			isTwiddled() const;
	void			setTwiddled(bool bTwiddled);
	bool			isCubeMap() const;
	void			setCubeMap(bool bCubeMap);
	bool			isVolume() const;
	void			setVolume(const bool bVolume);
	float			getNormalMap() const;
	void			setNormalMap(const float fNormalMap);
	bool			isNormalMap() const;
	bool			hasFalseMips() const;
	void			setFalseMips(const bool bFalseMips); 

	/*******************************************************************************
	* Function Name  : ConvertToFloat/Int16/Int8
	* Description    : Converts the data to float/int16/int8 from int8/int16/float
	*					expanding/contracting the buffer.
	*					If the data is not int8/int16/float PVRTHROWs
	*******************************************************************************/
	void				ConvertToFloat();
	void				ConvertToInt16();
	void				ConvertToInt8();
	/*******************************************************************************
	* Function Name  : SwapChannels
	* Description    : Swaps the two specified channels.
	*					If the data is not int8/int16/float PVRTHROWs
	*******************************************************************************/
	void				SwapChannels(unsigned int u32ChannelA, unsigned int u32ChannelB);
	/*******************************************************************************
	* Function Name  : getSurfaceData
	* Description    : Returns a pointer to the surface specified, NULL if it doesn't exist
	*******************************************************************************/
	int8*				getSurfaceData(unsigned int u32SurfaceNum);
	/*******************************************************************************
	* Function Name  : Append
	* Description    : Appends a texture to the current one if possible. If the current
	*					texture has no surfaces then acts like a copy assignment
	*******************************************************************************/
	void				append(CPVRTexture& sTexture);
	/*******************************************************************************
	* Function Name  : RGBToAlpha
	* Description    : Adds alpha to a texture from the RGB data of the other texture
	*******************************************************************************/
	void				RGBToAlpha(	CPVRTexture& sAlphaTexture,
									const unsigned int u32DestSurfaceNum = 0,
									const unsigned int u32SourceSurfaceNum = 0 );

	/*******************************************************************************
	* Function Name  : writeToFile
	* Description    : writes to a file as a .pvr or .dds depending on the extension
	*					of the file path passed.
	*******************************************************************************/
	size_t				writeToFile(const char* const strFilename,
									const unsigned int u32TextureVersion = CPVRTextureHeader::u32CURRENT_VERSION)const ;

	/*******************************************************************************
	* Function Name  : writeIncludeFile32Bits
	* Description    : writes to a C++ compatible header file
	*******************************************************************************/
	size_t				writeIncludeFile32Bits(const char* const pszFilename,
							const char* const varName,
							const unsigned int u32TextureVersion = CPVRTextureHeader::u32CURRENT_VERSION)const ;

	private:
	CPVRTextureHeader	m_sHeader;
	CPVRTextureData		m_sData;

};

}
#endif // CPVRTEXTURE_H

/*************************************************************************
Revisions:
 * RCS History:	$Log: CPVRTexture.h,v $
 * RCS History:	Revision 1.5.6.1  2008/02/05 17:43:05  gml
 * RCS History:	[EXTERNAL UTILITIES PVRTEXLIB] Improved alpha flag support again.
 * RCS History:	
 * RCS History:	Revision 1.5  2007/08/15 16:52:06  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] further alterations for public release.
 * RCS History:	
 * RCS History:	Revision 1.4  2007/08/13 17:19:06  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] Edited file i/o and moved accessor function bodies into .cpp
 * RCS History:	
 * RCS History:	Revision 1.3  2007/08/07 13:09:33  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] Added automatic revision logging, fixed a few comments.
 * RCS History:	
1.1		22/5/2007	Gordon MacLachlan
	Created. 
*************************************************************************/

/*****************************************************************************
 End of file (PVRTexture.h)
*****************************************************************************/
