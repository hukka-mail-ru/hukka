/******************************************************************************
 * Name			: CPVRTextureHeader.h
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
 * Description	: Class to represent header information of a pvr texture file. Typically associated
 *				  with a CPVRTextureData instance or part of a CPVRTexture instance
 *
 * Platform		: ANSI
 ******************************************************************************/

#ifndef CPVRTEXTUREHEADER_H
#define CPVRTEXTUREHEADER_H

#include "PVRTexLibGlobals.h"

namespace pvrtexlib
{

class PVRTextureUtilities;

class CPVRTextureHeader
{
public:

	/*******************************************************************************
	* Constructor
	* Description		: Blank constructor that allows the creation of a 
	*						valid but blank CPVRHeader
	*******************************************************************************/
	CPVRTextureHeader();


	/*******************************************************************************
	* Constructor
	* In
	:	u32Width		:	width of topmost image in pixels
	:	u32Height		:	height of topmost image
	:	u32MipMapCount	:	count of MIP-map levels
	:	u32NumSurfaces	:	number of surfaces present in texture
	:	bBorder			:	does the texture have an added border (se PVRTC compression documentation)
	:	bTwiddled		:	is the texture twiddled (use morton order)
	:	bCubeMap		:	is the texture a cube map
	:	bVolume			:	is the texture a volume texture (little support right now)
	:	bFalseMips		:	are false-coloured MIP-map levels encoded
	:	ePixelType		:	which pixel format is used for the data
	:	fNormalMap		:	multiplier when used in normal map creation for this texture.
	:						0.0f implies that this is not a normal map texture
	* Description		: Manual constructor that allows the creation of a CPVRHeader
	*******************************************************************************/
	CPVRTextureHeader(const unsigned int u32Width,
		const unsigned int	u32Height,
		const unsigned int	u32MipMapCount,
		const unsigned int	u32NumSurfaces,
		const bool			bBorder,
		const bool			bTwiddled,
		const bool			bCubeMap,
		const bool			bVolume,
		const bool			bFalseMips,
		const bool			bDoAlpha,
		const PixelType		ePixelType,
		const float			fNormalMap);

	/*******************************************************************************
	* Constructor
	* In
	:	fFile			:	open FILE pointer to PVR header data (beginning of a .pvr file)
	* Description		:	Reads a header from the passed FILE pointer leaving this
	*						at the beginning of any data following the header.
	*******************************************************************************/
	CPVRTextureHeader(FILE *const fFile);
	/*******************************************************************************
	* Constructor
	* In
	:	fFile			:	pointer to PVR header data (beginning of a .pvr file)
	* Description		:	Reads a header from the passed pointer.
	*******************************************************************************/
	CPVRTextureHeader(const int8* const pPVRData);


	/*******************************************************************************
	* Accessor Methods for basic properties
	*******************************************************************************/
	unsigned int	getWidth() const;
	void			setWidth(unsigned int u32Width);
	unsigned int	getHeight() const;
	void			setHeight(unsigned int u32Height);
	void			getDimensions(unsigned int& width, unsigned int& height) const;
	void			setDimensions(const unsigned int u32Width, const unsigned int u32Height);
	unsigned int	getMipMapCount() const;
	void			setMipMapCount(unsigned int u32MipMapCount);
	PixelType		getPixelType() const;
	void			setPixelType(const PixelType ePixelType);
	unsigned int	getNumSurfaces() const;
	void			setNumSurfaces(unsigned int u32NumSurfaces);

	/*******************************************************************************
	* Accessor Methods for preprocessing properties etc.
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
	bool			hasMips() const;
	bool			hasFalseMips() const;
	void			setFalseMips(const bool bFalseMips);
	bool			hasAlpha();
	void			setAlpha(const bool bAlpha);
	unsigned int	getOriginalVersionNumber();
	unsigned int	getCurrentVersionNumber();

	
	/*******************************************************************************
	* Function Name  : getSurfaceSize
	* Returns        : size of an individual surface (including MIP-map levels)
	*					described by the header
	*******************************************************************************/
	size_t			getSurfaceSize() const;
	/*******************************************************************************
	* Function Name  : getSurfaceSizeInPixels
	* Returns        : the number of pixels describd by this surface
	*******************************************************************************/
	unsigned int	getSurfaceSizeInPixels() const;
	/*******************************************************************************
	* Function Name  : getTotalTextureSize
	* Returns        : sum of the size of all individual surfaces (including MIP-map
	*					levels) described by the header
	*******************************************************************************/
	size_t			getTotalTextureSize() const;

	/*******************************************************************************
	* Function Name  : getFileHeaderSize
	* Returns        : returns file size from original header version (if possible)
	*					otherwise PVRTHROWs
	*******************************************************************************/
	size_t			getFileHeaderSize();
	/*******************************************************************************
	* Function Name  : getFileHeaderSize
	* Returns        : returns file size from passed header version (if possible)
	*					otherwise PVRTHROWs
	*******************************************************************************/
	size_t			getFileHeaderSize(int u32HeaderVersion);
	/*******************************************************************************
	* Function Name  : getPrecMode
	* Returns        : returns the precision mode of the pixel type of this header
	*******************************************************************************/
	PVR_PRECMODE	getPrecMode() const;
	/*******************************************************************************
	* Function Name  : getBitsPerPixel
	* Returns        : returns the number of bits of data per pixel required by the
	*					pixel type of this header
	*******************************************************************************/
	unsigned int	getBitsPerPixel() const;
	/*******************************************************************************
	* Function Name  : hasSurfaceCompatibleWith
	* Returns        : returns whether this header describes a texture with surfaces
						that are compatible for appending etc with the ones described
						in the passed header.
	*******************************************************************************/
	bool			hasSurfaceCompatibleWith(const CPVRTextureHeader & sHeader);
	/*******************************************************************************
	* Function Name  : writeToFile
	* Returns        : writes this pvr header to the FILE* passed leaving the FILE
						pointer at the end of the header data. Returns the size 
						of data written
	*******************************************************************************/
	size_t			writeToFile(FILE* const fFile)const;
	/*******************************************************************************
	* Function Name  : writeToPointer
	* Returns        : writes this pvr header to the pointer passed. Returns the size
						of the data written
	*******************************************************************************/
	size_t			writeToPointer(int8 * const pPointer)const;
	/*******************************************************************************
	* Function Name  : writeToIncludeFile
	* Returns        :  writes this pvr header to the FILE* passed leaving the FILE
						pointer at the end of the header data. Writes this as the
						beginning of a c++ compatible header file. Returns the size 
						of data written
	*******************************************************************************/
	size_t			writeToIncludeFile(FILE* const fFile)const;

	const static unsigned int	u32CURRENT_VERSION	= 2;

private:
	PVRTextureUtilities			*PVRTU;		// note this is a singleton
	unsigned int				m_u32Height, m_u32Width, m_u32MipMapCount,m_u32NumSurfaces;
	unsigned int				m_u32OriginalVersionNumber;	// the file header version on the disk from which this class was constructed
	bool						m_bTwiddled, m_bCubeMap, m_bVolume, m_bFalseMips, m_bBorder, m_bAlpha;
	PixelType					m_ePixelType;
	float						m_fNormalMap;

	void InitBlank();
	void InitFromPointer(const int8* const pData);

// *** CURRENT PVR HEADER V3 ***

	struct PVRTextureHeaderV3{
		unsigned int	u32Version;
		unsigned int	u32PixelType;
		unsigned int	u32Height;
		unsigned int	u32Width;
		unsigned int	u32MIPMapCount;
		unsigned int	u32NumSurfaces;
		unsigned int	u32Flags;
		float			fNormalMapScale;
	};
// *** OLD PVR HEADERS ***

	// legacy pvr file structs
	// only really for file I/O with old pvrs
	struct PVRTextureHeaderV1{
		unsigned int  dwHeaderSize;		/* size of the structure */
		unsigned int  dwHeight;			/* height of surface to be created */
		unsigned int  dwWidth;			/* width of input surface */
		unsigned int  dwMipMapCount;	/* number of mip-map levels requested */
		unsigned int  dwpfFlags;		/* pixel format flags */
		unsigned int  dwDataSize;		/* Size of the compress data */
		unsigned int  dwBitCount;		/* number of bits per pixel  */
		unsigned int  dwRBitMask;		/* mask for red bit */
		unsigned int  dwGBitMask;		/* mask for green bits */
		unsigned int  dwBBitMask;		/* mask for blue bits */
		unsigned int  dwAlphaBitMask;	/* mask for alpha channel */
	} ;

	struct PVRTextureHeaderV2{
		unsigned int  dwHeaderSize;		/* size of the structure */
		unsigned int  dwHeight;			/* height of surface to be created */
		unsigned int  dwWidth;			/* width of input surface */
		unsigned int  dwMipMapCount;	/* number of mip-map levels requested */
		unsigned int  dwpfFlags;		/* pixel format flags */
		unsigned int  dwDataSize;		/* Size of the compress data */
		unsigned int  dwBitCount;		/* number of bits per pixel  */
		unsigned int  dwRBitMask;		/* mask for red bit */
		unsigned int  dwGBitMask;		/* mask for green bits */
		unsigned int  dwBBitMask;		/* mask for blue bits */
		unsigned int  dwAlphaBitMask;	/* mask for alpha channel */
		unsigned int  dwPVR;			/* should be 'P' 'V' 'R' '!' */
		unsigned int  dwNumSurfs;		/* number of slices for volume textures or skyboxes */
	};

	const static unsigned int PVRTEX_MIPMAP			= (1<<8);
	const static unsigned int PVRTEX_TWIDDLE		= (1<<9);
	const static unsigned int PVRTEX_BUMPMAP		= (1<<10);
	const static unsigned int PVRTEX_TILING			= (1<<11);
	const static unsigned int PVRTEX_CUBEMAP		= (1<<12);
	const static unsigned int PVRTEX_FALSEMIPCOL	= (1<<13);
	const static unsigned int PVRTEX_VOLUME			= (1<<14);
	const static unsigned int PVRTEX_ALPHA			= (1<<15);

	// V3
	const static unsigned int PVRTEX_IDENTV3		= 0x03525650;	// 'P''V''R'3

	// legacy
	const static unsigned int PVRTEX_IDENTIFIER		= 0x21525650;
	const static unsigned int PVRTEX_PIXELTYPE		= 0xff;			// pixel type is always in the last 16bits of the flags

	void DoBitMasks(PVRTextureHeaderV2 *phPVR, bool bAlpha) const;

};

}

#endif // CPVRTEXTUREHEADER_H

/*************************************************************************
Revisions:
 * RCS History:	$Log: CPVRTextureHeader.h,v $
 * RCS History:	Revision 1.10.2.1  2008/02/05 17:43:05  gml
 * RCS History:	[EXTERNAL UTILITIES PVRTEXLIB] Improved alpha flag support again.
 * RCS History:	
 * RCS History:	Revision 1.10  2008/01/09 16:31:09  gml
 * RCS History:	[EXTERNAL UTILITIES PVRTEXTOOL] Improvd support for alpha in texture headers.
 * RCS History:	
 * RCS History:	Revision 1.9  2007/12/13 17:54:19  gml
 * RCS History:	[EXTERNAL UTILITIES PVRTEXTOOL] Added OGL_BGRA_8888 format. Added auto opening of help docs to Linux
 * RCS History:	
 * RCS History:	Revision 1.8  2007/12/04 16:21:32  gml
 * RCS History:	[EXTERNAL UTILITIES PVRTEXTOOL/LIB] Fixed for min max macros, added warning for alpha encoding when the target format is not capable of alpha. Texture class now tracks this. Fixed DXT1 encoding to observe punch-through transparency.
 * RCS History:	
 * RCS History:	Revision 1.7  2007/08/15 16:52:06  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] further alterations for public release.
 * RCS History:	
 * RCS History:	Revision 1.6  2007/08/13 17:34:09  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] Changed border to a bool. Fixed header includes. Removed references to v3 header for release.
 * RCS History:	
 * RCS History:	Revision 1.5  2007/08/07 13:09:33  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] Added automatic revision logging, fixed a few comments.
 * RCS History:	
1.1		22/5/2007	Gordon MacLachlan
	Created. 
*************************************************************************/
/*****************************************************************************
 End of file (PVRTexture.h)
*****************************************************************************/
