/******************************************************************************
* Name			: PVRTextureUtilities.h
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
* Description	: Texture processing utility class. Employs a singleton structure.
*
* Platform		: ANSI
******************************************************************************/


#ifndef PVRTEXLIB_H
#define PVRTEXLIB_H

/*****************************************************************************
* Includes
*****************************************************************************/
#include "singleton.h"

#include "PVRTexLibGlobals.h"
#include "CPVRTextureData.h"
#include "CPVRTextureHeader.h"
#include "CPVRTexture.h"

namespace pvrtexlib
{
	class PVRTextureUtilities : public singleton<PVRTextureUtilities>
	{

	public:
		/*******************************************************************************
		* Function Name			: CompressPVR
		* In/Outputs		
		: sCompressedTexture	: Output CPVRTexture 
		: sDecompressedTexture	: Input CPVRTexture needs to be in ARBGB8888, ABGR16161616, ABGR32323232F
		: nMode					: Parameter value for specific image compressor - eg ETC
		* Description			: Takes a CPVRTexture in one of the data
		*						: formats ARBGB8888, ABGR16161616, ABGR32323232F
		*						: and compresses to the pixel type specified in the dest
		*						: PVRTexture. i32Mode specifies the quality mode.
		*******************************************************************************/
		void CompressPVR(	CPVRTexture& sCompressedTexture,
							CPVRTexture& sDecompressedTexture, const int i32Mode=0);
		/*******************************************************************************
		* Function Name			: CompressPVR
		* In/Outputs		
		: sCompressedHeader		: Output CPVRTextureHeader 
		: sCompressedData		: Output CPVRTextureData
		: sDecompressedHeader	: Input CPVRTexture needs to be in ARBGB8888, ABGR16161616, ABGR32323232F
		: sDecompressedData		: Input CPVRTexture needs to be in ARBGB8888, ABGR16161616, ABGR32323232F
		: nMode					: Parameter value for specific image compressor - eg ETC
		* Description			: Takes a CPVRTextureHeader/CPVRTextureData pair in one of the data
		*						: formats ARBGB8888, ABGR16161616, ABGR32323232F
		*						: and compresses to the pixel type specified in the dest
		*						: CPVRTextureHeader, the data goes int he dest CPVRTextureData.
		*						: i32Mode specifies the quality mode.
		*******************************************************************************/
		void CompressPVR(	CPVRTextureHeader			&sSourceHeader,
							CPVRTextureData				&sSourceData,
							CPVRTextureHeader			&sCompHeader,
							CPVRTextureData				&sCompData,
							const int					i32Mode=0);

		/*******************************************************************************
		* Function Name			: DecompressPVR
		* In/Outputs		
		: sCompressedTexture	: Output CPVRTexture 
		: sDecompressedTexture	: Input CPVRTexture needs to be in ARBGB8888, ABGR16161616, ABGR32323232F
		* Description			: Takes a CPVRTexture and decompresses it into one of the data
		*						: formats ARBGB8888, ABGR16161616, ABGR32323232F depending on the input format.
		*******************************************************************************/
		void DecompressPVR(CPVRTexture& sCompressedTexture,
										CPVRTexture& sDecompressedTexture);
		/*******************************************************************************
		* Function Name			: DecompressPVR
		* In/Outputs		
		: sCompressedHeader		: Output CPVRTextureHeader; will be in ARBGB8888, ABGR16161616, ABGR32323232F
		: sCompressedData		: Output CPVRTextureData; will be in ARBGB8888, ABGR16161616, ABGR32323232F
		: sDecompressedHeader	: Input CPVRTextureHeader 
		: sDecompressedData		: Input CPVRTextureData 
		* Description			: Takes a CPVRTexture and decompresses it into one of the data
		*						: formats ARBGB8888, ABGR16161616, ABGR32323232F depending on the input format.
		*******************************************************************************/
		void DecompressPVR(	const CPVRTextureHeader		& sCompressedHeader,
			const CPVRTextureData		& sCompressedData,
			CPVRTextureHeader			& sDecompressedHeader,
			CPVRTextureData				& sDecompressedData);

		/*******************************************************************************
		* Function Name			: ProcessRawPVR
		* In/Outputs		
		: sInputTexture			: Input CPVRTexture needs to be in ARBGB8888, ABGR16161616, ABGR32323232F
		: sOutputTexture		: Output CPVRTexture 
		* Description			: Takes a CPVRTexture and processes it according to the differences in the passed
		*						:	output CPVRTexture and the passed parameters. Requires the input texture
		*						:	to be in one of ARBGB8888, ABGR16161616, ABGR32323232F.
		*******************************************************************************/
		bool ProcessRawPVR(	CPVRTexture&		sInputTexture,
							CPVRTexture&		sOutputTexture,
							const unsigned int		uTilingMode,
							const bool				bDoBleeding,
							const float				fBleedRed=0.0f,
							const float				fBleedGreen=0.0f,
							const float				fBleedBlue=0.0f,
							PVR_RESIZE				eResizeMode=eRESIZE_BICUBIC );
		/*******************************************************************************
		* Function Name			: ProcessRawPVR
		* In/Outputs		
		: sInputTexture			: Input CPVRTexture needs to be in ARBGB8888, ABGR16161616, ABGR32323232F
		: sOutputTexture		: Output CPVRTexture 
		* Description			: Takes a CPVRTexture and decompresses it into one of the data
		*						: formats ARBGB8888, ABGR16161616, ABGR32323232F depending on the input format.
		*******************************************************************************/
		bool ProcessRawPVR(	CPVRTextureHeader&		sInputHeader,
							CPVRTextureData&		sInputData,
							CPVRTextureHeader&		sOutputHeader,
							CPVRTextureData&		sOutputData,
							const unsigned int		uTilingMode,
							const bool				bDoBleeding,
							const float				fBleedRed,
							const float				fBleedGreen,
							const float				fBleedBlue,
							PVR_RESIZE				eResizeMode=eRESIZE_BICUBIC );

		/*******************************************************************************
		* Constructor - Don't use explicitly. To get an instance use the 
		*	PVRTextureUtilities::inst() and PVRTextureUtilities::ptr() functions instead.
		*******************************************************************************/
		PVRTextureUtilities();
	};


}


/*************************************************************************
Revisions:
 * RCS History:	$Log: PVRTexLib.h,v $
 * RCS History:	Revision 1.14  2007/11/22 15:21:52  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] Changed namespace pvrt to namespace PVRT
 * RCS History:	
 * RCS History:	Revision 1.13  2007/11/20 12:42:23  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] Made alterations to use PVRT::string instead of std::string due to VS2003-VS2005 incompatibilities.
 * RCS History:	
 * RCS History:	Revision 1.12  2007/10/25 16:38:59  gml
 * RCS History:	[EXTERNAL UTILITIES PVRTEXTOOL] Added resizing for CL.
 * RCS History:	
 * RCS History:	Revision 1.11  2007/10/25 13:08:29  gml
 * RCS History:	[EXTERNAL UTILITIES PVRTEXTOOL] Added resizing for CL.
 * RCS History:	
 * RCS History:	Revision 1.10  2007/10/25 11:46:14  gml
 * RCS History:	[EXTERNAL UTILITIES PVRTEXTOOL] Added resizing for CL.
 * RCS History:	
 * RCS History:	Revision 1.9  2007/08/15 16:52:06  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] further alterations for public release.
 * RCS History:	
 * RCS History:	Revision 1.8  2007/08/13 17:42:15  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] Changed to be header for public release - is a cut-down version of pvrutilities class.
 * RCS History:	
*************************************************************************/

#endif
/*****************************************************************************
End of file (pvr_utils.h)
*****************************************************************************/
