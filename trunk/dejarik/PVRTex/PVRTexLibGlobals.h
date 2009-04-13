/******************************************************************************
* Name			: PVRTexLibGlobals.h
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
* Description	: Macros, enums and constants for the PVRTexLib.
*
* Platform		: ANSI
******************************************************************************/

#ifndef PVRTEXLIBGLOBALS_H
#define PVRTEXLIBGLOBALS_H

/*****************************************************************************
* Includes
*****************************************************************************/
#include <exception>

namespace pvrtexlib
{
	/*****************************************************************************
	* Arithmetic Macros
	*****************************************************************************/

	#define _CLAMP_(X,Xmin,Xmax) (  (X)<(Xmax) ?  (  (X)<(Xmin)?(Xmin):(X)  )  : (Xmax)    )

	/*****************************************************************************
	* Memory Macros
	*****************************************************************************/
#ifndef PVRFREE 
	#define PVRFREE(A) { if(A) {free(A); A=NULL;} } 
#endif
#ifndef PVRDELETE 
	#define PVRDELETE(A) { if(A) {delete(A); A=NULL;}} 
#endif
#ifndef	PVRDELETEARRAY 
	#define PVRDELETEARRAY(A) { if(A) {delete[](A); A=NULL;}} 
#endif

	/*****************************************************************************
	* Exception class and macros
	* Use char* literals only for m_what.
	*****************************************************************************/
	class PVRException : public std::exception
	{
	public:
		PVRException(char* what)throw(): m_what(what){}
		char *  what() {return m_what;}
		~PVRException() throw(){}
	private:
		 char*  m_what;
	};

	#define PVRTRY			try
	#define PVRTHROW(A)		{PVRException myException(A); throw(myException);}
	#define PVRCATCH(A)		catch(PVRException& A)
	#define PVRCATCHALL		catch(...)

	/*****************************************************************************
	* typedefs for 3 standard pixel type channels
	*****************************************************************************/
	typedef 	unsigned char	int8;
	typedef 	unsigned short	int16;
	typedef		float			float32;

	/*****************************************************************************
	* PixelType - corresponds to all pixel formats understood by PVRTexLib
	*****************************************************************************/
	enum PixelType
	{
		MGLPT_ARGB_4444 = 0x00,
		MGLPT_ARGB_1555,
		MGLPT_RGB_565,
		MGLPT_RGB_555,
		MGLPT_RGB_888,
		MGLPT_ARGB_8888,
		MGLPT_ARGB_8332,
		MGLPT_I_8,
		MGLPT_AI_88,
		MGLPT_1_BPP,
		MGLPT_VY1UY0,
		MGLPT_Y1VY0U,
		MGLPT_PVRTC2,
		MGLPT_PVRTC4,

		// OpenGL version of pixel types
		OGL_RGBA_4444= 0x10,
		OGL_RGBA_5551,
		OGL_RGBA_8888,
		OGL_RGB_565,
		OGL_RGB_555,
		OGL_RGB_888,
		OGL_I_8,
		OGL_AI_88,
		OGL_PVRTC2,
		OGL_PVRTC4,
		// OGL_BGRA_8888 extension
		OGL_BGRA_8888,

		// S3TC
		D3D_DXT1 = 0x20,
		D3D_DXT2,
		D3D_DXT3,
		D3D_DXT4,
		D3D_DXT5,

		D3D_RGB_332,
		D3D_AI_44,
		D3D_LVU_655,
		D3D_XLVU_8888,
		D3D_QWVU_8888,

		//10 bits per channel
		D3D_ABGR_2101010,
		D3D_ARGB_2101010,
		D3D_AWVU_2101010,

		//16 bits per channel
		D3D_GR_1616,
		D3D_VU_1616,
		D3D_ABGR_16161616,

		//HDR formats
		D3D_R16F,
		D3D_GR_1616F,
		D3D_ABGR_16161616F,

		//32 bits per channel
		D3D_R32F,
		D3D_GR_3232F,
		D3D_ABGR_32323232F,

		// Ericsson
		ETC_RGB_4BPP,


		MGLPT_NOTYPE = 0xffffffff			// v3 headers will most likely use 32 bits for pixel type
											// value will be masked to 0xff for other header versions

	};

	// The number of APIs supported by the library
	// enums for each of these APIs
	typedef enum E_API_TAG
	{
		eALL_API=0,
		eD3DM,
		eDX9,
		eMGL,
		eOGL2,
		eOGLES,
		eOGLES2,
		NUM_APIS,
	} E_API;

	// names for APIs
	const char ppszAPINames[NUM_APIS][16] =
	{
		"All APIs","Direct3D Mobile","DirectX 9","MGL","OpenGL","OpenGL ES 1.x","OpenGL ES 2.0"
	};

	// names for cube map faces
	const char g_pszCubeFaceNames[6][6] =
	{
		"FRONT","BACK","RIGHT","LEFT","TOP","BASE"
	};

	enum PVR_PRECMODE
	{	// precision modes - correspond to standard pixel types
		ePREC_INT8=0,
		ePREC_INT16,
		ePREC_FLOAT,
	} ;

	enum PVR_RESIZE
	{
		eRESIZE_NEAREST=0,
		eRESIZE_BILINEAR,
		eRESIZE_BICUBIC
	};

}

#endif
/*************************************************************************
Revisions:
 * RCS History:	$Log: PVRTexLibGlobals.h,v $
 * RCS History:	Revision 1.15  2007/12/19 11:30:28  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXTOOL] Changes for string utilities functions that are now in Tools
 * RCS History:	
 * RCS History:	Revision 1.14  2007/12/13 17:54:19  gml
 * RCS History:	[EXTERNAL UTILITIES PVRTEXTOOL] Added OGL_BGRA_8888 format. Added auto opening of help docs to Linux
 * RCS History:	
 * RCS History:	Revision 1.13  2007/12/04 16:21:32  gml
 * RCS History:	[EXTERNAL UTILITIES PVRTEXTOOL/LIB] Fixed for min max macros, added warning for alpha encoding when the target format is not capable of alpha. Texture class now tracks this. Fixed DXT1 encoding to observe punch-through transparency.
 * RCS History:	
 * RCS History:	Revision 1.12  2007/11/22 15:21:52  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] Changed namespace pvrt to namespace PVRT
 * RCS History:	
 * RCS History:	Revision 1.11  2007/11/20 12:42:23  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] Made alterations to use PVRT::string instead of std::string due to VS2003-VS2005 incompatibilities.
 * RCS History:	
 * RCS History:	Revision 1.10  2007/10/25 11:46:14  gml
 * RCS History:	[EXTERNAL UTILITIES PVRTEXTOOL] Added resizing for CL.
 * RCS History:	
 * RCS History:	Revision 1.9  2007/08/28 16:24:52  gml
 * RCS History:	[EXTERNAL UTILITIES PVRTEXLIB] Removed Pixel structs to separate files.
 * RCS History:	
 * RCS History:	Revision 1.8  2007/08/23 10:18:38  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] fix for linux 'illegal instruction'.
 * RCS History:	
 * RCS History:	Revision 1.7  2007/08/22 10:18:42  gml
 * RCS History:	[EXTERNAL UTILITIES PVRTEXLIB] ifndefed exception macros in case of conflicts with other libs. Added PVRTRY and PVRCATCHALL. Exception handling should be easily removed (if required) now. Extended pixel structs with constructors and updated types they use.
 * RCS History:	
 * RCS History:	Revision 1.6  2007/08/15 16:52:06  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] further alterations for public release.
 * RCS History:	
 * RCS History:	Revision 1.5  2007/08/14 10:07:14  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] made PixelType just a normal enum
 * RCS History:	
 * RCS History:	Revision 1.4  2007/08/13 17:42:35  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] Fixed header includes.
 * RCS History:	
 * RCS History:	Revision 1.3  2007/08/07 13:09:33  gml
 * RCS History:	[INTERNAL UTILITIES PVRTEXLIB] Added automatic revision logging, fixed a few comments.
 * RCS History:	
1.1		26/7/2007	Gordon MacLachlan
		Created.
*************************************************************************/

/*****************************************************************************
End of file (pvr_utils.h)
*****************************************************************************/
