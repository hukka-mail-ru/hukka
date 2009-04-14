#include "BMP.h"
#include "CPVRTexture.h"

using namespace std;
using namespace pvrtexlib;

#define COLOR_COMPONENTS 4 // RGBA

#ifdef LINUX_BUILD
    #include <X11/Xmd.h> // for INT16, INT32
    #pragma pack(1)
    struct BITMAPFILEHEADER 
    {
    
        INT16 bfType;
        INT32 bfSize;
        INT16 bfReserved1;
        INT16 bfReserved2;
        INT32 bfOffBits;
    };
    
    #pragma pack(1)
    struct BITMAPINFOHEADER 
    { 
      INT32 biSize; 
      INT32 biWidth; 
      INT32 biHeight; 
      INT16 biPlanes; 
      INT16 biBitCount; 
      INT32 biCompression; 
      INT32 biSizeImage; 
      INT32 biXPelsPerMeter; 
      INT32 biYPelsPerMeter; 
      INT32 biClrUsed; 
      INT32 biClrImportant; 
    }; 
    
    #pragma pack(1)
    struct RGBTRIPLE 
    {
      INT8 rgbtRed;
      INT8 rgbtGreen;
      INT8 rgbtBlue;
    };
#endif

#ifdef _WIN32
    #include <windows.h>
#endif

    

EDR_SurfacePtr EDR_LoadPVR(const char* filename)
{
    try
    {
        CPVRTexture texture(filename);
        
        CPVRTextureData texData = texture.getData();
        CPVRTextureHeader texHeader = texture.getHeader();
        
        PixelType type = texture.getPixelType(); // just interesting...

        EDR_SurfacePtr surface (new EDR_Surface());
        surface->w = texture.getWidth();
        surface->h = texture.getHeight();
        surface->size = texData.getDataSize();
        
        surface->pixels = new char[texData.getDataSize()];
        memcpy(surface->pixels, texData.getData(), texData.getDataSize());
       
        return surface;
    }
    PVRCATCH(exeption)
    {
        throw runtime_error(string("PVR ERROR: ") + exeption.what());
    }
    
}

EDR_SurfacePtr EDR_LoadBMP(const char* filename)
{
    TRY_BEGINS;
    
#ifndef BM
#define BM 0x4D42
#endif
    


    BITMAPFILEHEADER bmpHeader = {0};
    BITMAPINFOHEADER bmpInfo = {0};
    RGBTRIPLE rgb = {0};

    if(!filename) 
        throw runtime_error("Filename not set");

    // Open the file
    FILE * file;
    if( !(file = fopen(filename, "rb")))
    {
        throw runtime_error(string("Can't open file: ") + string(filename));
    }

    // Read the bmp header and check for a valid file
    if(!fread(&bmpHeader, sizeof(bmpHeader), 1, file)) 
    {
        fclose(file);
        throw runtime_error(string("Can't read BMP header") + string(filename));
    }
    if(bmpHeader.bfType != BM) 
    {
        fclose(file);
        throw runtime_error(string("Not a BMP file") + string(filename));
    }

    // Read the infoheader
    if(!fread(&bmpInfo, sizeof(bmpInfo), 1, file)) 
    {
        fclose(file);
        throw runtime_error(string("Can't read BMP info") + string(filename));
    }

    // Get the informations
    int width = bmpInfo.biWidth;
    int height = bmpInfo.biHeight;
    int bpp = bmpInfo.biBitCount;

    if(bmpInfo.biCompression || bpp != 24) 
    {
        fclose( file );
        throw runtime_error(string("Can't read compressed BMP") + string(filename));
    }

    char* pixels = new char[width * height * COLOR_COMPONENTS];
    if(!pixels) 
    {
        fclose(file);
        throw runtime_error(string("Can't allocate memory for surface") + string(filename));
    }

    // Read the pixels
    unsigned base = 0;
    for (int j=height-1; j >= 0 ; j--) 
    {
        for (int i=0; i < width; i++) 
        {
            if(!fread(&rgb, sizeof(rgb), 1, file)) 
            {
                delete[] pixels;
                fclose(file);
                throw runtime_error(string("Can't read BMP pixels") + string(filename));
            }
           
            pixels[base + 3] = (rgb.rgbtRed == 0 && rgb.rgbtGreen == 0 && rgb.rgbtBlue == 0) ? 
                              0 : 255;
            pixels[base + 2] = rgb.rgbtRed;
            pixels[base + 1] = rgb.rgbtGreen;
            pixels[base + 0] = rgb.rgbtBlue;

            base += COLOR_COMPONENTS;
        }
    }

    fclose(file);

    EDR_SurfacePtr surface (new EDR_Surface());
    surface->pixels = pixels;
    surface->w = width;
    surface->h = height;
    
    return surface;

    TRY_RETHROW;
}