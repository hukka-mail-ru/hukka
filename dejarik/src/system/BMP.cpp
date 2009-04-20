#include "BMP.h"
#include <GLES/gl.h>  
#include <vector>
using namespace std;


#define TWO_BYTES 2 // RGBA
#define FOUR_BYTES 4 // RGBA

#ifdef _WIN32
#include <windows.h>
#endif

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

/* PVR Header */
#pragma pack(1)
struct PVRHEADER
{
    INT32 dwHeaderSize;
    INT32 dwHeight;
    INT32 dwWidth;
    INT32 dwMipMapCount;
    INT32 dwpfFlags;
    INT32 dwDataSize;
    INT32 dwBitCount;
    INT32 dwRBitMask;
    INT32 dwGBitMask;
    INT32 dwBBitMask;
    INT32 dwAlphaBitMask;
    INT32 dwPVR;
    INT32 dwNumSurfs;
};

#pragma pack(1)
struct COLOR565
{
    INT8 first;
    INT8 second;
};

#pragma pack(1)
struct PCXHEADER
{
    INT8  manufacturer; // must be 0x0a
    INT8  version;
    INT8  encoding; // must be 0x01
    INT8  bitsPerPixel;
    INT16 xMin;
    INT16 yMin;
    INT16 xMax;
    INT16 yMax;
    INT16 hRes;
    INT16 wRes;
    INT8  colorMap[48];
    INT8  reserved;
    INT8  nPlanes;
    INT16 bytesPerLine;
    INT16 paletteInfo;
    INT8  filler[58];
};










EDR_SurfacePtr EDR_LoadPCX(const char* filename)
{
    TRY_BEGINS;

    
    PCXHEADER pcxHeader = {0};

    if(!filename) 
        throw runtime_error("Filename not set");

    // Open the file
    FILE * file;
    if( !(file = fopen(filename, "rb")))
    {
        throw runtime_error(string("Can't open file: ") + string(filename));
    }

    // Read the header 
    if(!fread(&pcxHeader, sizeof(pcxHeader), 1, file)) 
    {
        fclose(file);
        throw runtime_error(string("Can't read PVR header") + string(filename));
    }
    if(pcxHeader.manufacturer != 0x0a || pcxHeader.encoding != 0x01) 
    {
        fclose(file);
        throw runtime_error(string("Not a PCX file") + string(filename));
    }

    int width = pcxHeader.xMax - pcxHeader.xMin + 1;
    int height = pcxHeader.yMax - pcxHeader.yMin + 1;

    
    // Read the pixels
    unsigned palette_size = 256*3;
    int file_size = 0;
   
    char compressed_pixels[width * height * TWO_BYTES + palette_size];
  
    while(fread(&compressed_pixels[file_size], sizeof(char), 1, file)) 
    {
        file_size++;
    }



    int beacon = file_size - palette_size - 1;
    
    // for uncompressed pixels
    char* pixels = new char[palette_size + width * height];
    
    // GL wants the palette to be before the image data
    memcpy(pixels, compressed_pixels + beacon + 1, palette_size);
    
    // unpack PCX
    int k=palette_size;
    for(int i=0; i<beacon; i++)
    {
     //   if(k > palette_size + width * height) 
     //       break;
        
        unsigned char byte = compressed_pixels[i];
        if(byte > 192) // Two high bits are set = Repeat
        {
            byte -= 192; // Repeat how many times?            
            char color = compressed_pixels[++i];
            for(int j=0; j<byte; j++)
            {
                pixels[k++] = color;
            }
        }
        else
        {
            char color = byte;
            pixels[k++] = color;
        }
    }
    

    
    // Flip vertically
    unsigned base = 0;
    for (int j=height-1; j > height/2; j--) 
    {
        for (int i=0; i < width; i++) 
        {
            char temp = pixels[palette_size + j*width + i];
            pixels[palette_size + j*width + i] = pixels[palette_size + base];
            pixels[palette_size + base] = temp;
            base ++;
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


EDR_SurfacePtr EDR_LoadPVR(const char* filename)
{
    TRY_BEGINS;
    
    PVRHEADER pvrHeader = {0};
    COLOR565 color565 = {0};
    
    if(!filename) 
        throw runtime_error("Filename not set");

    // Open the file
    FILE * file;
    if( !(file = fopen(filename, "rb")))
    {
        throw runtime_error(string("Can't open file: ") + string(filename));
    }
    
    // Read the header 
    if(!fread(&pvrHeader, sizeof(pvrHeader), 1, file)) 
    {
        fclose(file);
        throw runtime_error(string("Can't read PVR header") + string(filename));
    }
    
    int width = pvrHeader.dwWidth;
    int height = pvrHeader.dwHeight;
    
    char* pixels = new char[width * height * TWO_BYTES]; // 16 bits per pixel
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
            if(!fread(&color565, sizeof(color565), 1, file)) 
            {
                delete[] pixels;
                fclose(file);
                throw runtime_error(string("Can't read BMP pixels") + string(filename));
            }
           
            pixels[base + 1] = color565.second;
            pixels[base + 0] = color565.first;

            base += TWO_BYTES;
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

    char* pixels = new char[width * height * FOUR_BYTES]; // 24 bits per pixel + 8 bits alpha channel
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

            base += FOUR_BYTES;
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