#ifndef BMP_H_
#define BMP_H_

#include "Macros.h"

struct EDR_Surface // for BMP loading
{
    EDR_Surface(): pixels(0), size(0), w(0), h(0) {}
    ~EDR_Surface() { delete[] pixels; }
    
    unsigned char* pixels;
    unsigned size;
    unsigned w;
    unsigned h;
};

CLASSPTR(EDR_Surface);

EDR_SurfacePtr EDR_LoadBMP(const char* filename);
EDR_SurfacePtr EDR_LoadPVR(const char* filename);
EDR_SurfacePtr EDR_LoadPCX(const char* filename);

    
#endif /*BMP_H_*/
