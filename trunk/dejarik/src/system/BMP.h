#ifndef BMP_H_
#define BMP_H_

struct EDR_Surface // for BMP loading
{
    char* pixels;
    unsigned size;
    unsigned w;
    unsigned h;
};

EDR_Surface* EDR_LoadBMP(const char* filename);
EDR_Surface* EDR_LoadPVR(const char* filename);

void EDR_FreeSurface(EDR_Surface* surface);
    
#endif /*BMP_H_*/
