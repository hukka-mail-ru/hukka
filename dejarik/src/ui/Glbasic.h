#ifndef Glbasic_H_
#define Glbasic_H_


//    #define GL_GLEXT_PROTOTYPES

    #include <GLES/gl.h>


// just a copy-paste from GLU.

void gluPerspective (float fovy, float aspect, float zNear, float zFar);
int gluUnProject (float winX, float winY, float winZ, const float *model, const float *proj, const int *view, float* objX, float* objY, float* objZ);


#endif /*Glbasic_H_*/
