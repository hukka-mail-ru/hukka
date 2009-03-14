#ifndef Glbasic_H_
#define Glbasic_H_


#ifdef OPENGL_BUILD
    #include <GL/gl.h>
#endif

#ifdef OPENGL_ES_BUILD
    #define GL_GLEXT_PROTOTYPES

    #include <GLES/gl.h>
    #include <GLES/glext.h>

#endif

void gluPerspective (float fovy, float aspect, float zNear, float zFar);
int gluUnProject (float winX, float winY, float winZ, const float *model, const float *proj, const int *view, float* objX, float* objY, float* objZ);

#endif /*Glbasic_H_*/
