#ifndef Glbasic_H_
#define Glbasic_H_


#ifdef OPENGL_BUILD
    #include <GL/gl.h>
#endif

#ifdef OPENGL_ES_BUILD
    #define GL_GLEXT_PROTOTYPES

    #include <GLES/gl.h>
//    #include <GLES/glext.h>

#endif

// just a copy-paste from GLU.

void gluPerspective (float fovy, float aspect, float zNear, float zFar);
int gluUnProject (float winX, float winY, float winZ, const float *model, const float *proj, const int *view, float* objX, float* objY, float* objZ);

// x-version must be replaced be f-version in "normal" opengl 
#ifdef OPENGL_BUILD
void glTranslatex(float x, float y, float z);
void glRotatex(float angle, float x, float y, float z);
void glFrustumx(float left, float right, float bottom, float top, float near, float far);
#endif

#endif /*Glbasic_H_*/
