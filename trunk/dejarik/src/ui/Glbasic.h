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

void gluPerspective (double fovy, double aspect, double zNear, double zFar);
int gluUnProject (double winX, double winY, double winZ, const double *model, const double *proj, const int *view, double* objX, double* objY, double* objZ);

#endif /*Glbasic_H_*/
