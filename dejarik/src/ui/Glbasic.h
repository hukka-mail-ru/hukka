#ifndef Glbasic_H_
#define Glbasic_H_


#ifdef OPENGL_BUILD
    #include <GL/gl.h>
#endif

#ifdef OPENGL_ES_BUILD
    #include <GLES/gl.h>
    #include <GLES/glext.h>

    #undef GL_OES_single_precision
    #define GL_GLEXT_PROTOTYPES
#endif

void gluPerspective (double fovy, double aspect, double zNear, double zFar);
int gluUnProject (double winX, double winY, double winZ, const double *model, const double *proj, const int *view, double* objX, double* objY, double* objZ);

#endif /*Glbasic_H_*/
