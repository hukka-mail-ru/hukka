#ifndef Glbasic_H_
#define Glbasic_H_


#ifdef LINUX_BUILD
    #include <GL/gl.h>
  //  #include <GLES/gl.h>
#endif

#ifdef WIN_BUILD
    #include <GLES/gl.h>
#endif

void gluPerspective (double fovy, double aspect, double zNear, double zFar);
int gluUnProject (double winX, double winY, double winZ, const double *model, const double *proj, const int *view, double* objX, double* objY, double* objZ);

#endif /*Glbasic_H_*/
