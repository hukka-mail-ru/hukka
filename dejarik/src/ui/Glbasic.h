#ifndef Glbasic_H_
#define Glbasic_H_


#ifdef LINUX_BUILD
    #include <GL/gl.h>
#endif

#ifdef WIN_BUILD
    #include <GLES/gl.h>
#endif

GLAPI void GLAPIENTRY gluPerspective (GLdouble fovy, GLdouble aspect, GLdouble zNear, GLdouble zFar);
GLAPI GLint GLAPIENTRY gluUnProject (GLdouble winX, GLdouble winY, GLdouble winZ, const GLdouble *model, const GLdouble *proj, const GLint *view, GLdouble* objX, GLdouble* objY, GLdouble* objZ);

#endif /*Glbasic_H_*/
