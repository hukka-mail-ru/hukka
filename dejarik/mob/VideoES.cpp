#include "VideoES.h"


using namespace std;

Texture texture1;

//----------------------------------------------------------------------------
void Video::perspective(GLfloat fovy, GLfloat aspect, GLfloat zNear,  GLfloat zFar)
{
    GLfixed xmin, xmax, ymin, ymax, aspectFixed, znearFixed;     

    aspectFixed = FixedFromFloat(aspect);
    znearFixed = FixedFromFloat(zNear);

    ymax = MultiplyFixed(znearFixed, FixedFromFloat((GLfloat)tan(fovy * 3.1415962f / 360.0f)));  
    ymin = -ymax;

    xmin = MultiplyFixed(ymin, aspectFixed);
    xmax = MultiplyFixed(ymax, aspectFixed);  
    glFrustumx(xmin, xmax, ymin, ymax, znearFixed, FixedFromFloat(zFar));
}
//----------------------------------------------------------------------------
void Video::setPerspective(HWND hWnd)
{
    RECT r;
    GetWindowRect(hWnd, &r);    
    float ratio = (float)(r.right - r.left)/(r.bottom - r.top);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();        
    perspective(45.0f,ratio, 1.0f, 40.0f);
    glMatrixMode(GL_MODELVIEW);
}
//----------------------------------------------------------------------------

wstring Video::getCurDir()
{
    WCHAR buf[MAXPATHLEN];
    GetModuleFileName(0, buf, MAXPATHLEN);
    wstring dir(buf);
    
    size_t pos = dir.rfind(L"\\");
    dir = dir.substr(0, pos);
    
    return dir;
}

//----------------------------------------------------------------------------
// Load BMP files (24 bpp only!)
int Video::loadTexture(HWND hWnd, LPCWSTR filename, Texture& texture) 
{
#ifndef BM
#define BM 0x4D42
#endif

    unsigned char* pixels;
    int i, j, base;
    int result;
    int width;
    int height;
    int bpp;

    BITMAPFILEHEADER bmpHeader = {0};
    BITMAPINFOHEADER bmpInfo = {0};
    RGBTRIPLE rgb = {0};

    if(!filename) return 0;

    // Open the file
    FILE * file;
    if( !(file = _wfopen(filename, L"rb")))
        return 0;

    // Read the bmp header and check for a valid file
    result = fread(&bmpHeader, sizeof(bmpHeader), 1, file);
    if(!result) 
    {
        fclose(file);
        return 0;
    }
    if(bmpHeader.bfType != BM) 
    {
        fclose(file);
        return 0;
    }

    // Read the infoheader
    result = fread(&bmpInfo, sizeof(bmpInfo), 1, file);
    if(!result) 
    {
        fclose(file);
        return 0;
    }

    // Get the informations
    width = bmpInfo.biWidth;
    height = bmpInfo.biHeight;
    bpp = bmpInfo.biBitCount;

    if(bmpInfo.biCompression || bpp !=24) 
    {
        fclose( file );
        return 0;
    }

    pixels = (unsigned char*)malloc(width * height * 3);
    if(!pixels) 
    {
        fclose(file);
        return 0;
    }
    memset(pixels, 0, width * height * 3);
    base = 0;

    // Read the pixels
    for (j=0; j < height; j++) 
        for (i=0; i < width; i++) 
        {
            result = fread(&rgb, sizeof(rgb), 1, file);
            if(!result) 
            {
                free(pixels);
                fclose(file);
                return 0;
            }
            pixels[base+0] = rgb.rgbtRed;
            pixels[base+1] = rgb.rgbtGreen;
            pixels[base+2] = rgb.rgbtBlue;
            base += 3;
        }

    fclose(file);

    glGenTextures(1, &texture.id);
    glBindTexture(GL_TEXTURE_2D, texture.id);
    glTexParameterx(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameterx(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameterx(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameterx(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, pixels);  

    texture.w = width;
    texture.h = height;

    // Free the data
    free(pixels);
    return 1;
}


//----------------------------------------------------------------------------
bool Video::initGLES(HWND hWnd)
{  
    EGLConfig configs[10];
    EGLint matchingConfigs;	

    /*configAttribs is a integers list that holds the desired format of 
    our framebuffer. We will ask for a framebuffer with 24 bits of 
    color and 16 bits of z-buffer. We also ask for a window buffer, not 
    a pbuffer or pixmap buffer*/	
    const EGLint configAttribs[] =
    {
        EGL_RED_SIZE,       8,
        EGL_GREEN_SIZE,     8,
        EGL_BLUE_SIZE,      8,
        EGL_ALPHA_SIZE,     EGL_DONT_CARE,
        EGL_DEPTH_SIZE,     16,
        EGL_STENCIL_SIZE,   EGL_DONT_CARE,
        EGL_SURFACE_TYPE,   EGL_WINDOW_BIT,
        EGL_NONE,           EGL_NONE
    };

    HDC hDC = GetWindowDC(hWnd);
    mGlesDisplay = eglGetDisplay(hDC);	 //Ask for an available display

    //Display initialization (we don't care about the OGLES version numbers)
    if(!eglInitialize(mGlesDisplay, NULL, NULL)) 
        return FALSE;

    /*Ask for the frame buffer configuration that best fits our 
    parameters. At most, we want 10 configurations*/
    if(!eglChooseConfig(mGlesDisplay, configAttribs, &configs[0], 10,  &matchingConfigs)) 
        return FALSE;

    //If there isn't any configuration enough good
    if (matchingConfigs < 1)  return FALSE;	  

    /*eglCreateWindowSurface creates an on screen EGLSurface and returns 
    a handle  to it. Any EGL rendering context created with a 
    compatible EGLConfig can be used to render into this surface.*/
    mGlesSurface = eglCreateWindowSurface(mGlesDisplay, configs[0], hWnd, configAttribs);	
    if(!mGlesSurface) return FALSE;

    // Let's create our rendering context
    mGlesContext=eglCreateContext(mGlesDisplay,configs[0],0,configAttribs);

    if(!mGlesContext) return FALSE;

    //Now we will activate the context for rendering	
    eglMakeCurrent(mGlesDisplay, mGlesSurface, mGlesSurface, mGlesContext); 

    /*Remember: because we are programming for a mobile device, we cant 
    use any of the OpenGL ES functions that finish in 'f', we must use 
    the fixed point version (they finish in 'x'*/
    glClearColorx(0, 0, 0, 0);
    glShadeModel(GL_SMOOTH);  
    
    RECT r;
    GetWindowRect(hWnd, &r);  
    glViewport(r.left, r.top, r.right - r.left, r.bottom - r.top);		  
    setPerspective(hWnd);
    
    
    wstring filename = getCurDir() + L"\\img\\menu.bmp";
    
    if(!loadTexture(hWnd, filename.c_str(), texture1))
        return FALSE;
    
    return TRUE;
}


void Video::drawPolygon(float* vertexArray, unsigned vertNum, const RGBA_Color& color)
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);  
    glLoadIdentity();  

    glTranslatex( FixedFromFloat(0.0f), 
        FixedFromFloat(0.0f), 
        FixedFromFloat(-10.0f) );


    glColor4x(
        FixedFromFloat(color.r), 
        FixedFromFloat(color.b), 
        FixedFromFloat(color.g), 
        FixedFromFloat(color.a));

    glEnableClientState(GL_VERTEX_ARRAY);

    glVertexPointer(3, GL_FLOAT, 0, vertexArray);
    glDrawArrays(GL_TRIANGLE_FAN, 0, vertNum);

    glDisableClientState(GL_VERTEX_ARRAY);
}


void Video::drawImage(const Texture& texture, const RGBA_Color& color, 
               float winX, float winY, float angle)
{       
    float x1 = winX;
    float y1 = winY;
    float z1 = 0;
    
    float x2 = winX  + texture.w;
    float y2 = winY  + texture.h;
    float z2 = 0;
    
        
    glPushMatrix();
    glEnable( GL_TEXTURE_2D );

    glBindTexture(GL_TEXTURE_2D, texture.id);
    
    glColor4x(
        FixedFromFloat(color.r), 
        FixedFromFloat(color.b), 
        FixedFromFloat(color.g), 
        FixedFromFloat(color.a));
        
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);  
    glLoadIdentity();  

    glTranslatex( FixedFromFloat(0.0f), 
        FixedFromFloat(0.0f), 
        FixedFromFloat(-10.0f) );
/*
    glTranslatex(FixedFromFloat((x1+x2)/2), 
                 FixedFromFloat((y1+y2)/2), 
                 ZERO); // rotate [move to the coordinate center]
                 
    glRotatex(FixedFromFloat(angle) ,ZERO, ZERO, ONE); // rotation
    
    glTranslatex(FixedFromFloat(-(x1+x2)/2), 
                 FixedFromFloat(-(y1+y2)/2), 
                 ZERO); // move back to the old position
*/
    const float vertices []=
    {
        x1,  y1, 0,
        x2,  y1, 0,
        x2,  y2, 0,
        x1,  y2, 0,
    };

    const float texCoords[] = 
    {
        0, 0,
        1, 0,
        1, 1,
        0, 1,
    };

    glVertexPointer(3, GL_FLOAT, 0, vertices);
    glTexCoordPointer(2, GL_FLOAT, 0, texCoords);
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);

    glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);

    glDisable( GL_TEXTURE_2D );
    glPopMatrix();

}

//----------------------------------------------------------------------------
void Video::drawAll()
{

        
    float vertexArray[] = 
    {
        0,0,0,   
        1,0,0,     
        1,1,0,
        0,1,0 
    };

  //  drawPolygon(&vertexArray[0], 4, RGBA_Color(1,1,1,0.5));

    drawImage(texture1, RGBA_Color(1,1,1,0.5), 0, 0, 0);
        
    eglSwapBuffers(mGlesDisplay, mGlesSurface);
}





//----------------------------------------------------------------------------
void Video::quitGLES()
{
    

    if(mGlesDisplay)
    {
        eglMakeCurrent(mGlesDisplay, NULL, NULL, NULL);  
        if(mGlesContext) eglDestroyContext(mGlesDisplay, mGlesContext);
        if(mGlesSurface) eglDestroySurface(mGlesDisplay, mGlesSurface);
        eglTerminate(mGlesDisplay);
    }
}


//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////


/*
void Video::winToGL(float winX, float winY, float& x, float& y, float& z)
{

    // TODO Rrrrhh! The magic numbers should be somehow calculated, based on GL_PROJECTION and GL_MODELVIEW
    float modelview[16] = {1,0,0,0,0,1,0,0,0,0,1,0,0,0,-10,1} ;              
    float projection[16] = {3.218951,0,0,0,0,2.414213,0,0,0,0,-1.002002,-1,0,0,-0.2002002,0};


    winY = (float)viewport[3] - winY;           // Subtract The Current Mouse Y Coordinate From The Screen Height

    GLfloat winZ = 0.990991; // A magic number :( because opengl ES doesn't have GL_DEPTH_COMPONENT   
    // glReadPixels(winX, winY, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, &winZ);    

    gluUnProject( winX, winY, winZ, modelview, projection, viewport, &x, &y, &z);

}
*/