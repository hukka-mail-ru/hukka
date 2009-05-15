#include <iostream>
#include "Video.h"
#include "Window.h"
#include "System.h"


using namespace std;


/*
#define PRECISION 16    
#define ONE (1 << PRECISION)
#define ZERO 0


inline GLfixed FixedFromInt(int value) {return value << PRECISION;}
inline GLfixed FixedFromFloat(float value) {return static_cast<GLfixed>(value * static_cast<float>(ONE));}
inline GLfixed MultiplyFixed(GLfixed op1, GLfixed op2) {return (op1 * op2) >> PRECISION;};
*/
    
// virtual scene size
#define SCREEN_WIDTH  800 // must be big enough
#define SCREEN_HEIGHT 1000 // must be big enough
    
   


void Video::startup()
{
    TRY_BEGINS;
    
    EDR_CreateWindow(WINDOW_WIDTH, WINDOW_HEIGHT, "Dejarik");   
    
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();   
    
    // The center of coordinates must be at the center of the game board  
    // (a litte upper of geometrical window center) 
    glViewport(-(SCREEN_WIDTH - WINDOW_WIDTH)/2,
    		   -(SCREEN_HEIGHT - WINDOW_HEIGHT)/2  + (WINDOW_HEIGHT/2 - FIELD_TEXTURE_WIDTH),
    		   SCREEN_WIDTH, 
    		   SCREEN_HEIGHT);

    glOrthof((-SCREEN_WIDTH/2),  (SCREEN_WIDTH/2),
     	     (-SCREEN_HEIGHT/2), (SCREEN_HEIGHT/2),
    	     (0) , (1));

    glMatrixMode(GL_MODELVIEW);
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
    

    // 2D rendering
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_FOG);
    glDisable(GL_LIGHTING);
    glDisable(GL_ALPHA_TEST);
    glDisable(GL_DITHER);

    checkError();
    
    // Load all the textures 
    createTextures();
    
    TRY_RETHROW;
}



void Video::drawSolidPolygon(const GLshort* vertexArray, unsigned vertNum, const RGBA_Color& color)
{
    TRY_BEGINS;
    
    glLoadIdentity();
    
    glColor4f(color.r, color.b, color.g, color.a);    

    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(2, GL_SHORT, 0, vertexArray);
    
    glDrawArrays(GL_TRIANGLE_FAN, 0, vertNum);

    glDisableClientState(GL_VERTEX_ARRAY);
    
    checkError();
    
    TRY_RETHROW;
}


void Video::drawLineLoop(const GLshort* vertexArray, unsigned vertNum, const RGBA_Color& color,
        float width)
{
    TRY_BEGINS;
    
    glLoadIdentity();
    
    glColor4f(color.r, color.b, color.g, color.a);    
    
    glLineWidth(width);
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(2, GL_SHORT, 0, vertexArray);
    
    glDrawArrays(GL_LINE_LOOP, 0, vertNum);

    glDisableClientState(GL_VERTEX_ARRAY);
    
    checkError();
    
    TRY_RETHROW;
}


void Video::drawSprite(const std::string& texName, const unsigned fragmentID, 
                       const RGBA_Color& color, BindXY bindXY, 
                       GLfloat x, GLfloat y, float angle)
{
    TRY_BEGINS;
    
    if(!textures[texName])
    {
        ostringstream os;
        os << "Can't find name " << texName;
        throw runtime_error(os.str()); 
    }
      
    TexturePtr texture = textures[texName];
    
    if(!texture->fragmentsInRow)
    {   
        ostringstream os;
        os << "fragmentsInRow = 0 in " << texName << endl;
        throw runtime_error(os.str());
    }
    
    // position of the sprite
    const unsigned width = texture->surface->width;
    switch(bindXY)
    {
        case XY_CENTER:
            x -= width / 2;
            y -= width / 2;
            break;
        case XY_RIGHT_BOTTOM:
            x -= width;
            break;
        case XY_LEFT_TOP:
            y -= width;
            break;
        case XY_RIGHT_TOP:
            x -= width;
            y -= width;
            break;            
        case XY_LEFT_BOTTOM:
            break;
    }

    // Set vertex coordinatex
    GLfloat x1 = x;
    GLfloat y1 = y;
    GLfloat x2 = x + width;
    GLfloat y2 = y + width;
    // Set texture coordinates
    GLfloat texWidth = 1.0;  
    GLfloat texX = 0.0;
    GLfloat texY = 0.0;
    
    // Deal with fragments (subtextures) if they exist
    if(texture->fragmentsInRow > 1) 
    {
        GLshort dw = width / texture->fragmentsInRow;    
        x2 = x + dw;
        y2 = y + dw;
        
        GLshort shift = width/2 - (x2-x1)/2; 
        x1 += shift;
        y1 += shift;
        x2 += shift;
        y2 += shift;
        
        texWidth = 1.0 / texture->fragmentsInRow;  
        texX = fragmentID % texture->fragmentsInRow * texWidth;
        texY = fragmentID / texture->fragmentsInRow * texWidth;
    }
    
    // Set arrays
    const GLfloat vertices []=
    {
        x1,  y1,
        x2,  y1,
        x2,  y2,
        x1,  y2,
    };   
               
    const GLfloat texCoords[] = 
    {
        texX,            texY,
        texX + texWidth, texY,
        texX + texWidth, texY + texWidth,
        texX,            texY + texWidth
    };
 
    // Draw operations
    glLoadIdentity();
    glEnable( GL_TEXTURE_2D );
    glBindTexture(GL_TEXTURE_2D, texture->id);
       
    glColor4f(color.r, color.b, color.g, color.a);    
    
    // rotation
    glTranslatef((x1+x2)/2.0, (y1+y2)/2.0,  0.0); 
    glRotatef(angle , 0.0, 0.0, 1.0); 
    glTranslatef(-(x1+x2)/2.0, -(y1+y2)/2.0, 0.0); 
    
    
    glVertexPointer(2, GL_FLOAT, 0, vertices);
    glTexCoordPointer(2, GL_FLOAT, 0, texCoords);
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY); 

    //    glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

        GLushort index[] = { 0,1,2,0,2,3};
        glDrawElements( GL_TRIANGLE_FAN,6, GL_UNSIGNED_SHORT, index ); // Crash, but if I write glDrawArrays it works!

        
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);

    glDisable( GL_TEXTURE_2D );
    
    checkError();

    TRY_RETHROW;
}


void Video::enableBlend()
{
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}


void Video::disableBlend()
{
    glDisable(GL_BLEND);
}


void Video::copyBufferIntoTexture(const std::string& texName, GLint x, GLint y)
{
    TRY_BEGINS;
    
    if(!textures[texName])
    {
        ostringstream os;
        os << "Can't find name " << texName;
        throw runtime_error(os.str()); 
    }
    
    TexturePtr texture = textures[texName];
    
    glBindTexture(GL_TEXTURE_2D, texture->id);
    
    const unsigned width = texture->surface->width;
    glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, x, y, width, width, 0);
    
  //  checkError();
    
    TRY_RETHROW;
}


int Video::getTextureSize(int format, int width, int height)
{
    int base = 0; // e.g. palette size
    int bpp = 0;

    switch (format)
    {
        // Palette formats
        case GL_PALETTE8_RGB8_OES:
            bpp = 8;
            base = 256*3;
            break;
        case GL_PALETTE8_RGBA8_OES:
            bpp = 8;
            base = 256*4;
            break;
        case GL_PALETTE8_R5_G6_B5_OES:
        case GL_PALETTE8_RGBA4_OES:
        case GL_PALETTE8_RGB5_A1_OES:
            bpp = 8;
            base = 256*2;
            break;
            
            

        case GL_PALETTE4_RGB8_OES:
            bpp = 4;
            base = 16*3;
            break;
        case GL_PALETTE4_RGBA8_OES:
            bpp = 4;
            base = 16*4;
            break;
        case GL_PALETTE4_R5_G6_B5_OES:
        case GL_PALETTE4_RGBA4_OES:
        case GL_PALETTE4_RGB5_A1_OES:
            bpp = 4;
            base = 16*2;
            break;
    }


   return base + (width * height * bpp / 8);
}

void Video::createTexture(const char* dir, const char* name, unsigned fragmentsInRow)
{
    TRY_BEGINS;
    
    TexturePtr texture (new Texture); 
    texture->fragmentsInRow = fragmentsInRow;
    
    ostringstream path;
    path << EDR_GetCurDir() << dir << "/" << name << ".bmp";

    /* Create storage space for the texture */
    texture->surface = EDR_LoadBMP(path.str().c_str()); 

    /* Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit */
    if (texture->surface)
    {
        /* Create The Texture */
        glGenTextures(1, &texture->id);

        /* Typical Texture Generation Using Data From The Bitmap */
        glBindTexture(GL_TEXTURE_2D, texture->id);

        /* Linear Filtering */
        glTexParameterx(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
        glTexParameterx(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
        glTexParameterx(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
        glTexParameterx(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
        
        /* Generate The Texture */
        const unsigned width = texture->surface->width;
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, width, 0, 
                     GL_RGBA, GL_UNSIGNED_BYTE, texture->surface->pixels );
    }

    checkError();
    
    textures[name] = texture;  
    
    TRY_RETHROW;
}


void Video::createEmptyTexture(const char* name, unsigned short width)
{
    TRY_BEGINS;
    
    TexturePtr texture (new Texture); 
    
    texture->surface = EDR_SurfacePtr(new EDR_Surface());
    texture->surface->width = width;
    texture->surface->pixels = new unsigned char[width * width * 4];
        
    /* Create The Texture */
    glGenTextures(1, &texture->id);
    
    /* Typical Texture Generation Using Data From The Bitmap */
    glBindTexture(GL_TEXTURE_2D, texture->id);

    /* Generate The Texture */
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, width, 0, GL_RGBA, // blue chanel must be changed by red 
                 GL_UNSIGNED_BYTE, texture->surface->pixels );

    /* Linear Filtering */
    glTexParameterx( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    glTexParameterx( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );

    checkError();
    
    textures[name] = texture;  
    
    TRY_RETHROW;
}


void Video::createCompressedTexture(const char* dir, const char* name)
{
    TRY_BEGINS;

    TexturePtr texture (new Texture); 
    
    ostringstream path;
    path << EDR_GetCurDir() << dir << "/" << name << ".pcx"; 
    
    // Create storage space for the texture 
    texture->surface = EDR_LoadPCX(path.str().c_str());

    // Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit 
    if (texture->surface)
    {
        glGenTextures(1, &texture->id);

        glBindTexture(GL_TEXTURE_2D, texture->id);
 
        const unsigned width = texture->surface->width;
        glCompressedTexImage2D (GL_TEXTURE_2D, 0,  GL_PALETTE8_RGB8_OES, 
                width, width, 0, 
                getTextureSize(GL_PALETTE8_RGB8_OES, width, width),
                texture->surface->pixels);

        glTexParameterx( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
        glTexParameterx( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    }

    checkError();
    
    textures[name] = texture;  
 
    TRY_RETHROW;
}


void Video::createTextures()
{
    TRY_BEGINS;
      

    createTexture("img", "pieces", 4);


    createCompressedTexture("img", "board1");
    createCompressedTexture("img", "board2");
    createCompressedTexture("img", "board3");
    createCompressedTexture("img", "board4");
    
    createCompressedTexture("img", "menu1");
    createCompressedTexture("img", "menu2");
    
    createEmptyTexture("field1", FIELD_TEXTURE_WIDTH);
    createEmptyTexture("field2", FIELD_TEXTURE_WIDTH);
    createEmptyTexture("field3", FIELD_TEXTURE_WIDTH);
    createEmptyTexture("field4", FIELD_TEXTURE_WIDTH);
    createEmptyTexture("field_center", CENTRAL_TEXTURE_WIDTH);
    
    
    TRY_RETHROW;
}


void Video::checkError(const std::string where)
{
    TRY_BEGINS;
    GLenum err = glGetError();
    if(err != GL_NO_ERROR)
    {
        ostringstream os;
        os << where << " Error 0x" << hex << err << endl;
        throw runtime_error(os.str());
    }
    TRY_RETHROW;
}

void Video::clearScreen()
{
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
}
