#include <iostream>
#include "Video.h"
#include "Window.h"
#include "System.h"
#include "BMP.h"

using namespace std;



#define PRECISION 16    
#define ONE (1 << PRECISION)
#define ZERO 0


inline GLfixed FixedFromInt(int value) {return value << PRECISION;}
inline GLfixed FixedFromFloat(float value) {return static_cast<GLfixed>(value * static_cast<float>(ONE));}
inline GLfixed MultiplyFixed(GLfixed op1, GLfixed op2) {return (op1 * op2) >> PRECISION;};

    
// virtual scene size
#define SCREEN_WIDTH  800 // must be big enough
#define SCREEN_HEIGHT 1000 // must be big enough
    
   


void Video::startup(const std::vector<std::string>& pieceNames)
{
    TRY_BEGINS;
    
    EDR_CreateWindow(WINDOW_WIDTH, WINDOW_HEIGHT, "Dejarik");   
    
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();   
    
    // The center of coordinates must be at the center of the game board  
    // (a litte upper of geometrical window center) 
    glViewport(-(SCREEN_WIDTH - WINDOW_WIDTH)/2,
    		   -(SCREEN_HEIGHT - WINDOW_HEIGHT)/2  + (WINDOW_HEIGHT/2 - BOARD_TEXTURE_WIDTH),
    		   SCREEN_WIDTH, 
    		   SCREEN_HEIGHT);

    glOrthox(FixedFromInt(-SCREEN_WIDTH/2),  FixedFromInt(SCREEN_WIDTH/2),
     	     FixedFromInt(-SCREEN_HEIGHT/2), FixedFromInt(SCREEN_HEIGHT/2),
    	     FixedFromInt(0) , FixedFromInt(1));

    glMatrixMode(GL_MODELVIEW);
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
    


    // Optimization
   // glDisable(GL_DEPTH_TEST);

    // Load all the textures 
    createTextures(pieceNames);
    
    TRY_RETHROW;
}



void Video::drawSolidPolygon(const GLshort* vertexArray, unsigned vertNum, const RGBA_Color& color)
{
    glLoadIdentity();
    
    glColor4x(
        FixedFromFloat(color.r), 
        FixedFromFloat(color.b), 
        FixedFromFloat(color.g), 
        FixedFromFloat(color.a));

    glEnableClientState(GL_VERTEX_ARRAY);

    glVertexPointer(3, GL_SHORT, 0, vertexArray);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    
        glDrawArrays(GL_TRIANGLE_FAN, 0, vertNum);

    glDisable(GL_BLEND);
    glDisableClientState(GL_VERTEX_ARRAY);
}


void Video::drawLineLoop(const GLshort* vertexArray, unsigned vertNum, const RGBA_Color& color,
        float width)
{
    TRY_BEGINS;
    
    glLoadIdentity();
    
    glColor4x(
        FixedFromFloat(color.r), 
        FixedFromFloat(color.b), 
        FixedFromFloat(color.g), 
        FixedFromFloat(color.a));
    
    glLineWidthx(FixedFromFloat(width));

    glEnableClientState(GL_VERTEX_ARRAY);

    glVertexPointer(3, GL_SHORT, 0, vertexArray);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    
        glDrawArrays(GL_LINE_LOOP, 0, vertNum);

    glDisable(GL_BLEND);
    glDisableClientState(GL_VERTEX_ARRAY);
    
    TRY_RETHROW;
}

void Video::drawSprite(
        const std::string& texName, const RGBA_Color& color, 
        BindXY bindXY, GLshort x, GLshort y, float angle)
{
    TRY_BEGINS;
    
    
    
    if(!textures[texName])
    {
        ostringstream os;
        os << "Can't find name " << texName;
        throw runtime_error(os.str()); 
    }
    
    TexturePtr texture = textures[texName];
    
    if(bindXY == XY_CENTER)
    {
        x -= texture->w / 2;
        y -= texture->h / 2;
    }
    else if(bindXY == XY_RIGHT_BOTTOM)
    {
        x -= texture->w;
    }
    else if(bindXY == XY_LEFT_TOP)
    {
        y -= texture->h;
    }
    else if(bindXY == XY_RIGHT_TOP)
    {
        x -= texture->w;
        y -= texture->h;
    }

                  
    glLoadIdentity();
    glEnable( GL_TEXTURE_2D );
    glBindTexture(GL_TEXTURE_2D, texture->id);
    
    glColor4x(
        FixedFromFloat(color.r), 
        FixedFromFloat(color.b), 
        FixedFromFloat(color.g), 
        FixedFromFloat(color.a));

    GLshort x1 = x;
    GLshort y1 = y;
    
    GLshort x2 = x  + texture->w;
    GLshort y2 = y + texture->h;

    glTranslatex(FixedFromFloat((x1+x2)/2), 
                 FixedFromFloat((y1+y2)/2), 
                 ZERO); // rotate [move to the coordinate center]
                 
    glRotatex(FixedFromFloat(angle) ,ZERO, ZERO, ONE); // rotation
    
    glTranslatex(FixedFromFloat(-(x1+x2)/2), 
                 FixedFromFloat(-(y1+y2)/2), 
                 ZERO); // move back to the old position

    const GLshort vertices []=
    {
        x1,  y1, 0,
        x2,  y1, 0,
        x2,  y2, 0,
        x1,  y2, 0,
    };

    const GLshort texCoords[] = 
    {
        0, 0,
        1, 0,
        1, 1,
        0, 1,
    };

    glVertexPointer(3, GL_SHORT, 0, vertices);
    glTexCoordPointer(2, GL_SHORT, 0, texCoords);
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY); 

    if(texture->blended == BLENDED_ON) // optimization
    {
       glEnable(GL_BLEND);
       glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    }

        glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

    if(texture->blended == BLENDED_ON)
    {
       glDisable(GL_BLEND);
    }
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);

    glDisable( GL_TEXTURE_2D );

    TRY_RETHROW;
}


void Video::createTexture(const char* dir, const char* name, Blended blended)
{
    TRY_BEGINS;
    
    TexturePtr texture (new Texture); 

    texture->blended = blended;
    
    ostringstream path;
    path << EDR_GetCurDir() << dir << "/" << name << ".bmp";
    
    /* Status indicator */
    bool res = false;

    /* Create storage space for the texture */
    EDR_SurfacePtr surface = EDR_LoadBMP(path.str().c_str()); 

    /* Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit */
    if (surface)
    {

        /* Set the status to true */
        res = true;

        /* Create The Texture */
        glGenTextures(1, &texture->id);

        /* Typical Texture Generation Using Data From The Bitmap */
        glBindTexture(GL_TEXTURE_2D, texture->id);

        /* Generate The Texture */
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, surface->w, surface->h, 0, GL_RGBA, // blue chanel must be changed by red 
                     GL_UNSIGNED_BYTE, surface->pixels );
        
        texture->w = surface->w;
        texture->h = surface->h;

        /* Linear Filtering */
        glTexParameterx( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
        glTexParameterx( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    }

    GLenum err = glGetError();
    if(!res || err != GL_NO_ERROR)
    {
        ostringstream os;
        os << "Error " << err << "; Path: " << path.str() << endl;
        throw runtime_error(os.str());
    }
    
    textures[name] = texture;  
    
    TRY_RETHROW;
}

#include "OGLES/PVRTTextureAPI.h"
#include "PVRTResourceFile.h"

void Video::createCompressedTexture(const char* dir, const char* name, Blended blended)
{
    TRY_BEGINS;
    
    /*
    TexturePtr texture (new Texture); 

    texture->blended = blended;
    
    ostringstream path;
    path << EDR_GetCurDir() << dir << "/" << name << ".pvr";
    
    CPVRTResourceFile TexFile(path.str().c_str());
    if (!TexFile.IsOpen())
    {
        ostringstream os;
        os << "TexFile.IsOpen error. Path: " << path.str() << endl;
        throw runtime_error(os.str());
    }

    if(!PVRTLoadPartialTextureFromPointer(TexFile.DataPtr(), NULL, 0, &texture->id, NULL))
    {
        ostringstream os;
        os << "PVRTLoadPartialTextureFromPointer error. Path: " << path.str() << endl;
        throw runtime_error(os.str());
    }
    
    */
    TexturePtr texture (new Texture); 

    texture->blended = blended;
    
    ostringstream path;
    path << EDR_GetCurDir() << dir << "/" << name << ".pvr";
    
    // Status indicator 
    bool res = false;

    // Create storage space for the texture 
    EDR_SurfacePtr surface = EDR_LoadPVR(path.str().c_str());

    // Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit 
    if (surface)
    {

        res = true;

        glGenTextures(1, &texture->id);

        glBindTexture(GL_TEXTURE_2D, texture->id);

      //  glCompressedTexImage2D (GL_TEXTURE_2D, 0,  GL_PALETTE4_RGB5_A1_OES, 
      //          surface->w, surface->h, 0, 16*2+(128*128/2), surface->pixels);
               glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, surface->w, surface->h, 0, GL_RGB, // blue chanel must be changed by red 
                       GL_UNSIGNED_SHORT_5_6_5, surface->pixels );
        
        texture->w = surface->w;
        texture->h = surface->h;

        glTexParameterx( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
        glTexParameterx( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    }

    GLenum err = glGetError();
    if(!res || err != GL_NO_ERROR)
    {
        ostringstream os;
        os << "Error " << err << "; Path: " << path.str() << endl;
        throw runtime_error(os.str());
    }

    
    textures[name] = texture;  
    
    TRY_RETHROW;
}


void Video::createTextures(const std::vector<std::string>& names)
{
    TRY_BEGINS;
      
    createTexture("img/pieces", "Molator0", BLENDED_ON);
    createTexture("img/pieces", "Sarvip0", BLENDED_ON);
    createTexture("img/pieces", "Ghhhk0", BLENDED_ON);
    createTexture("img/pieces", "Monnok0", BLENDED_ON);
    createTexture("img/pieces", "Strider0", BLENDED_ON);
    createTexture("img/pieces", "Ngok0", BLENDED_ON);
    createTexture("img/pieces", "Klorslug0", BLENDED_ON);
    createTexture("img/pieces", "Houjix0", BLENDED_ON);
  
    
    createTexture("img", "ex");
    /*
    for(unsigned i =0; i<names.size(); ++i)
    {
        createTexture(names[i] +"0", IT_MASKED); 
        cout << names[i] +"0" << endl;
    }
    */
    /* 
    for(unsigned i =1; i<8; ++i)
    {
        ostringstream name;
        name << "Molator" << i;
        createTexture(name.str(), IT_MASKED);
    }
*/

    createCompressedTexture("img", "board1");
    createTexture("img", "board2");
    createTexture("img", "board3");
    createTexture("img", "board4");
    
    createTexture("img", "menu1");
    createTexture("img", "menu2");
    

    
    
    TRY_RETHROW;
}
