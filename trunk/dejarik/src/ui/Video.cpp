#include <iostream>
#include "Video.h"
#include "Window.h"
#include "Glbasic.h"


using namespace std;


/////////////////////////////////////////////////////////////////////////
// For BMP loading
/////////////////////////////////////////////////////////////////////////
#ifdef LINUX_BUILD
    #include <X11/Xmd.h> // for INT16, INT32
    #pragma pack(1)
    struct BITMAPFILEHEADER 
    {
    
        INT16 bfType;
        INT32 bfSize;
        INT16 bfReserved1;
        INT16 bfReserved2;
        INT32 bfOffBits;
    };
    
    #pragma pack(1)
    struct BITMAPINFOHEADER 
    { 
      INT32 biSize; 
      INT32 biWidth; 
      INT32 biHeight; 
      INT16 biPlanes; 
      INT16 biBitCount; 
      INT32 biCompression; 
      INT32 biSizeImage; 
      INT32 biXPelsPerMeter; 
      INT32 biYPelsPerMeter; 
      INT32 biClrUsed; 
      INT32 biClrImportant; 
    }; 
    
    #pragma pack(1)
    struct RGBTRIPLE 
    {
      INT8 rgbtRed;
      INT8 rgbtGreen;
      INT8 rgbtBlue;
    };
#endif
/////////////////////////////////////////////////////////////////////////

#define PRECISION 16    
#define ONE (1 << PRECISION)
#define ZERO 0


inline GLfixed FixedFromInt(int value) {return value << PRECISION;}
inline GLfixed FixedFromFloat(float value) {return static_cast<GLfixed>(value * static_cast<float>(ONE));}
inline GLfixed MultiplyFixed(GLfixed op1, GLfixed op2) {return (op1 * op2) >> PRECISION;};

    
#define WINDOW_WIDTH  240
#define WINDOW_HEIGHT 320

// virtual scene size
#define SCREEN_WIDTH  800 // must be big enough
#define SCREEN_HEIGHT 1000 // must be big enough
    
   


void Video::startup(const std::vector<std::string>& pieceNames)
{
    TRY_BEGINS;
    
    createEGLWindow(WINDOW_WIDTH, WINDOW_HEIGHT, "Dejarik");
    
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
    
    // Load all the textures 
    createImages(pieceNames);
    
    TRY_RETHROW;
}



void Video::drawPolygon(GLshort* vertexArray, unsigned vertNum, const RGBA_Color& color)
{
    glLoadIdentity();
    
    glColor4x(
        FixedFromFloat(color.r), 
        FixedFromFloat(color.b), 
        FixedFromFloat(color.g), 
        FixedFromFloat(color.a));

    glEnableClientState(GL_VERTEX_ARRAY);

    glVertexPointer(3, GL_SHORT, 0, vertexArray);
    glDrawArrays(GL_TRIANGLE_FAN, 0, vertNum);

    glDisableClientState(GL_VERTEX_ARRAY);
}





void Video::freeSurface(Surface* surface)
{
    delete[] surface->pixels;
    delete surface;
}


Video::Surface* Video::loadBMP(const char* filename)
{
    TRY_BEGINS;
    
#ifndef BM
#define BM 0x4D42
#endif
    
#define COLOR_COMPONENTS 4 // RGBA 

    BITMAPFILEHEADER bmpHeader = {0};
    BITMAPINFOHEADER bmpInfo = {0};
    RGBTRIPLE rgb = {0};

    if(!filename) 
        throw runtime_error("Filename not set");

    // Open the file
    FILE * file;
    if( !(file = fopen(filename, "rb")))
    {
        throw runtime_error(string("Can't open file: ") + string(filename));
    }

    // Read the bmp header and check for a valid file
    if(!fread(&bmpHeader, sizeof(bmpHeader), 1, file)) 
    {
        fclose(file);
        throw runtime_error(string("Can't read BMP header") + string(filename));
    }
    if(bmpHeader.bfType != BM) 
    {
        fclose(file);
        throw runtime_error(string("Not a BMP file") + string(filename));
    }

    // Read the infoheader
    if(!fread(&bmpInfo, sizeof(bmpInfo), 1, file)) 
    {
        fclose(file);
        throw runtime_error(string("Can't read BMP info") + string(filename));
    }

    // Get the informations
    int width = bmpInfo.biWidth;
    int height = bmpInfo.biHeight;
    int bpp = bmpInfo.biBitCount;

    if(bmpInfo.biCompression || bpp != 24) 
    {
        fclose( file );
        throw runtime_error(string("Can't read compressed BMP") + string(filename));
    }

    GLbyte* pixels = new GLbyte[width * height * COLOR_COMPONENTS];
    if(!pixels) 
    {
        fclose(file);
        throw runtime_error(string("Can't allocate memory for surface") + string(filename));
    }

    // Read the pixels
    unsigned base = 0;
    for (int j=height-1; j >= 0 ; j--) 
    {
        for (int i=0; i < width; i++) 
        {
            if(!fread(&rgb, sizeof(rgb), 1, file)) 
            {
                delete[] pixels;
                fclose(file);
                throw runtime_error(string("Can't read BMP pixels") + string(filename));
            }
           
            pixels[base + 3] = (rgb.rgbtRed == 0 && rgb.rgbtGreen == 0 && rgb.rgbtBlue == 0) ? 
                              0 : 255;
            pixels[base + 2] = rgb.rgbtRed;
            pixels[base + 1] = rgb.rgbtGreen;
            pixels[base + 0] = rgb.rgbtBlue;

            base += COLOR_COMPONENTS;
        }
    }

    fclose(file);

    Surface* surface = new Surface();
    surface->pixels = pixels;
    surface->w = width;
    surface->h = height;
    
    return surface;

    TRY_RETHROW;
}



void Video::loadTexture(TexturePtr& texture, const std::string& path)
{
    TRY_BEGINS;
    
    /* Status indicator */
    bool res = false;

    /* Create storage space for the texture */
    Surface* surface = loadBMP(path.c_str()); 

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

    /* Free up any memory we may have used */
    if (surface)
        freeSurface(surface);

    if(!res)
    {
        ostringstream os;
        os << "loadTexture failed: " << path << endl;
        throw runtime_error(os.str());
    }
    
    TRY_RETHROW;
}




void Video::drawShape(const vector<float>& xWin, const vector<float>& yWin, 
        const RGBA_Color& color, float width)
{
    TRY_BEGINS;
    /*
    vector<float> vertices; 
    
    for(unsigned i=0; i< xWin.size(); i++)
    {
        float x = 0;
        float y = 0;
        float z = 0;
        Video::winToGL(xWin[i], yWin[i], x, y, z);
        
        vertices.push_back( x );
        vertices.push_back( y );
        vertices.push_back( 0 );
    }
    
    glColor4x(color.r, color.g, color.b, 0);
    glLineWidthx(width);
    
    
    glVertexPointer(3, GL_FLOAT, 0, &vertices[0]);
    glEnableClientState(GL_VERTEX_ARRAY);

        glDrawArrays(GL_LINE_LOOP, 0, xWin.size());

    glDisableClientState(GL_VERTEX_ARRAY);
 
    glColor4x(1, 1, 1, 0); // reset
    */
    
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
    
    
    if(bindXY == XY_CENTER)
    {
        x -= textures[texName]->w / 2;
        y -= textures[texName]->h / 2;
    }
    else if(bindXY == XY_RIGHT_BOTTOM)
    {
        x -= textures[texName]->w;
    }
    else if(bindXY == XY_LEFT_TOP)
    {
        y -= textures[texName]->h;
    }
    else if(bindXY == XY_RIGHT_TOP)
    {
        x -= textures[texName]->w;
        y -= textures[texName]->h;
    }


    drawTexture(textures[texName], color, x, y, angle);

    TRY_RETHROW;
}


void Video::drawTexture(const TexturePtr& texture, const RGBA_Color& color, 
        GLshort x, GLshort y, float angle)
{                
    GLshort x1 = x;
    GLshort y1 = y;
    
    GLshort x2 = x  + texture->w;
    GLshort y2 = y + texture->h;
    
    glLoadIdentity();
        
  //  glPushMatrix();
    glEnable( GL_TEXTURE_2D );

    glBindTexture(GL_TEXTURE_2D, texture->id);
    
    glColor4x(
        FixedFromFloat(color.r), 
        FixedFromFloat(color.b), 
        FixedFromFloat(color.g), 
        FixedFromFloat(color.a));


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
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

    glDisable(GL_BLEND);
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);

    glDisable( GL_TEXTURE_2D );
 //   glPopMatrix();

}

void Video::createImage(const std::string& name)
{
    TRY_BEGINS;

    TexturePtr texture (new Texture); 
    
    ostringstream fullname;
    fullname << "img/" << name << ".bmp";
    
    loadTexture(texture, fullname.str());  
    
    textures[name] = texture;  

    TRY_RETHROW;
}

void Video::createImages(const std::vector<std::string>& names)
{
    TRY_BEGINS;
      
    createImage("Molator0");
    createImage("Sarvip0");
    createImage("Ghhhk0");
    createImage("Monnok0");
    createImage("Strider0");
    createImage("Ngok0");
    createImage("Klorslug0");
    createImage("Houjix0");
  
    
    createImage("ex");
    /*
    for(unsigned i =0; i<names.size(); ++i)
    {
        createImage(names[i] +"0", IT_MASKED); 
        cout << names[i] +"0" << endl;
    }
    */
    /* 
    for(unsigned i =1; i<8; ++i)
    {
        ostringstream name;
        name << "Molator" << i;
        createImage(name.str(), IT_MASKED);
    }
*/

    createImage("board1");
    createImage("board2");
    createImage("board3");
    createImage("board4");
    
    createImage("menu1");
    createImage("menu2");
    

    
    
    TRY_RETHROW;
}
