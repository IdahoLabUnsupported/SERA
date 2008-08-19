/* ==========================================================================
 *
 * Definitions for the registration colormap management routines.
 *
 * ==========================================================================
 */

#include <Xm/Xm.h>

#define REG_MAX_COLORS          256
#define REG_NUM_RESERVED_COLORS   6
#define REG_NUM_GRAYS           128
#define REG_MAX_GRAY            (REG_MAX_COLORS - REG_NUM_RESERVED_COLORS - 1)
#define REG_MIN_GRAY            (REG_MAX_GRAY - REG_NUM_GRAYS + 1)

typedef struct _CsInfoType
{
  Colormap        cmap;                   /* The colormap */
  XColor          cmap_colors[256];       /* Pseudocolor map values */
  unsigned char   cmap_values[256*3];     /* Truecolor color values */
  unsigned long   cmap_pixels[256];       /* Truecolor pixel values */
  int             maxHWcolormaps;
  int             depth;
  int             colortype;
  int             background;
  int             saturation;
  int             offset;
  int             gamma;                                        
  XVisualInfo     visual_info;                                       

  Display         *display;
  int             screen;
} CsInfoType;

/* Prototypes =========================================================== */

void CsStoreColor (CsInfoType *, Colormap, XColor *);
void CsStoreColors (CsInfoType *, Colormap, XColor *, int);

void CsQueryColor (CsInfoType *, Colormap, XColor *);
void CsQueryColors (CsInfoType *, Colormap, XColor *, int);

void CsInitColormap (CsInfoType *);

void CsLoadGamma (CsInfoType *, unsigned char *, float);
void CsLoadRGB (CsInfoType *);

void CsPutImage (CsInfoType *, Window, GC, XImage *, 
   int, int, int, int, int, int);


