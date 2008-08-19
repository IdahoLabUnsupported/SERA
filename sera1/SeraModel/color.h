#ifndef COLOR_H
#define COLOR_H

typedef struct _color_t {
  /* cmap is only used in 256 color mode, else is NULL */
  Colormap cmap;
  XColor mycmap[256];
  unsigned char cmap_vals[256*3];
  unsigned long truecolors[256];
  int depth;
  int colortype;
  Display *dpy;                     /* dupl. in image matrix */
  int screen;                       /* dupl. in image matrix */
} color_t;

color_t * get_color_info(void);
void print_supported_visuals(Display *);
void myXStoreColor(Display *, Colormap, XColor *);
void myXStoreColors(Display *, Colormap, XColor *, int);
void myXQueryColor(Display *, Colormap, XColor *);
void myXQueryColors(Display *, Colormap, XColor *, int);
void init_colors(Widget);
void use_new_color_depth(int, unsigned char *,
			 unsigned int);
int get_num_bytes(void);
void XPutImageOneByteData(Display *, Window, GC, XImage *, int, int, int, int, int, int);

void add_guaranteed_colors(Widget, Colormap *cmap);
void load_gamma_colormap(Display *, unsigned char *);
void load_gamma_colormap2(Display *, unsigned char *, float);
void colormap_load_rgb(Display *);

#endif
