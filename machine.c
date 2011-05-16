#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <math.h>
#include <png.h>

#include <GL/gl.h>
#include <SDL/SDL.h>

#define min(x, y) ((x) > (y) ? (y) : (x))
#define max(x, y) ((x) > (y) ? (x) : (y))

float mutiply_and_sum_vectors(float vector1[3], float vector2[3]) {
    float sum = 0;
    unsigned i;
    for (i = 0; i < 3; i++) {
	sum += vector1[i] * vector2[i];
    }
    return sum;
}

void multiply_vector_with_matrix(float vector[3], float matrix[9]) {
    unsigned i;
    float tmp[3];
    for (i = 0; i < 3; i++) {
	tmp[i] = mutiply_and_sum_vectors(vector, matrix + (3 * i));
    }
    memcpy(vector, tmp, 3 * sizeof(float));
}

float vector_length(float a[3]) {
    return sqrt(a[0] * a[0] + a[1] * a[1] + a[2] * a[2]);
}

void rotate_vector(float vector[3], float axis[3], float angle) {
    float len = vector_length(axis);
    if (len > 0.0) {
	float c = cos(-angle);
	float s = sin(-angle);
	float x = axis[0] / len;
	float y = axis[1] / len;
	float z = axis[2] / len;
	float matrix[9] = { x*x+(1-x*x)*c, x*y*(1-c)-z*s, x*z*(1-c)+y*s, 
			     x*y*(1-c)+z*s, y*y+(1-y*y)*c, y*z*(1-c)-x*s,
			     x*z*(1-c)-y*s, y*z*(1-c)+x*s, z*z+(1-z*z)*c };
	multiply_vector_with_matrix(vector, matrix);
    }
}

float dot_product(float a[3], float b[3]) {
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
}

float get_angle(float a[3], float b[3]) {
    float x = dot_product(a, b) / (vector_length(a) * vector_length (b));
    return acos(min(1.0, max(-1.0, x)));
}

#define X 0
#define Y 1
#define Z 2

#define X_AXIS 0
#define Y_AXIS 3
#define Z_AXIS 6

float x_axis[] = { 1.0, 0.0, 0.0 };
float y_axis[] = { 0.0, 1.0, 0.0 };
float z_axis[] = { 0.0, 0.0, 1.0 };

#define HALF_PI 1.5707963267948966

void rotate_space(float vector[3], float space[9]) {
    float new_x_axis[] = { 1.0, 0.0, 0.0 };
    float y_deviance = get_angle(space + Y_AXIS, y_axis);
    float z_deviance = -atan2(space[Y_AXIS + X], space[Y_AXIS + Z]);

    float c = cos(y_deviance);
    float s = sin(y_deviance);
    float r_x[9] = { 1, 0, 0, 0, c,-s, 0, s, c };

    c = cos(z_deviance);
    s = sin(z_deviance);
    float r_y[9] = { c, 0,-s, 0, 1, 0, s, 0, c };

    multiply_vector_with_matrix(vector, r_x);
    multiply_vector_with_matrix(vector, r_y);

    multiply_vector_with_matrix(new_x_axis, r_x);
    multiply_vector_with_matrix(new_x_axis, r_y);

    float tmp[3];
    memcpy(tmp, space + Y_AXIS, 3 * sizeof(float));
    rotate_vector(vector,
		  tmp,
		  get_angle(new_x_axis, space + X_AXIS)
  		  *
		  (get_angle(new_x_axis, space + Z_AXIS) < HALF_PI ? -1 : +1));
}

unsigned set_opengl_texture(char *buf, int width, int height, int channels) {
    unsigned texture = 0;
    GLenum format = (channels == 3 ? GL_RGB : GL_RGBA);
    glGenTextures(1, &texture);
    glBindTexture(GL_TEXTURE_2D, texture);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexImage2D(GL_TEXTURE_2D, 0, channels, width, height,
		 0, format, GL_UNSIGNED_BYTE, buf);
    glBindTexture(GL_TEXTURE_2D, 0);
    return texture;
}

#define PNG_BYTES_TO_CHECK 4
int check_if_png(FILE *fp) {
    char buf[PNG_BYTES_TO_CHECK];
    return (fread(buf, 1, PNG_BYTES_TO_CHECK, fp) == PNG_BYTES_TO_CHECK)
	&& !png_sig_cmp(buf, (png_size_t) 0, PNG_BYTES_TO_CHECK);
}

static unsigned char *png_data = NULL;
static int png_channels;
static int png_w;
static int png_h;

void get_raw_image(png_structp png_ptr, png_infop info_ptr) {
    unsigned bytes_per_row = png_get_rowbytes(png_ptr, info_ptr);
    unsigned height = png_get_image_height(png_ptr, info_ptr);        
    png_bytepp row_data = png_get_rows(png_ptr, info_ptr);
    png_data = malloc(bytes_per_row * height);
    unsigned i;
    for (i = 0; i < height; i++) {
	memcpy(png_data + i * bytes_per_row, row_data[i], bytes_per_row);
    }
}

unsigned char *load_png(const char *name) {
    png_infop info_ptr;
    png_infop end_info;
    png_structp png_ptr = NULL;    
    FILE *png_fp = fopen(name, "rb");

    if (!png_fp || !check_if_png(png_fp)) {
	goto png_end;
    }

    png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
    if (!png_ptr) {
	goto png_end;
    }
    
    if (!(info_ptr = png_create_info_struct(png_ptr))) {
        png_destroy_read_struct(&png_ptr, NULL, NULL);
	goto png_end;
    }
    
    if (!(end_info = png_create_info_struct(png_ptr))) {
        png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
	goto png_end;
    }

    png_init_io(png_ptr, png_fp);
    png_set_sig_bytes(png_ptr, PNG_BYTES_TO_CHECK);
    png_read_png(png_ptr, info_ptr, 0, NULL);

    get_raw_image(png_ptr, info_ptr);
    png_w = png_get_image_width(png_ptr, info_ptr);
    png_h = png_get_image_height(png_ptr, info_ptr);
    png_channels = png_get_channels(png_ptr, info_ptr);

    png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
  png_end:
    if (png_fp) fclose(png_fp);
    return png_data;
}

void free_png(void) {
    if (png_data) {
	free(png_data);
	png_data = NULL;
    }    
}

unsigned load_texture(const char *name) {
    unsigned texture = 0;
    
    if (load_png(name)) {    
	texture = set_opengl_texture(png_data, png_w, png_h, png_channels);
    }
    free_png();

    if (!texture) printf("ERROR: bad %s\n", name);
    return texture;
}

void delete_texture(unsigned texture) {
    glDeleteTextures(1, &texture);
}

enum {
    FILTER_LINEAR,
    FILTER_CUBIC
};

struct image {
    int w, h;
    int filter;
    int channels;
    float *data;
};

struct picture {
    struct image *img;
    struct image *sum;
};

static unsigned img_idx(struct image *img, int x, int y, int c) {
    return (y * img->w + x) * img->channels + c;
}

static float img_value(struct image *img, int x, int y, int c) {
    if (x >= img->w) x = img->w - 1;
    if (y >= img->h) y = img->h - 1;
    return (x < 0 || y < 0) ? 0.0 : img->data[img_idx(img, x, y, c)];
}

static void img_update(struct image *img, int x, int y, int c, float value) {
    img->data[img_idx(img, x, y, c)] = value;
}

static inline float cubic(float x) {
    return x * x * (3 - 2 * x);
}

static inline float cubic_interpolation(float val1, float val2, float amount) {
    return val1 - cubic(amount) * (val1 - val2);
}

static inline float linear_interpolation(float val1, float val2, float amount) {
    return val1 - amount * (val1 - val2);
}

float get_interpolated(struct image *img, float x, float y, int c) {
    float (*fn)(float, float, float);
    switch (img->filter) {
    case FILTER_LINEAR:
	fn = linear_interpolation;
	break;
    case FILTER_CUBIC:
	fn = cubic_interpolation;
	break;
    default:
	printf("ERROR: unknown filter method %i\n", img->filter);
	return 0.0;
    }
    float fx = x * (img->w - 1);
    float fy = y * (img->h - 1);
    int x1 = floor(fx);
    int y1 = floor(fy);
    int x2 = floor(fx + 1.0);
    int y2 = floor(fy + 1.0);
    float qx = fx - x1;
    float qy = fy - y1;
    float p11 = img_value(img, x1, y1, c);
    float p21 = img_value(img, x2, y1, c);
    float p12 = img_value(img, x1, y2, c);
    float p22 = img_value(img, x2, y2, c);
    float dx1 = fn(p11, p21, qx);
    float dx2 = fn(p12, p22, qx);
    return fn(dx1, dx2, qy);
}

float get_integrated(void *ptr, float x, float y, int c, float s) {
    struct picture *pic = (struct picture *) ptr;
    float integrated = 0.0;
    float half = 0.5 * s;
    float x1 = max(x - half, 0.0);
    float y1 = max(y - half, 0.0);
    float x2 = min(x + half, 1.0);
    float y2 = min(y + half, 1.0);
    float px = (x2 - x1) * pic->img->w;
    float py = (y2 - y1) * pic->img->h;
    float exact = get_interpolated(pic->img, x, y, c);
    if (px > 1.0 || py > 1.0) {
	float p11 = get_interpolated(pic->sum, x1, y1, c);
	float p21 = get_interpolated(pic->sum, x2, y1, c);
	float p12 = get_interpolated(pic->sum, x1, y2, c);
	float p22 = get_interpolated(pic->sum, x2, y2, c);
	integrated = ((p22 + p11) - (p21 + p12)) / (px * py);
    }
    float amount = min(1.0, max(0.0, max(px, py) - 1.0));
    return cubic_interpolation(exact, integrated, amount);
}

static void make_sum_table(struct image *img, struct image *sum) {
    int x, y, c;
    for (y = 0; y < sum->h; y++) {
	for (x = 0; x < sum->w; x++) {
	    for (c = 0; c < sum->channels; c++) {
		if (x == 0 || y == 0) {
		    img_update(sum, x, y, c, 0.0);		    
		}
		else {
		    float p11 = img_value(sum, x - 1, y - 1, c);
		    float p12 = img_value(sum, x - 1, y,     c);
		    float p21 = img_value(sum, x,     y - 1, c);
		    float p22 = img_value(img, x - 1, y - 1, c);
		    img_update(sum, x, y, c, p22 + p12 + p21 - p11);
		}
	    }
	}
    }
}

static struct image *new_image(int w, int h, int c) {
    struct image *img = malloc(sizeof(struct image));
    img->w = w;
    img->h = h;
    img->channels = c;
    img->filter = FILTER_CUBIC;
    img->data = malloc(w * h * c * sizeof(float));
    return img;
}

static void delete_image(struct image *img) {
    free(img->data);
    free(img);
}

void *load_picture(const char *name) {
    struct picture *pic;
    if (load_png(name)) {
	int i;
	pic = malloc(sizeof(struct picture));
	pic->img = new_image(png_w, png_h, png_channels);
        for (i = 0; i < png_w * png_h * png_channels; i++) {
            pic->img->data[i] = png_data[i] / 255.0;
        }
	pic->sum = new_image(png_w + 1, png_h + 1, png_channels);
	pic->sum->filter = FILTER_LINEAR;
	make_sum_table(pic->img, pic->sum);
    }
    free_png();

    if (!pic) printf("ERROR: bad %s\n", name);
    return (void *) pic;
}

void delete_picture(void *ptr) {
    struct picture *pic = (struct picture *) ptr;
    delete_image(pic->img);
    delete_image(pic->sum);
    free(pic);
}

unsigned screen_w = 640;
unsigned screen_h = 480;

float get_w(void) {
    return screen_w;
}

float get_h(void) {
    return screen_h;
}

#define BYTES_PER_PIXEL 3

static unsigned screenBufferWidth(void) {
    return screen_w * BYTES_PER_PIXEL;
}

static unsigned screenBufferSize(void) {
    return screen_h * screenBufferWidth();
}

static unsigned bufferY(unsigned offset) {
    return offset / screenBufferWidth();
}

static unsigned bufferX(unsigned offset) {
    return (offset % screenBufferWidth()) / BYTES_PER_PIXEL;
}

static unsigned bufferOffset(unsigned x, unsigned y) {
    return (y * screenBufferWidth() + x * BYTES_PER_PIXEL);
}

static void copy_screen(unsigned char *buf) {
    int i, j = 0;
    unsigned char tmp[screenBufferWidth()];
    glReadPixels(0, 0, screen_w, screen_h, GL_RGB, GL_UNSIGNED_BYTE, buf);
    for (i = (screen_h - 1); i >= screen_h / 2; i--) {
	memcpy(tmp, buf + j, screenBufferWidth());
	memcpy(buf + j, buf + (i * screenBufferWidth()), screenBufferWidth());
	memcpy(buf + (i * screenBufferWidth()), tmp, screenBufferWidth());
	j += screenBufferWidth();
    }
}

void save_screen_buffer(const char *file_name, unsigned char *buf) {
    int fd = open(file_name,  O_CREAT | O_WRONLY, 0644);
    if (fd != -1) {
	char header[256];
	unsigned char ptr[screenBufferSize()];
	unsigned len = sprintf(header, "P6\n%u %u\n255\n", screen_w, screen_h);

	if (!buf) {
	    copy_screen(ptr);
	    buf = ptr;
	}

	write(fd, header, len);
	write(fd, buf, screenBufferSize());
	close(fd);
    }
    else {
	printf("ERROR: can not write %s\n", file_name);
    }
}

void save_screen(const char *file_name) {
    save_screen_buffer(file_name, NULL);
}

SDL_Surface *screen = NULL;

unsigned flags = SDL_OPENGL | SDL_RESIZABLE | SDL_HWSURFACE;

static void init_video_mode(void) {
    screen = SDL_SetVideoMode(screen_w, screen_h, 32, flags);    
    glViewport(0, 0, screen_w, screen_h);
}

static void resize_window(unsigned w, unsigned h) {
    screen_w = w;
    screen_h = h;
    SDL_FreeSurface(screen);
    init_video_mode();
}

void swap_buffers(void) {
    SDL_GL_SwapBuffers();
}

void exit_graphics(void) {
    if (screen) {
	SDL_FreeSurface(screen);
	screen = NULL;
	SDL_Quit();
    }
}

void init_graphics(void) {
    if (!screen) {
	SDL_Init(SDL_INIT_VIDEO);
	SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
	init_video_mode();
	SDL_WM_SetCaption("sandbox", NULL);
	SDL_ShowCursor(SDL_DISABLE);
    }
}
 
unsigned some_mouse_button = 0;

void event_loop(void (*draw_scene)(void),		
		int (*key_change)(int, int),
		void (*mouse_move)(int, int, int, int, int)) {
    int done = 0;
    SDL_Event event;
    some_mouse_button = 0;
    while(!done && SDL_WaitEvent(&event)) {
	do {
	    switch(event.type) {	    
	    case SDL_VIDEORESIZE:
		resize_window(event.resize.w, event.resize.h);
		break;
	    case SDL_KEYDOWN:
	    case SDL_KEYUP:
		if (key_change(event.key.state == SDL_PRESSED, 
			       event.key.keysym.sym)) {
		    done = 1;
		}
		break;
	    case SDL_MOUSEBUTTONDOWN:
		some_mouse_button = 1;
		break;
	    case SDL_MOUSEBUTTONUP:
		some_mouse_button = 0;
		break;
	    case SDL_MOUSEMOTION:
		mouse_move(event.motion.x, 
			   event.motion.y,
			   event.motion.xrel,
			   event.motion.yrel, 
			   some_mouse_button);
		break;	
	    case SDL_QUIT:
		done = 1;
		break;            
	    default:
		break;
	    } 
	}
	while (SDL_PeepEvents(&event, 1, SDL_GETEVENT, SDL_ALLEVENTS));
	draw_scene();
    }
}

int crandom(unsigned modulo) {
#ifdef unix
    return random() % modulo;
#else
    return rand() % modulo;
#endif
}

void cseed(unsigned seed) {
#ifdef unix
    srandom(seed);
#else
    srand(seed);
#endif
}

void emit_char(int num) {
    char buf[] = { (char) num };
    write(1, buf, 1);
}

/* classical perlin noise */

#define TABSIZE           256
#define TABMASK           (TABSIZE - 1)
#define PERM(x)           perm[(x) & TABMASK]
#define INDEX(ix, iy, iz) PERM((ix) + PERM((iy) + PERM(iz)))

static unsigned char perm[TABSIZE] = {
225, 155, 210, 108, 175, 199, 221, 144, 203, 116, 70,  213, 69,  158, 33,  252,
5,   82,  173, 133, 222, 139, 174, 27,  9,   71,  90,  246, 75,  130, 91,  191,
169, 138, 2,   151, 194, 235, 81,  7,   25,  113, 228, 159, 205, 253, 134, 142,
248, 65,  224, 217, 22,  121, 229, 63,  89,  103, 96,  104, 156, 17,  201, 129,
36,  8,   165, 110, 237, 117, 231, 56,  132, 211, 152, 20,  181, 111, 239, 218,
170, 163, 51,  172, 157, 47,  80,  212, 176, 250, 87,  49,  99,  242, 136, 189,
162, 115, 44,  43,  124, 94,  150, 16,  141, 247, 32,  10,  198, 223, 255, 72,
53,  131, 84,  57,  220, 197, 58,  50,  208, 11,  241, 28,  3,   192, 62,  202,
18,  215, 153, 24,  76,  41,  15,  179, 39,  46,  55,  6,   128, 167, 23,  188,
106, 34,  187, 140, 164, 73,  112, 182, 244, 195, 227, 13,  35,  77,  196, 185,
26,  200, 226, 119, 31,  123, 168, 125, 249, 68,  183, 230, 177, 135, 160, 180,
12,  1,   243, 148, 102, 166, 38,  238, 251, 37,  240, 126, 64,  74,  161, 40,
184, 149, 171, 178, 101, 66,  29,  59,  146, 61,  254, 107, 42,  86,  154, 4,
236, 232, 120, 21,  233, 209, 45,  98,  193, 114, 78,  19,  206, 14,  118, 127,
48,  79,  147, 85,  30,  207, 219, 54,  88,  234, 190, 122, 95,  67,  143, 109,
137, 214, 145, 93,  92,  100, 245, 0,   216, 186, 60,  83,  105, 97,  204, 52
};

#define GRANULARITY 1000

float *gradientTab;
float gradientBank[3 * TABSIZE * GRANULARITY];

#define RANDMASK 0x7fffffff
#define RANDNBR ((random() & RANDMASK) / (float) RANDMASK)

void rotate_gradients(float angle) {
    int x = GRANULARITY * angle / (2.0 * M_PI);
    gradientTab = gradientBank + 3 * TABSIZE * (x % GRANULARITY);
}

void gradientTabInitTab(int seed, float *table) {
    float z, r, theta;
    int i;
    srandom(seed);
    for(i = 0; i < TABSIZE; i++) {
	z = 1.0 - 2.0 * RANDNBR;
	r = sqrtf(1 - z * z);
	theta = (float) (2.0 * M_PI * RANDNBR);
	*table++ = r * cosf(theta);
	*table++ = r * sinf(theta);
	*table++ = z;
    }
}

void gradientTabInit(int seed) {
    int i, j;
    float axisTab[3 * TABSIZE];
    gradientTab = gradientBank;
    gradientTabInitTab(seed + 1, axisTab);
    for (i = 0; i < GRANULARITY; i++) {
	int offset = 3 * i * TABSIZE;
	gradientTabInitTab(seed, gradientBank + offset);
	for (j = 0; j < 3 * TABSIZE; j += 3) {
	    rotate_vector(gradientBank + offset + j, axisTab + j,
			  (2.0 * M_PI * i) / (float) GRANULARITY);
	}    
    }
}

float glattice(int ix, int iy, int iz, float fx, float fy, float fz) {
    float *g = &gradientTab[INDEX(ix,iy,iz) * 3];
    return g[0] * fx + g[1] * fy + g[2] * fz;
}

#define LERP(t, x0, x1) ((x0) + (t) * ((x1) - (x0)))
#define FLOOR(x) ((int)(x) - ((x) < 0 && (x) != (int)(x)))
#define SMOOTHSTEP(x) ((x) * (x) * (3 - 2 * (x)))

float noise3D(float x, float y, float z) {
    int ix, iy, iz;
    float wx, wy, wz;
    float fx0, fx1, fy0, fy1, fz0, fz1;
    float vx0, vx1, vy0, vy1, vz0, vz1;

    static int initialized = 0;
    if (!initialized) {
	gradientTabInit(98761);
	initialized = 1;
    }

    ix = FLOOR(x);
    fx0 = x - ix;
    fx1 = fx0 - 1;
    wx = SMOOTHSTEP(fx0);
    iy = FLOOR(y);
    fy0 = y - iy;
    fy1 = fy0 - 1;
    wy = SMOOTHSTEP(fy0);
    iz = FLOOR(z);
    fz0 = z - iz;
    fz1 = fz0 - 1;
    wz = SMOOTHSTEP(fz0);

    vx0 = glattice(ix,iy,iz,fx0,fy0,fz0);
    vx1 = glattice(ix+1,iy,iz,fx1,fy0,fz0);
    vy0 = LERP(wx, vx0, vx1);
    vx0 = glattice(ix,iy+1,iz,fx0,fy1,fz0);
    vx1 = glattice(ix+1,iy+1,iz,fx1,fy1,fz0);
    vy1 = LERP(wx, vx0, vx1);
    vz0 = LERP(wy, vy0, vy1);
    vx0 = glattice(ix,iy,iz+1,fx0,fy0,fz1);
    vx1 = glattice(ix+1,iy,iz+1,fx1,fy0,fz1);
    vy0 = LERP(wx, vx0, vx1);
    vx0 = glattice(ix,iy+1,iz+1,fx0,fy1,fz1);
    vx1 = glattice(ix+1,iy+1,iz+1,fx1,fy1,fz1);
    vy1 = LERP(wx, vx0, vx1);
    vz1 = LERP(wy, vy0, vy1);
    return ((LERP(wz, vz0, vz1)) + 0.7) * 0.71428573;
}
