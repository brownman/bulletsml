/*
 * sdl.c
 */

#include "sdl.h"
#include <SDL/SDL.h>

/*
 * Module initialization function.
 */
extern void Scm_Init_sdllib(ScmModule*);

static void SDL_SurfacePtr_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx) {
	SDL_Surface *q = SCM_SDL_SURFACEPTR(obj);
	Scm_Printf(out, "#<SDL_Surface*: %p>", q);
}

static void SDL_SurfacePtr_cleanup(ScmObj obj) {
	SDL_Surface *q = SCM_SDL_SURFACEPTR(obj);
//	delete q;
}

ScmClass *Scm_SDL_SurfaceClass;

SCM_EXTENSION_ENTRY void Scm_Init_gauche_sdl(void)
{
	ScmModule *mod;

#define	DEFINE_INTVAL(v)	SCM_DEFINE(mod, #v, Scm_MakeInteger(v))

	/* Register this DSO to Gauche */
	SCM_INIT_EXTENSION(sdl);

	/* Create the module if it doesn't exist yet. */
	mod = SCM_MODULE(SCM_FIND_MODULE("sdl", TRUE));

	Scm_SDL_SurfaceClass = Scm_MakeForeignPointerClass(mod, "<SDL_SurfacePtr>", SDL_SurfacePtr_print, SDL_SurfacePtr_cleanup, SCM_FOREIGN_POINTER_KEEP_IDENTITY|SCM_FOREIGN_POINTER_MAP_NULL);

	DEFINE_INTVAL(SDL_INIT_VIDEO);

	// flags for SDL_SetVideoMode
	DEFINE_INTVAL(SDL_HWSURFACE);
	DEFINE_INTVAL(SDL_DOUBLEBUF);
	DEFINE_INTVAL(SDL_FULLSCREEN);
	DEFINE_INTVAL(SDL_RESIZABLE);
	DEFINE_INTVAL(SDL_SRCCOLORKEY);

	// event
	DEFINE_INTVAL(SDL_KEYDOWN);
	DEFINE_INTVAL(SDL_KEYUP);
	DEFINE_INTVAL(SDL_QUIT);
	DEFINE_INTVAL(SDL_VIDEORESIZE);

	// keysym
	DEFINE_INTVAL(SDLK_ESCAPE);
	DEFINE_INTVAL(SDLK_SPACE);
	DEFINE_INTVAL(SDLK_UP);
	DEFINE_INTVAL(SDLK_DOWN);
	DEFINE_INTVAL(SDLK_RIGHT);
	DEFINE_INTVAL(SDLK_LEFT);
	DEFINE_INTVAL(SDLK_F5);
	DEFINE_INTVAL(SDLK_LSHIFT);
	DEFINE_INTVAL(SDLK_LCTRL);
	DEFINE_INTVAL(SDLK_LAST);

	/* Register stub-generated procedures */
	Scm_Init_sdllib(mod);
}
