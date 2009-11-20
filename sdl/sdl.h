/*
 * sdl.h
 */

/* Prologue */
#ifndef GAUCHE_SDL_H
#define GAUCHE_SDL_H

#include <gauche.h>
#include <gauche/extend.h>

#if defined(EXTSDL_EXPORTS)
#define LIBGAUCHE_EXT_BODY
#endif
#include <gauche/extern.h>      /* redefine SCM_EXTERN  */

SCM_DECL_BEGIN

extern ScmClass *Scm_SDL_SurfaceClass;
#define SCM_SDL_SURFACEPTRP(obj)     SCM_XTYPEP(obj, Scm_SDL_SurfaceClass)
#define SCM_SDL_SURFACEPTR(obj)      SCM_FOREIGN_POINTER_REF(SDL_Surface*, obj)
#define SCM_SDL_SURFACEPTR_BOX(ptr)  Scm_MakeForeignPointer(Scm_SDL_SurfaceClass, ptr)

/* Epilogue */
SCM_DECL_END

#endif  /* GAUCHE_SDL_H */
