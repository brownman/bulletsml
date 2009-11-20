#define	GAUCHE_API_0_9
#include <gauche.h>
#include <stdio.h>

#include <SDL/SDL.h>

//=============================================================================

/* Error handling */
void error_exit(ScmObj c) {
	ScmObj m = Scm_ConditionMessage(c);
	if (SCM_FALSEP(m)) {
		Scm_Printf(SCM_CURERR, "gosh: Thrown unknown condition: %S\n", c);
	} else {
		Scm_Printf(SCM_CURERR, "gosh: %S: %A\n", Scm_ConditionTypeName(c), m);
	}
	Scm_Exit(1);
}


int dofile(const char* fn) {
	ScmLoadPacket lpak;
	if (Scm_Load(fn, 0, &lpak) >= 0) {
		return TRUE;
	} else {
		error_exit(lpak.exception);
		return FALSE;
	}
}

ScmObj call_proc(ScmSymbol* proc_sym, ScmObj args) {
	ScmModule* module = Scm_UserModule();
	ScmObj proc = Scm_SymbolValue(module, proc_sym);
	if (SCM_PROCEDUREP(proc)) {
		ScmObj r = Scm_ApplyRec(proc, args);
		return r;
	} else {
		return SCM_FALSE;
	}
}

#define	LIBPATH	"../Gauche-0.8.14/winnt/"
//#define	LIBPATH	"lib"

int main(int argc, char* argv[]) {
	GC_INIT();
	Scm_Init(GAUCHE_SIGNATURE);
	Scm_AddLoadPath(".", FALSE);
	Scm_AddLoadPath(LIBPATH "/share/gauche/0.8.14/lib", FALSE);
	Scm_AddLoadPath(LIBPATH "/lib/gauche/site/0.8.14/i686-pc-winnt", FALSE);

	dofile("gauche-init.scm");

	const char ScriptFn[] = "./entry.scm";
	dofile(ScriptFn);
	SCM_UNWIND_PROTECT {
		ScmObj args = SCM_LIST1(Scm_Cons(Scm_MakeString(ScriptFn, -1, -1, SCM_STRING_COPYING), SCM_NIL));
		call_proc(SCM_SYMBOL(SCM_INTERN("main")), args);
	} SCM_WHEN_ERROR {
		printf("error\n");
	} SCM_END_PROTECT;

	Scm_Exit(0);

	return 0;
}
