/* 
 * tkMain.c 			-- Initialization of Tk
 *
 * This code initializes the Tk library. It corresponds to a part of the 
 * file main.c of the wish interpreter. 
 *
 *           Author: Erick Gallesio [eg@unice.fr]
 *    Creation date: 13-May-1993 10:59
 * Last file update: 10-Feb-1995 22:23
 *
 *
 * Code used here was originally copyrigthed as shown below:
 *      Copyright 1990-1992 Regents of the University of California.
 *
 *
 * Copyright (C) 1993,1994,1995 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
 * 
 *
 * Permission to use, copy, and/or distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that both the above copyright notice and this permission notice appear in
 * all copies and derived works.  Fees for distribution or use of this
 * software or derived works may only be charged with express written
 * permission of the copyright holder.  
 * This software is provided ``as is'' without express or implied warranty.
 *
 * This software is a derivative work of other copyrighted softwares; the
 * copyright notices of these softwares are placed in the file COPYRIGHTS
 *
 * Modified for ECL by Giuseppe Attardi [attardi@di.unipi.it]
 *
 */

#include "ecl.h"
#include "tk.h"


/*
 * Command used to initialize ECL/tk:
 */

static char initCmd[] =
"(tk::tk-init)";

/*
 * Global variables used by the main program:
 */

static Tk_Window w;		/* The main window for the application.  If
				 * NULL then the application no longer
				 * exists. */
Tcl_Interp *ECL_interp = NULL;	/* Interpreter for this application. */
int Tk_initialized = FALSE;	/* TRUE when Tk is fully initialized */
cl_object Tk_root_window;

/*
 * Forward declarations for procedures defined later in this file:
 */

static void DelayedMap _ANSI_ARGS_((ClientData clientData));
static void StructureProc _ANSI_ARGS_((ClientData clientData,
				       XEvent *eventPtr));

extern StdinResume();
extern cl_object tk_package;


/*
 *----------------------------------------------------------------------
 *
 * Tk_main
 *
 *----------------------------------------------------------------------
 */

void
Tk_main(int synchronize, char *name, char *fileName, char *Xdisplay,
	     char *geometry)
{
  Tk_3DBorder border;

  ECL_interp = Tcl_CreateInterp();
  
  /*
   * Parse command-line arguments.
   */

  Tcl_SetVar(ECL_interp, "*geometry*", geometry ? geometry : "",
	     TCL_GLOBAL_ONLY);

  /*
   * Initialize the Tk application and arrange to map the main window
   * after the startup script has been executed, if any.  This way
   * the script can withdraw the window so it isn't ever mapped
   * at all.
   */

  w = Tk_CreateMainWindow(ECL_interp, Xdisplay, name, "ECL/Tk");
  if (w == NULL) {
    fprintf(stderr, "%s\n", ECL_interp->result);
    exit(1);
  }

  Tcl_SetVar(ECL_interp, "*root*", ".", TCL_GLOBAL_ONLY);
  Tk_root_window = _intern("*ROOT*", tk_package);

  Tk_CreateEventHandler(w, StructureNotifyMask, StructureProc,
			(ClientData) NULL);
  Tk_DoWhenIdle(DelayedMap, (ClientData) NULL);
  if (synchronize) {
    XSynchronize(Tk_Display(w), True);
  }
  Tk_GeometryRequest(w, 200, 200);
  border = Tk_Get3DBorder(ECL_interp, w, None, "#cccccc");
  if (border == NULL) {
    Tcl_SetResult(ECL_interp, (char *) NULL, TCL_STATIC);
    Tk_SetWindowBackground(w, WhitePixelOfScreen(Tk_Screen(w)));
  } 
  else {
    Tk_SetBackgroundFromBorder(w, border);
  }
  XSetForeground(Tk_Display(w), DefaultGCOfScreen(Tk_Screen(w)),
		 BlackPixelOfScreen(Tk_Screen(w)));
  
  Tk_initialized = 1;		/* Ok, it's fully initialized		     */

  /*
   * Set up a handler for stdin, for resuming read when input
   * becomes available
   */
  Tk_CreateFileHandler(0, TK_READABLE, (Tk_FileProc *)StdinResume,
		       (ClientData) 0);
  StdinEnableEvents();		/* check for events when idle */

  /*
   * Set the geometry of the main window, if requested.
   */
  if (geometry != NULL) {
    if (TCL_OK != Tcl_VarEval(ECL_interp, "(wm 'geometry *root* '", 
			      geometry, ")", NULL))
      fprintf(stderr, "**** Warning: %s\n", ECL_interp->result);
  }

  /*
   * Execute ECL/Tk's initialization script, followed by the script specified
   * on the command line, if any.
   */
  
  Tcl_GlobalEval(ECL_interp, initCmd);
}


/*
 *----------------------------------------------------------------------
 *
 * StructureProc --
 *
 *	This procedure is invoked whenever a structure-related event
 *	occurs on the main window.  If the window is deleted, the
 *	procedure modifies "w" to record that fact.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Variable "w" may get set to NULL.
 *
 *----------------------------------------------------------------------
 */

static void
StructureProc(ClientData clientData, /* Information about window. */
	      XEvent *eventPtr)	/* Information about event. */
{
  if (eventPtr->type == DestroyNotify)
    w = NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * DelayedMap --
 *
 *	This procedure is invoked by the event dispatcher once the
 *	startup script has been processed.  It waits for all other
 *	pending idle handlers to be processed (so that all the
 *	geometry information will be correct), then maps the
 *	application's main window.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The main window gets mapped.
 *
 *----------------------------------------------------------------------
 */

static void
DelayedMap(ClientData clientData)
{

    while (Tk_DoOneEvent(TK_IDLE_EVENTS) != 0) {
	/* Empty loop body. */
    }
    if (w == NULL) {
	return;
    }
    Tk_MapWindow(w);
}
