#define _ARGS(x) (int n, ...)

#include "ecl.h"
#include "page.h"

#include "functions_list.h"

void
init_all_functions(void) {
  int i;

  for (i = 0; all_functions[i].name != NULL; i++) {
    switch (all_functions[i].type) {
    case cl:
      make_function(all_functions[i].name, (cl_objectfn)all_functions[i].f);
      break;
    case si:
      make_si_function(all_functions[i].name+4, (cl_objectfn)all_functions[i].f);
      break;
    case form: {
      cl_object s = make_ordinary(all_functions[i].name);
      s->symbol.isform = TRUE;
      s->symbol.mflag = FALSE;
    }
    }
  }
}
