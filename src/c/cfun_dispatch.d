/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    cfun_dispatch.c -- Trampolines for functions
*/

static cl_object dispatch0 (cl_nargs nargs, cl_object x0) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 0) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0);
}

static cl_object dispatch1 (cl_nargs nargs, cl_object x0, cl_object x1) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 1) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1);
}

static cl_object dispatch2 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 2) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2);
}

static cl_object dispatch3 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 3) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3);
}

static cl_object dispatch4 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 4) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4);
}

static cl_object dispatch5 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 5) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5);
}

static cl_object dispatch6 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 6) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6);
}

static cl_object dispatch7 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 7) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7);
}

static cl_object dispatch8 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 8) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

static cl_object dispatch9 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 9) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9);
}

static cl_object dispatch10 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 10) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10);
}

static cl_object dispatch11 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 11) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11);
}

static cl_object dispatch12 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 12) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12);
}

static cl_object dispatch13 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 13) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13);
}

static cl_object dispatch14 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 14) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14);
}

static cl_object dispatch15 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 15) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15);
}

static cl_object dispatch16 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 16) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16);
}

static cl_object dispatch17 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 17) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17);
}

static cl_object dispatch18 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 18) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18);
}

static cl_object dispatch19 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 19) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19);
}

static cl_object dispatch20 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 20) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20);
}

static cl_object dispatch21 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 21) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21);
}

static cl_object dispatch22 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 22) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22);
}

static cl_object dispatch23 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 23) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23);
}

static cl_object dispatch24 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 24) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24);
}

static cl_object dispatch25 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 25) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25);
}

static cl_object dispatch26 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 26) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26);
}

static cl_object dispatch27 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 27) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27);
}

static cl_object dispatch28 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 28) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28);
}

static cl_object dispatch29 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 29) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29);
}

static cl_object dispatch30 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 30) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30);
}

static cl_object dispatch31 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 31) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31);
}

static cl_object dispatch32 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 32) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32);
}

static cl_object dispatch33 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 33) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33);
}

static cl_object dispatch34 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 34) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34);
}

static cl_object dispatch35 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 35) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35);
}

static cl_object dispatch36 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 36) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36);
}

static cl_object dispatch37 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 37) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37);
}

static cl_object dispatch38 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 38) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38);
}

static cl_object dispatch39 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 39) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39);
}

static cl_object dispatch40 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 40) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40);
}

static cl_object dispatch41 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 41) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41);
}

static cl_object dispatch42 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 42) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42);
}

static cl_object dispatch43 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 43) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43);
}

static cl_object dispatch44 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 44) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44);
}

static cl_object dispatch45 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 45) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45);
}

static cl_object dispatch46 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 46) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46);
}

static cl_object dispatch47 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 47) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47);
}

static cl_object dispatch48 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 48) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48);
}

static cl_object dispatch49 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 49) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49);
}

static cl_object dispatch50 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 50) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50);
}

static cl_object dispatch51 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 51) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51);
}

static cl_object dispatch52 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 52) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52);
}

static cl_object dispatch53 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 53) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53);
}

static cl_object dispatch54 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 54) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54);
}

static cl_object dispatch55 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54, cl_object x55) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 55) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55);
}

static cl_object dispatch56 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54, cl_object x55, cl_object x56) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 56) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56);
}

static cl_object dispatch57 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54, cl_object x55, cl_object x56, cl_object x57) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 57) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57);
}

static cl_object dispatch58 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54, cl_object x55, cl_object x56, cl_object x57, cl_object x58) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 58) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58);
}

static cl_object dispatch59 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54, cl_object x55, cl_object x56, cl_object x57, cl_object x58, cl_object x59) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 59) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59);
}

static cl_object dispatch60 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54, cl_object x55, cl_object x56, cl_object x57, cl_object x58, cl_object x59, cl_object x60) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 60) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60);
}

static cl_object dispatch61 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54, cl_object x55, cl_object x56, cl_object x57, cl_object x58, cl_object x59, cl_object x60, cl_object x61) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 61) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61);
}

static cl_object dispatch62 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54, cl_object x55, cl_object x56, cl_object x57, cl_object x58, cl_object x59, cl_object x60, cl_object x61, cl_object x62) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 62) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62);
}

static cl_object dispatch63 (cl_nargs nargs, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54, cl_object x55, cl_object x56, cl_object x57, cl_object x58, cl_object x59, cl_object x60, cl_object x61, cl_object x62, cl_object x63) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (narg != 63) FEwrong_num_arguments(fun);
  return fun->cfunfixed.orig(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63);
}

static dispatch_table[64] = {dispatch0, dispatch1, dispatch2, dispatch3, dispatch4, dispatch5, dispatch6, dispatch7, dispatch8, dispatch9, dispatch10, dispatch11, dispatch12, dispatch13, dispatch14, dispatch15, dispatch16, dispatch17, dispatch18, dispatch19, dispatch20, dispatch21, dispatch22, dispatch23, dispatch24, dispatch25, dispatch26, dispatch27, dispatch28, dispatch29, dispatch30, dispatch31, dispatch32, dispatch33, dispatch34, dispatch35, dispatch36, dispatch37, dispatch38, dispatch39, dispatch40, dispatch41, dispatch42, dispatch43, dispatch44, dispatch45, dispatch46, dispatch47, dispatch48, dispatch49, dispatch50, dispatch51, dispatch52, dispatch53, dispatch54, dispatch55, dispatch56, dispatch57, dispatch58, dispatch59, dispatch60, dispatch61, dispatch62, dispatch63};
