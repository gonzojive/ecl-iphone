
#include "pvmecl.h"
init_code(int size, object data_stream)
{VT2 CLSR2
	volatile object VVprotect;
	Cblock.cd_start=(char *)init_code; Cblock.cd_size=size;
	VVprotect=Cblock.cd_data=read_VV(VV,VM1,data_stream);
	MF0(VV[95],L1);
	(void)putprop(VV[95],VV[Vdeb95],VV[96]);
	MF0(VV[2],L2);
	funcall(2,VV[97]->s.s_gfdef,VV[1])        /*  PROCLAIM        */;
	putprop(VV[2],VV[4],VV[3]);
	MF0(VV[98],L3);
	(void)putprop(VV[98],VV[Vdeb98],VV[96]);
	MF0(VV[99],L4);
	(void)putprop(VV[99],VV[Vdeb99],VV[96]);
	MF0(VV[100],L5);
	(void)putprop(VV[100],VV[Vdeb100],VV[96]);
	MF0(VV[101],L6);
	(void)putprop(VV[101],VV[Vdeb101],VV[96]);
	MF0(VV[8],L7);
	funcall(2,VV[97]->s.s_gfdef,VV[7])        /*  PROCLAIM        */;
	putprop(VV[8],VV[9],VV[3]);
	MF0(VV[102],L8);
	(void)putprop(VV[102],VV[Vdeb102],VV[96]);
	MF0(VV[103],L9);
	(void)putprop(VV[103],VV[Vdeb103],VV[96]);
	MF0(VV[13],L10);
	funcall(2,VV[97]->s.s_gfdef,VV[12])       /*  PROCLAIM        */;
	putprop(VV[13],VV[14],VV[3]);
	MF0(VV[104],L11);
	(void)putprop(VV[104],VV[Vdeb104],VV[96]);
	MF0(VV[17],L12);
	funcall(2,VV[97]->s.s_gfdef,VV[16])       /*  PROCLAIM        */;
	putprop(VV[17],VV[18],VV[3]);
	MF0(VV[105],L13);
	(void)putprop(VV[105],VV[Vdeb105],VV[96]);
	MF0(VV[21],L14);
	funcall(2,VV[97]->s.s_gfdef,VV[20])       /*  PROCLAIM        */;
	putprop(VV[21],VV[22],VV[3]);
	MF0(VV[106],L15);
	(void)putprop(VV[106],VV[Vdeb106],VV[96]);
	MF0(VV[107],L16);
	(void)putprop(VV[107],VV[Vdeb107],VV[96]);
	MF0(VV[108],L17);
	(void)putprop(VV[108],VV[Vdeb108],VV[96]);
	MF0(VV[109],L18);
	(void)putprop(VV[109],VV[Vdeb109],VV[96]);
	MF0(VV[110],L19);
	MF0(VV[111],L20);
	(void)putprop(VV[111],VV[Vdeb111],VV[96]);
	MF0(VV[112],L21);
	(void)putprop(VV[112],VV[Vdeb112],VV[96]);
	MF0(VV[113],L21);
	(void)putprop(VV[113],VV[Vdeb113],VV[96]);
	MF0(VV[114],L23);
	MF0(VV[115],L24);
	(void)putprop(VV[115],VV[Vdeb115],VV[96]);
	MF0(VV[116],L25);
	MF0(VV[117],L26);
	(void)putprop(VV[117],VV[Vdeb117],VV[96]);
	MF0(VV[118],L27);
	MF0(VV[119],L28);
	(void)putprop(VV[119],VV[Vdeb119],VV[96]);
	MF0(VV[120],L29);
	MF0(VV[121],L30);
	(void)putprop(VV[121],VV[Vdeb121],VV[96]);
	MF0(VV[122],L31);
	(void)putprop(VV[122],VV[Vdeb122],VV[96]);
	MF0(VV[123],L32);
	MF0(VV[124],L33);
	(void)putprop(VV[124],VV[Vdeb124],VV[96]);
	MF0(VV[125],L34);
	(void)putprop(VV[125],VV[Vdeb125],VV[96]);
	MF0(VV[126],L35);
	(void)putprop(VV[126],VV[Vdeb126],VV[96]);
	MF0(VV[127],L36);
	(void)putprop(VV[127],VV[Vdeb127],VV[96]);
	MF0(VV[34],L37);
	funcall(2,VV[97]->s.s_gfdef,VV[33])       /*  PROCLAIM        */;
	putprop(VV[34],VV[35],VV[3]);
	MF0(VV[128],L38);
	(void)putprop(VV[128],VV[Vdeb128],VV[96]);
	MF0(VV[40],L39);
	funcall(2,VV[97]->s.s_gfdef,VV[39])       /*  PROCLAIM        */;
	putprop(VV[40],VV[41],VV[3]);
	MF0(VV[129],L40);
	(void)putprop(VV[129],VV[Vdeb129],VV[96]);
	MF0(VV[130],L41);
	(void)putprop(VV[130],VV[Vdeb130],VV[96]);
	MF0(VV[45],L42);
	funcall(2,VV[97]->s.s_gfdef,VV[44])       /*  PROCLAIM        */;
	putprop(VV[45],VV[46],VV[3]);
	MF0(VV[131],L43);
	(void)putprop(VV[131],VV[Vdeb131],VV[96]);
	MF0(VV[50],L44);
	funcall(2,VV[97]->s.s_gfdef,VV[49])       /*  PROCLAIM        */;
	putprop(VV[50],VV[51],VV[3]);
	MF0(VV[132],L45);
	(void)putprop(VV[132],VV[Vdeb132],VV[96]);
	MF0(VV[54],L46);
	funcall(2,VV[97]->s.s_gfdef,VV[53])       /*  PROCLAIM        */;
	putprop(VV[54],VV[55],VV[3]);
	MF0(VV[133],L47);
	(void)putprop(VV[133],VV[Vdeb133],VV[96]);
	MF0(VV[58],L48);
	funcall(2,VV[97]->s.s_gfdef,VV[57])       /*  PROCLAIM        */;
	putprop(VV[58],VV[59],VV[3]);
	MF0(VV[134],L49);
	(void)putprop(VV[134],VV[Vdeb134],VV[96]);
	MF0(VV[62],L50);
	funcall(2,VV[97]->s.s_gfdef,VV[61])       /*  PROCLAIM        */;
	putprop(VV[62],VV[63],VV[3]);
	MF0(VV[135],L51);
	(void)putprop(VV[135],VV[Vdeb135],VV[96]);
	MF0(VV[66],L52);
	funcall(2,VV[97]->s.s_gfdef,VV[65])       /*  PROCLAIM        */;
	putprop(VV[66],VV[67],VV[3]);
	MF0(VV[136],L53);
	(void)putprop(VV[136],VV[Vdeb136],VV[96]);
	MF0(VV[70],L54);
	funcall(2,VV[97]->s.s_gfdef,VV[69])       /*  PROCLAIM        */;
	putprop(VV[70],VV[71],VV[3]);
	MF0(VV[137],L55);
	(void)putprop(VV[137],VV[Vdeb137],VV[96]);
	MF0(VV[74],L56);
	funcall(2,VV[97]->s.s_gfdef,VV[73])       /*  PROCLAIM        */;
	putprop(VV[74],VV[75],VV[3]);
	MF0(VV[138],L57);
	(void)putprop(VV[138],VV[Vdeb138],VV[96]);
	MF0(VV[139],L58);
	MF0(VV[140],L59);
	(void)putprop(VV[140],VV[Vdeb140],VV[96]);
	MF0(VV[88],L60);
	funcall(2,VV[97]->s.s_gfdef,VV[87])       /*  PROCLAIM        */;
	putprop(VV[88],VV[89],VV[3]);
	MF0(VV[141],L61);
	(void)putprop(VV[141],VV[Vdeb141],VV[96]);
	MF0(VV[92],L62);
	funcall(2,VV[97]->s.s_gfdef,VV[91])       /*  PROCLAIM        */;
	putprop(VV[92],VV[93],VV[3]);
	MF0(VV[142],L63);
	(void)putprop(VV[142],VV[Vdeb142],VV[96]);
	Cblock.cd_start=(char *)end_init;
	Cblock.cd_size-=(char *)end_init - (char *)init_code;
	insert_contblock((char *)init_code,(char *)end_init - (char *)init_code);
}
static end_init() {}
/*	function definition for PVM-ERROR                             */
static L1(int narg, object V1, object V2)
{ VT3 VLEX3 CLSR3
TTL:
	RETURN(Lerror(3,VV[0],(V1),(V2))          /*  ERROR           */);
}

#include "/project/pvm/pvm3/include/pvm3.h"

/*	function definition for C_PVM_PKINT                           */
static L2(int narg, object V1)
{
	int x;
	x=pvm_pkint(&object_to_int(V1),1,1);
	VALUES(0)=MAKE_FIXNUM(x);
	RETURN(1);
}
/*	function definition for OBUFFER-INT                           */
static L3(int narg, object V1)
{ VT4 VLEX4 CLSR4
TTL:
	{int V2;                                  /*  INFO            */
	V2= pvm_pkint(&fix((V1)),1,1);
	if((0)==(V2)){
	goto L35;}
	L1(2,MAKE_FIXNUM(V2),VV[5])               /*  PVM-ERROR       */;
	}
L35:
	RETURN(0);
}
/*	function definition for PACK-TYPE-TAG                         */
static L4(int narg, object V1)
{ VT5 VLEX5 CLSR5
TTL:
	{int V2;                                  /*  RETURN-CODE     */
	V2= pvm_pkint(&fix((V1)),1,1);
	if((0)==(V2)){
	goto L39;}
	L1(2,MAKE_FIXNUM(V2),VV[6])               /*  PVM-ERROR       */;
	}
L39:
	RETURN(0);
}
/*	function definition for C-OBUFFER-NIL                         */
static L5(int narg)
{ VT6 VLEX6 CLSR6
TTL:
	RETURN(L4(1,MAKE_FIXNUM(2))               /*  PACK-TYPE-TAG   */);
}
/*	function definition for C-OBUFFER-T                           */
static L6(int narg)
{ VT7 VLEX7 CLSR7
TTL:
	RETURN(L4(1,MAKE_FIXNUM(3))               /*  PACK-TYPE-TAG   */);
}
/*	function definition for C_PVM_PKCHAR                          */
static L7(int narg, object V1)
{
	int x;
	x=pvm_pkbyte(&object_to_char(V1),1,1);
	VALUES(0)=MAKE_FIXNUM(x);
	RETURN(1);
}
/*	function definition for C-OBUFFER-CHAR                        */
static L8(int narg, object V1)
{ VT8 VLEX8 CLSR8
TTL:
	L4(1,MAKE_FIXNUM(4))                      /*  PACK-TYPE-TAG   */;
	{int V2;                                  /*  INFO            */
	V2= pvm_pkbyte(&char_code((V1)),1,1);
	if((0)==(V2)){
	goto L44;}
	L1(2,MAKE_FIXNUM(V2),VV[10])              /*  PVM-ERROR       */;
	}
L44:
	RETURN(0);
}
/*	function definition for C-OBUFFER-INT                         */
static L9(int narg, object V1)
{ VT9 VLEX9 CLSR9
TTL:
	L4(1,MAKE_FIXNUM(6))                      /*  PACK-TYPE-TAG   */;
	{int V2;                                  /*  INFO            */
	V2= pvm_pkint(&fix((V1)),1,1);
	if((0)==(V2)){
	goto L49;}
	L1(2,MAKE_FIXNUM(V2),VV[11])              /*  PVM-ERROR       */;
	}
L49:
	RETURN(0);
}
/*	function definition for C_PVM_PKFLOAT                         */
static L10(int narg, object V1)
{
	int x;
	x=pvm_pkfloat(&object_to_float(V1),1,1);
	VALUES(0)=MAKE_FIXNUM(x);
	RETURN(1);
}
/*	function definition for OBUFFER-FLOAT                         */
static L11(int narg, object V1)
{ VT10 VLEX10 CLSR10
TTL:
	{int V2;                                  /*  INFO            */
	V2= pvm_pkfloat(&sf((V1)),1,1);
	if((0)==(V2)){
	goto L53;}
	L1(2,MAKE_FIXNUM(V2),VV[15])              /*  PVM-ERROR       */;
	}
L53:
	RETURN(0);
}
/*	function definition for C_PVM_PKDOUBLE                        */
static L12(int narg, object V1)
{
	int x;
	x=pvm_pkdouble(&object_to_double(V1),1,1);
	VALUES(0)=MAKE_FIXNUM(x);
	RETURN(1);
}
/*	function definition for C-OBUFFER-DOUBLE                      */
static L13(int narg, object V1)
{ VT11 VLEX11 CLSR11
TTL:
	{int V2;                                  /*  INFO            */
	V2= pvm_pkdouble(&lf((V1)),1,1);
	if((0)==(V2)){
	goto L57;}
	L1(2,MAKE_FIXNUM(V2),VV[19])              /*  PVM-ERROR       */;
	}
L57:
	RETURN(0);
}
/*	function definition for C_PVM_PKSTR                           */
static L14(int narg, object V1, object V2)
{
	int x;
	x=(((object_to_int(V2) = pvm_pkint(&type,1,1)) == PvmOk) ?
         pvm_pkstr((V1)->st.st_self) : object_to_int(V2));
	VALUES(0)=MAKE_FIXNUM(x);
	RETURN(1);
}
/*	function definition for C-OBUFFER-SYMBOL                      */
static L15(int narg, object V1)
{ VT12 VLEX12 CLSR12
TTL:
	{object V2;                               /*  PNAME           */
	V2= symbol_name((V1));
	{int V3;                                  /*  LEN             */
	V3= ((V2))->v.v_fillp;
	L4(1,MAKE_FIXNUM(10))                     /*  PACK-TYPE-TAG   */;
	{int V4;                                  /*  INFO            */
	V4= (((V3 = pvm_pkint(&type,1,1)) == PvmOk) ?
         pvm_pkstr(((V2))->st.st_self) : V3);
	if((0)==(V4)){
	goto L61;}
	L1(2,MAKE_FIXNUM(V4),VV[23])              /*  PVM-ERROR       */;
	}
	}
	}
L61:
	RETURN(0);
}
/*	function definition for C-OBUFFER-STRING                      */
static L16(int narg, object V1)
{ VT13 VLEX13 CLSR13
TTL:
	{int V2;                                  /*  LEN             */
	V2= length((V1));
	L4(1,MAKE_FIXNUM(11))                     /*  PACK-TYPE-TAG   */;
	{int V3;                                  /*  INFO            */
	V3= (((V2 = pvm_pkint(&type,1,1)) == PvmOk) ?
         pvm_pkstr(((V1))->st.st_self) : V2);
	if((0)==(V3)){
	goto L68;}
	L1(2,MAKE_FIXNUM(V3),VV[24])              /*  PVM-ERROR       */;
	}
	}
L68:
	RETURN(0);
}
/*	function definition for C-OBUFFER-VECTOR-HEADER               */
static L17(int narg, object V1)
{ VT14 VLEX14 CLSR14
TTL:
	L4(1,MAKE_FIXNUM(12))                     /*  PACK-TYPE-TAG   */;
	{int V2;                                  /*  INFO            */
	V2= pvm_pkint(&fix((V1)),1,1);
	if((0)==(V2)){
	goto L75;}
	L1(2,MAKE_FIXNUM(V2),VV[25])              /*  PVM-ERROR       */;
	}
L75:
	RETURN(0);
}
/*	function definition for C-OBUFFER-LIST-HEADER                 */
static L18(int narg)
{ VT15 VLEX15 CLSR15
TTL:
	L4(1,MAKE_FIXNUM(13))                     /*  PACK-TYPE-TAG   */;
	RETURN(0);
}
/*	function definition for C_PVM_UNPACK_TAG                      */
static L19(int narg)
{
	object x;
	x= Cnil;
  { int tagval, info;
    info = pvm_upkint(&tagval,1,1);
    if (info != PvmOk) { VALUES(0) = Cnil; RETURN(1);}
    VALUES(0) = MAKE_FIXNUM(info);
    VALUES(1) = MAKE_FIXNUM(tagval);
    RETURN(2);
  };
	VALUES(0)=x;
	RETURN(1);
}
/*	function definition for IBUFFER-TAG                           */
static L20(int narg)
{ VT16 VLEX16 CLSR16
TTL:
	{ int V1;
	object V2;                                /*  INFO            */
	object V3;                                /*  VALUE           */
	V1=L23(0)                                 /*  C_PVM_UNPACK_INT*/;
	if (V1--==0) goto L81;
	V2= VALUES(0);
	if (V1--==0) goto L82;
	V3= VALUES(1);
	goto L83;
L81:
	V2= Cnil;
L82:
	V3= Cnil;
L83:
	if(((V2))==Cnil){
	goto L85;}
	VALUES(0) = (V3);
	RETURN(1);
L85:
	RETURN(L1(2,(V2),VV[26])                  /*  PVM-ERROR       */);}
}
/*	function definition for C-NEXT-MSG-TYPE                       */
static L21(int narg)
{ VT17 VLEX17 CLSR17
TTL:
	RETURN(L20(0)                             /*  IBUFFER-TAG     */);
}
/*	function definition for C_PVM_UNPACK_INT                      */
static L23(int narg)
{
	object x;
	x= Cnil;
  { int ival, info;
    info = pvm_upkint(&ival,1,1);
    if (info != PvmOk) { VALUES(0) = Cnil; RETURN(1);}
    VALUES(0) = MAKE_FIXNUM(info);
    VALUES(1) = MAKE_FIXNUM(ival);
    RETURN(2);
  };
	VALUES(0)=x;
	RETURN(1);
}
/*	function definition for C-IBUFFER-INT                         */
static L24(int narg)
{ VT18 VLEX18 CLSR18
TTL:
	{ int V1;
	object V2;                                /*  INFO            */
	object V3;                                /*  VALUE           */
	V1=L23(0)                                 /*  C_PVM_UNPACK_INT*/;
	if (V1--==0) goto L88;
	V2= VALUES(0);
	if (V1--==0) goto L89;
	V3= VALUES(1);
	goto L90;
L88:
	V2= Cnil;
L89:
	V3= Cnil;
L90:
	if(((V2))==Cnil){
	goto L92;}
	VALUES(0) = (V3);
	RETURN(1);
L92:
	RETURN(L1(2,(V2),VV[27])                  /*  PVM-ERROR       */);}
}
/*	function definition for C_PVM_UNPACK_CHAR                     */
static L25(int narg)
{
	object x;
	x= Cnil;
  { int info;
    char chval;
    info = pvm_upkbyte(&chval,1,1);
    if (info != PvmOk) { VALUES(0) = Cnil; RETURN(1);}
    VALUES(0) = MAKE_FIXNUM(info);
    VALUES(1) = code_char(chval);
    RETURN(2);
  };
	VALUES(0)=x;
	RETURN(1);
}
/*	function definition for C-IBUFFER-CHAR                        */
static L26(int narg)
{ VT19 VLEX19 CLSR19
TTL:
	{ int V1;
	object V2;                                /*  INFO            */
	object V3;                                /*  VALUE           */
	V1=L25(0)                                 /*  C_PVM_UNPACK_CHAR*/;
	if (V1--==0) goto L95;
	V2= VALUES(0);
	if (V1--==0) goto L96;
	V3= VALUES(1);
	goto L97;
L95:
	V2= Cnil;
L96:
	V3= Cnil;
L97:
	if(((V2))==Cnil){
	goto L99;}
	VALUES(0) = (V3);
	RETURN(1);
L99:
	RETURN(L1(2,(V2),VV[28])                  /*  PVM-ERROR       */);}
}
/*	function definition for C_PVM_UNPACK_FLOAT                    */
static L27(int narg)
{
	object x;
	x= Cnil;
  { int info;
    float fval;
    info = pvm_upkfloat(&fval,1,1);
    if (info != PvmOk) { VALUES(0) = Cnil; RETURN(1);}
    VALUES(0) = MAKE_FIXNUM(info);
    VALUES(1) = make_shortfloat(fval);
    RETURN(2);
  };
	VALUES(0)=x;
	RETURN(1);
}
/*	function definition for IBUFFER-FLOAT                         */
static L28(int narg)
{ VT20 VLEX20 CLSR20
TTL:
	{ int V1;
	object V2;                                /*  INFO            */
	object V3;                                /*  VALUE           */
	V1=L27(0)                                 /*  C_PVM_UNPACK_FLOAT*/;
	if (V1--==0) goto L102;
	V2= VALUES(0);
	if (V1--==0) goto L103;
	V3= VALUES(1);
	goto L104;
L102:
	V2= Cnil;
L103:
	V3= Cnil;
L104:
	if(((V2))==Cnil){
	goto L106;}
	VALUES(0) = (V3);
	RETURN(1);
L106:
	RETURN(L1(2,(V2),VV[29])                  /*  PVM-ERROR       */);}
}
/*	function definition for C_PVM_UNPACK_DOUBLE                   */
static L29(int narg)
{
	object x;
	x= Cnil;
  {
    int info;
    double dval;
    info = pvm_upkdouble(&dval,1,1);
    if (info != PvmOk) { VALUES(0) = Cnil; RETURN(1);}
    VALUES(0) = MAKE_FIXNUM(info);
    VALUES(1) = make_longfloat(dval);
    RETURN(2);
  };
	VALUES(0)=x;
	RETURN(1);
}
/*	function definition for C-IBUFFER-DOUBLE                      */
static L30(int narg)
{ VT21 VLEX21 CLSR21
TTL:
	{ int V1;
	object V2;                                /*  INFO            */
	object V3;                                /*  VALUE           */
	V1=L29(0)                                 /*  C_PVM_UNPACK_DOUBLE*/;
	if (V1--==0) goto L109;
	V2= VALUES(0);
	if (V1--==0) goto L110;
	V3= VALUES(1);
	goto L111;
L109:
	V2= Cnil;
L110:
	V3= Cnil;
L111:
	if(((V2))==Cnil){
	goto L113;}
	VALUES(0) = (V3);
	RETURN(1);
L113:
	RETURN(L1(2,(V2),VV[30])                  /*  PVM-ERROR       */);}
}
/*	function definition for SETSTRING                             */
static L31(int narg, object V1, object V2, object V3)
{ VT22 VLEX22 CLSR22
TTL:
	aset1((V3),fix((V2)),(V1));
	RETURN(0);
}
/*	function definition for C_PVM_UNPACK_CHARS                    */
static L32(int narg, object V1)
{
	object x;
	x=
  Cnil;
  { char *strchrs;
    int info;
    info = pvm_upkstr(strchrs);
    if (info != PvmOk) { VALUES(0) = Cnil; RETURN(1);}
    VALUES(0) = MAKE_FIXNUM(info);
    VALUES(1) = make_simple_string(strchrs);
    RETURN(2);
  };
	VALUES(0)=x;
	RETURN(1);
}
/*	function definition for GET-LENGTH-AND-STRING                 */
static L33(int narg)
{ VT23 VLEX23 CLSR23
TTL:
	{object V1;                               /*  LEN             */
	(*LK0)(0)                                 /*  IBUFFER-INT     */;
	V1= VALUES(0);
	{ int V2;
	object V3;                                /*  INFO            */
	object V4;                                /*  STR             */
	V2=L32(1,(V1))                            /*  C_PVM_UNPACK_CHARS*/;
	if (V2--==0) goto L118;
	V3= VALUES(0);
	if (V2--==0) goto L119;
	V4= VALUES(1);
	goto L120;
L118:
	V3= Cnil;
L119:
	V4= Cnil;
L120:
	if(((V3))==Cnil){
	goto L122;}
	if(!(number_compare(MAKE_FIXNUM(length((V4))),(V1))==0)){
	goto L125;}
	VALUES(0) = (V4);
	RETURN(1);
L125:
	RETURN(Lformat(4,Ct,VV[31],MAKE_FIXNUM(length((V4))),(V1))/*  FORMAT*/);
L122:
	RETURN(L1(2,(V3),VV[32])                  /*  PVM-ERROR       */);}
	}
}
/*	function definition for C-IBUFFER-SYMBOL                      */
static L34(int narg)
{ VT24 VLEX24 CLSR24
TTL:
	{object V1;                               /*  PNAME           */
	L33(0)                                    /*  GET-LENGTH-AND-STRING*/;
	V1= VALUES(0);
	RETURN(Lmake_symbol(1,(V1))               /*  MAKE-SYMBOL     */);
	}
}
/*	function definition for C-IBUFFER-STRING                      */
static L35(int narg)
{ VT25 VLEX25 CLSR25
TTL:
	RETURN(L33(0)                             /*  GET-LENGTH-AND-STRING*/);
}
/*	function definition for C-IBUFER-VECTOR-LENGTH                */
static L36(int narg)
{ VT26 VLEX26 CLSR26
TTL:
	RETURN(L24(0)                             /*  C-IBUFFER-INT   */);
}
/*	function definition for C_PVM_INITSEND                        */
static L37(int narg, object V1)
{
	int x;
	x=pvm_initsend(object_to_int(V1));
	VALUES(0)=MAKE_FIXNUM(x);
	RETURN(1);
}
/*	function definition for LPVM-INIT-SEND                        */
static L38(int narg, object V1)
{ VT27 VLEX27 CLSR27
TTL:
	if(type_of((V1))==t_fixnum||type_of((V1))==t_bignum){
	goto L129;}
	RETURN(Lerror(2,VV[36],TYPE_OF((V1)))     /*  ERROR           */);
L129:
	if(!(number_compare(MAKE_FIXNUM(0),(V1))>0)){
	goto L132;}
	RETURN(Lerror(2,VV[37],(V1))              /*  ERROR           */);
L132:
	{register int V2;                         /*  BUFID           */
	V2= pvm_initsend(fix((V1)));
	if(!((V2)<0)){
	goto L135;}
	L1(2,MAKE_FIXNUM(V2),VV[38])              /*  PVM-ERROR       */;
L135:
	VALUES(0) = MAKE_FIXNUM(V2);
	RETURN(1);
	}
}
/*	function definition for C_PVM_SEND                            */
static L39(int narg, object V1, object V2)
{
	int x;
	x=pvm_send(object_to_int(V1), object_to_int(V2));
	VALUES(0)=MAKE_FIXNUM(x);
	RETURN(1);
}
/*	function definition for LPVM-SEND-MESSAGE                     */
static L40(int narg, object V1, object V2, object V3, object V4, ...)
{ VT28 VLEX28 CLSR28
	{int i=4;
	object V5;
	va_list args; va_start(args, V4);
	if (i==narg) goto L138;
	V5= va_arg(args, object);
	i++;
	goto L139;
L138:
	V5= MAKE_FIXNUM(0);
L139:
	L38(1,(V5))                               /*  LPVM-INIT-SEND  */;
	(*LK1)(2,(V1),(V2))                       /*  WRITE-OBJECT    */;
	{int V6;                                  /*  INFO            */
	V6= pvm_send(fix((V4)), fix((V3)));
	if(!((V6)<0)){
	goto L143;}
	L1(2,MAKE_FIXNUM(V6),VV[42])              /*  PVM-ERROR       */;
	}
L143:
	RETURN(0);
	}
}
/*	function definition for LPVM-MULTICAST                        */
static L41(int narg, object V1, object V2, object V3, object V4, ...)
{ VT29 VLEX29 CLSR29
	{int i=4;
	volatile object V5;
	va_list args; va_start(args, V4);
	if (i==narg) goto L147;
	V5= va_arg(args, object);
	i++;
	goto L148;
L147:
	V5= MAKE_FIXNUM(0);
L148:
	L38(1,(V5))                               /*  LPVM-INIT-SEND  */;
	(*LK1)(2,(V1),(V2))                       /*  WRITE-OBJECT    */;
	{volatile object V6;
	volatile object V7;                       /*  TID             */
	V6= (V4);
	V7= Cnil;
L156:
	if(!((V6)==Cnil)){
	goto L157;}
	goto L152;
L157:
	V7= CAR((V6));
	{register int V9;                         /*  INFO            */
	V9= pvm_send(fix((V7)), fix((V3)));
	if(!((V9)<0)){
	goto L162;}
	L1(2,MAKE_FIXNUM(V9),VV[43])              /*  PVM-ERROR       */;
	}
L162:
	V6= CDR((V6));
	goto L156;
	}
L152:
	RETURN(0);
	}
}
/*	function definition for C_PVM_NRECV                           */
static L42(int narg, object V1, object V2)
{
	int x;
	x=pvm_nrecv(object_to_int(V1),object_to_int(V2));
	VALUES(0)=MAKE_FIXNUM(x);
	RETURN(1);
}
/*	function definition for LPVM-NONBLOCKING-RECV                 */
static L43(int narg, object V1, object V2, object V3)
{ VT30 VLEX30 CLSR30
TTL:
	{register int V4;                         /*  BUFID           */
	V4= pvm_nrecv(fix((V2)),fix((V3)));
	if(!((V4)<0)){
	goto L171;}
	RETURN(L1(2,MAKE_FIXNUM(V4),VV[47])       /*  PVM-ERROR       */);
L171:
	if(!((0)==(V4))){
	goto L174;}
	VALUES(0) = Cnil;
	RETURN(1);
L174:
	if(!((V4)>0)){
	goto L177;}
	RETURN((*LK2)(1,(V1))                     /*  READ-OBJECT     */);
L177:
	RETURN(Lerror(1,VV[48])                   /*  ERROR           */);
	}
}
/*	function definition for C_PVM_RECV                            */
static L44(int narg, object V1, object V2)
{
	int x;
	x=pvm_recv(object_to_int(V1), object_to_int(V2));
	VALUES(0)=MAKE_FIXNUM(x);
	RETURN(1);
}
/*	function definition for LPVM-BLOCKING-READ                    */
static L45(int narg, object V1, object V2, object V3)
{ VT31 VLEX31 CLSR31
TTL:
	{int V4;                                  /*  BUFID           */
	V4= pvm_recv(fix((V2)), fix((V3)));
	if(!((V4)<0)){
	goto L180;}
	L1(2,MAKE_FIXNUM(V4),VV[52])              /*  PVM-ERROR       */;
L180:
	RETURN((*LK2)(1,(V1))                     /*  READ-OBJECT     */);
	}
}
/*	function definition for C_PVM_MYTID                           */
static L46(int narg)
{
	int x;
	x=pvm_mytid();
	VALUES(0)=MAKE_FIXNUM(x);
	RETURN(1);
}
/*	function definition for LPVM-MY-TID                           */
static L47(int narg)
{ VT32 VLEX32 CLSR32
TTL:
	{register int V1;                         /*  INFO            */
	V1= pvm_mytid();
	if(!((V1)<0)){
	goto L184;}
	L1(2,MAKE_FIXNUM(V1),VV[56])              /*  PVM-ERROR       */;
L184:
	VALUES(0) = MAKE_FIXNUM(V1);
	RETURN(1);
	}
}
/*	function definition for C_PVM_EXIT                            */
static L48(int narg)
{
	int x;
	x=pvm_exit();
	VALUES(0)=MAKE_FIXNUM(x);
	RETURN(1);
}
/*	function definition for LPVM-EXIT                             */
static L49(int narg)
{ VT33 VLEX33 CLSR33
TTL:
	{int V1;                                  /*  INFO            */
	V1= pvm_exit();
	if((0)==(V1)){
	goto L187;}
	L1(2,MAKE_FIXNUM(V1),VV[60])              /*  PVM-ERROR       */;
	}
L187:
	RETURN(0);
}
/*	function definition for C_PVM_KILL                            */
static L50(int narg, object V1)
{
	int x;
	x=pvm_kill(object_to_int(V1));
	VALUES(0)=MAKE_FIXNUM(x);
	RETURN(1);
}
/*	function definition for LPVM-KILL                             */
static L51(int narg, object V1)
{ VT34 VLEX34 CLSR34
TTL:
	{int V2;                                  /*  INFO            */
	V2= pvm_kill(fix((V1)));
	if(!((V2)<0)){
	goto L191;}
	L1(2,MAKE_FIXNUM(V2),VV[64])              /*  PVM-ERROR       */;
	}
L191:
	RETURN(0);
}
/*	function definition for C_PVM_PARENT                          */
static L52(int narg)
{
	int x;
	x=pvm_parent();
	VALUES(0)=MAKE_FIXNUM(x);
	RETURN(1);
}
/*	function definition for LPVM-PARENT                           */
static L53(int narg)
{ VT35 VLEX35 CLSR35
TTL:
	{int V1;                                  /*  INFO            */
	V1= pvm_parent();
	if(!((V1)==(-23))){
	goto L195;}
	L1(2,MAKE_FIXNUM(V1),VV[68])              /*  PVM-ERROR       */;
	}
L195:
	RETURN(0);
}
/*	function definition for C_PVM_PSTAT                           */
static L54(int narg, object V1)
{
	int x;
	x=pvm_pstat(object_to_int(V1));
	VALUES(0)=MAKE_FIXNUM(x);
	RETURN(1);
}
/*	function definition for LPVM-PSTAT                            */
static L55(int narg, object V1)
{ VT36 VLEX36 CLSR36
TTL:
	{register int V2;                         /*  INFO            */
	V2= pvm_pstat(fix((V1)));
	if(!((V2)==(0))){
	goto L201;}
	VALUES(0) = MAKE_FIXNUM(V2);
	RETURN(1);
L201:
	if(!((V2)==(-31))){
	goto L204;}
	VALUES(0) = MAKE_FIXNUM(V2);
	RETURN(1);
L204:
	RETURN(L1(2,MAKE_FIXNUM(V2),VV[72])       /*  PVM-ERROR       */);
	}
}
/*	function definition for C_PVM_MSTAT                           */
static L56(int narg, object V1)
{
	int x;
	x=pvm_mstat(V1->st.st_self);
	VALUES(0)=MAKE_FIXNUM(x);
	RETURN(1);
}
/*	function definition for LPVM-MSTAT                            */
static L57(int narg, object V1)
{ VT37 VLEX37 CLSR37
TTL:
	if(type_of((V1))==t_string){
	goto L206;}
	Lerror(2,VV[76],TYPE_OF((V1)))            /*  ERROR           */;
L206:
	{register int V2;                         /*  INFO            */
	V2= pvm_mstat((V1)->st.st_self);
	if(!((V2)==(0))){
	goto L211;}
	VALUES(0) = VV[77];
	RETURN(1);
L211:
	if(!((V2)==(-6))){
	goto L214;}
	VALUES(0) = VV[78];
	RETURN(1);
L214:
	if(!((V2)==(-22))){
	goto L217;}
	VALUES(0) = VV[79];
	RETURN(1);
L217:
	RETURN(L1(2,MAKE_FIXNUM(V2),VV[80])       /*  PVM-ERROR       */);
	}
}
/*	function definition for C_PVM_SPAWN                           */
static L58(int narg, object V1, object V2, object V3, object V4)
{
	object x;
	x=
   Cnil;
   {
     int numt, tid, i;
     int sz = object_to_int(V2);
     object v;
     extern object lisp_package;

     siLmake_vector(7, intern("FIXNUM", lisp_package),
                        MAKE_FIXNUM(sz), Cnil, Cnil, Cnil, Cnil, Cnil);
     v = VALUES(0);
     numt = pvm_spawn(V1->st.st_self, 0, object_to_int(V2), V3->st.st_self, object_to_int(V4), v->v_self);
     if (numt < PvmOk) RETURN(1);
     VALUES(0) = MAKE_FIXNUM(numt);
     VALUES(1) = v;
     RETURN(2);
   };
	VALUES(0)=x;
	RETURN(1);
}
/*	function definition for LPVM-SPAWN                            */
static L59(int narg, object V1, object V2, object V3, object V4)
{ VT38 VLEX38 CLSR38
TTL:
	if(type_of((V1))==t_string){
	goto L220;}
	RETURN(Lerror(2,VV[81],TYPE_OF((V1)))     /*  ERROR           */);
L220:
	if(type_of((V2))==t_fixnum||type_of((V2))==t_bignum){
	goto L223;}
	RETURN(Lerror(2,VV[82],TYPE_OF((V2)))     /*  ERROR           */);
L223:
	if(type_of((V3))==t_string){
	goto L226;}
	RETURN(Lerror(2,VV[83],TYPE_OF((V3)))     /*  ERROR           */);
L226:
	if(type_of((V4))==t_fixnum||type_of((V4))==t_bignum){
	goto L229;}
	RETURN(Lerror(2,VV[84],TYPE_OF((V4)))     /*  ERROR           */);
L229:
	if(!(number_compare(MAKE_FIXNUM(1),(V4))<=0)){
	goto L231;}
	if(number_compare((V4),MAKE_FIXNUM(32))<=0){
	goto L232;}
L231:
	RETURN(Lerror(2,VV[85],(V4))              /*  ERROR           */);
L232:
	{ int V5;
	object V6;                                /*  NUM-SPAWNED     */
	object V7;                                /*  TIDS            */
	V5=L58(4,(V1),(V2),(V3),(V4))             /*  C_PVM_SPAWN     */;
	if (V5--==0) goto L237;
	V6= VALUES(0);
	if (V5--==0) goto L238;
	V7= VALUES(1);
	goto L239;
L237:
	V6= Cnil;
L238:
	V7= Cnil;
L239:
	if(!(number_compare(MAKE_FIXNUM(0),(V6))>0)){
	goto L241;}
	RETURN(L1(2,(V6),VV[86])                  /*  PVM-ERROR       */);
L241:
	VALUES(1) = (V7);
	VALUES(0) = (V6);
	RETURN(2);}
}
/*	function definition for C_PVM_SENDSIG                         */
static L60(int narg, object V1, object V2)
{
	int x;
	x=pvm_sendsig(object_to_int(V1),object_to_int(V2));
	VALUES(0)=MAKE_FIXNUM(x);
	RETURN(1);
}
/*	function definition for LPVM-SENDSIG                          */
static L61(int narg, object V1, object V2)
{ VT39 VLEX39 CLSR39
TTL:
	{int V3;                                  /*  INFO            */
	V3= pvm_sendsig(fix((V1)),fix((V2)));
	if(!((V3)<0)){
	goto L243;}
	L1(2,MAKE_FIXNUM(V3),VV[90])              /*  PVM-ERROR       */;
	}
L243:
	RETURN(0);
}
/*	function definition for C_PVM_ADVISE                          */
static L62(int narg, object V1)
{
	int x;
	x=pvm_advise(object_to_int(V1));
	VALUES(0)=MAKE_FIXNUM(x);
	RETURN(1);
}
/*	function definition for LPVM-ADVISE                           */
static L63(int narg, object V1)
{ VT40 VLEX40 CLSR40
TTL:
	{int V2;                                  /*  INFO            */
	V2= pvm_advise(fix((V1)));
	if((V2)==(0)){
	goto L247;}
	L1(2,MAKE_FIXNUM(V2),VV[94])              /*  PVM-ERROR       */;
	}
L247:
	RETURN(0);
}
static LKF2(int narg, ...) {TRAMPOLINK(VV[160],&LK2);}
static LKF1(int narg, ...) {TRAMPOLINK(VV[157],&LK1);}
static LKF0(int narg, ...) {TRAMPOLINK(VV[155],&LK0);}
