
;  Copyright 2000, 2001, 2002 Free Software Foundation, Inc.
; 
;  This file is part of the GNU MP Library.
; 
;  The GNU MP Library is free software; you can redistribute it and/or
;  modify it under the terms of the GNU Lesser General Public License as
;  published by the Free Software Foundation; either version 2.1 of the
;  License, or (at your option) any later version.
; 
;  The GNU MP Library is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  Lesser General Public License for more details.
; 
;  You should have received a copy of the GNU Lesser General Public
;  License along with the GNU MP Library; see the file COPYING.LIB.  If
;  not, write to the Free Software Foundation, Inc., 59 Temple Place -
;  Suite 330, Boston, MA 02111-1307, USA.
;
; Translation of AT&T syntax code by Brian Gladman 

%include "..\..\x86i.inc" 

%define       REG_AAAAAAAAAAAAAAAA    mm7
%define       REG_3333333333333333    mm6
%define       REG_0F0F0F0F0F0F0F0F    mm5
%define       REG_0000000000000000    mm4

%ifndef	PIC
	section	.data
	align   8

Lrodata_AAAAAAAAAAAAAAAA: 
    dd      0xAAAAAAAA
    dd      0xAAAAAAAA

Lrodata_3333333333333333: 
    dd      0x33333333
    dd      0x33333333

Lrodata_0F0F0F0F0F0F0F0F: 
    dd      0x0F0F0F0F
    dd      0x0F0F0F0F
%endif

%macro	ph_fun 1
	mov		ecx,[PARAM_SIZE]
	mov		eax,[PARAM_SRC]
%ifdef	PIC
	mov		edx,0xAAAAAAAA
	movd	mm7,edx
	punpckldq mm7,mm7
	mov		edx,0x33333333
	movd	mm6,edx
	punpckldq mm6,mm6
	mov		edx,0x0F0F0F0F
	movd	mm5,edx
	punpckldq mm5,mm5
%else
	movq	mm7,[Lrodata_AAAAAAAAAAAAAAAA]
	movq	mm6,[Lrodata_3333333333333333]
	movq	mm5,[Lrodata_0F0F0F0F0F0F0F0F]
%endif

%if	%1 == 1
	mov     edx,[PARAM_SRC2]
%endif
	pxor	mm4,mm4
	pxor	mm0,mm0
	sub		ecx,1
	ja		%%Ltop

%%Llast:
	movd	mm1,[eax+ecx*4]
%if	%1 == 1
	movd	mm2,[edx+ecx*4]
	pxor	mm1,mm2
%endif
	jmp	%%Lloaded

%%Ltop:
	movd	mm1,[eax]
	movd	mm2,[4+eax]
	punpckldq mm1,mm2
	add		eax,8
%if %1 == 1	
	movd	mm2,[edx]
	movd	mm3,[4+edx]
	punpckldq mm2,mm3
	pxor	mm1,mm2
	add		edx,8
%endif

%%Lloaded:
	movq	mm2,REG_AAAAAAAAAAAAAAAA
	pand	mm2,mm1
	psrlq	mm2,1
	psubd	mm1,mm2
	movq	mm2,REG_3333333333333333
	pand	mm2,mm1
	psrlq	mm1,2
	pand	mm1,REG_3333333333333333
	paddd	mm1,mm2

	movq	mm2,REG_0F0F0F0F0F0F0F0F
	pand	mm2,mm1
	psrlq	mm1,4
	pand	mm1,mm2
	paddd	mm1,mm2

	psadbw	mm1,REG_0000000000000000
	paddd	mm0,mm1

	sub		ecx,2
	jg		%%Ltop
	jz		%%Llast

	movd	eax,mm0
	emms
	ret
%endmacro

	section .text
	
%define	PARAM_SIZE  esp+frame+8 
%define PARAM_SRC   esp+frame+4 
%define	frame		0

	global	___gmpn_popcount
%ifdef	DLL
	export	___gmpn_popcount
%endif

	align   16
___gmpn_popcount:
	ph_fun	0

%define	PARAM_SIZE  esp+frame+12 
%define PARAM_SRC2  esp+frame+8 
%define PARAM_SRC   esp+frame+4 
%define	frame		0

	global	___gmpn_hamdist
%ifdef	DLL
	export	___gmpn_hamdist
%endif

	align   16
___gmpn_hamdist:
	ph_fun	1

	end
