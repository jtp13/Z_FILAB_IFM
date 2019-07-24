*&---------------------------------------------------------------------*
*& Report ZFLIFMR
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& MIT License
*&
*& Copyright (c) 2016 FI LAB. All rights reserved.
*&
*& Permission is hereby granted, free of charge, to any person obtaining
*& a copy of this software and associated documentation files (the "Software"),
*& to deal in the Software without restriction, including without limitation
*& the rights to use, copy, modify, merge, publish, distribute, sublicense,
*& and/or sell copies of the Software, and to permit persons to whom the
*& Software is furnished to do so, subject to the following conditions:
*&
*& The above copyright notice and this permission notice shall be included
*& in all copies or substantial portions of the Software.
*&
*& THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
*& OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*& FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*& AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*& LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*& OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
*& IN THE SOFTWARE.
*&---------------------------------------------------------------------*

REPORT zflifmr MESSAGE-ID zmc_flifm
LINE-SIZE 120.

*----------------------------------------------------------------------*
* CLASS declaration.
*----------------------------------------------------------------------*
INCLUDE zflifm_ifmr_screen_class.
INCLUDE zflifm_ifmr_main_class.

*----------------------------------------------------------------------*
* TABLES declaration.
*----------------------------------------------------------------------*
TABLES: zflifmt_gl_tot,
        zflifmt_setting.

*----------------------------------------------------------------------*
* Data declaration.
*----------------------------------------------------------------------*
DATA go_main TYPE REF TO zcl_flifm_main.

DATA lx_exception TYPE REF TO zcx_flifm_exception.

DATA lv_gjahr TYPE gjahr.

*----------------------------------------------------------------------*
* SELECTION SCREEN.
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE tblock1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) gja_i18n.
PARAMETERS: p_gjahr LIKE zflifmt_gl_tot-gjahr OBLIGATORY.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) mon_i18n.
PARAMETERS:     p_monat LIKE zflifmt_gl_tot-monat OBLIGATORY.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (28) buk_i18n.
SELECT-OPTIONS: s_bukrs FOR zflifmt_gl_tot-bukrs NO INTERVALS.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE tblock2.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) cja_i18n.
PARAMETERS: p_cgjahr LIKE zflifmt_gl_tot-gjahr OBLIGATORY.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_chk LIKE zflifmt_gl_tot-gjahr NO-DISPLAY.

SELECTION-SCREEN END OF BLOCK bl2.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE tblock3.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) lev_i18n.
PARAMETERS: p_level  LIKE snodetext-tlevel OBLIGATORY DEFAULT '02'.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_zero LIKE rfpdo-bilanull DEFAULT space.

SELECTION-SCREEN END OF BLOCK bl3.

*---------------------------------------------------------------------*
* INITIALIZATION.
*---------------------------------------------------------------------*
INITIALIZATION.

  tblock1 = zcl_flifm_i18n=>get_instance( )->block1.
  tblock2 = zcl_flifm_i18n=>get_instance( )->block2.
  tblock3 = zcl_flifm_i18n=>get_instance( )->block3.

  gja_i18n = zcl_flifm_i18n=>get_instance( )->reporting_year.
  mon_i18n = zcl_flifm_i18n=>get_instance( )->reporting_period.
  buk_i18n = zcl_flifm_i18n=>get_instance( )->company_code.
  cja_i18n = zcl_flifm_i18n=>get_instance( )->comparison_year.
  lev_i18n = zcl_flifm_i18n=>get_instance( )->tree_display_level.

  zcl_flifm_selection=>initialize( IMPORTING
                                    ev_gjahr     = p_gjahr
                                    ev_monat     = p_monat
                                    ev_cmp_gjahr = p_cgjahr ).

  p_chk = p_gjahr.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON p_monat.
  zcl_flifm_selection=>validate_input( p_monat ).

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF p_gjahr <> p_chk.
    p_cgjahr = p_gjahr - 1.
    p_chk = p_gjahr.
  ENDIF.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  zcl_flifm_selection=>set_data(
    ir_bukrs     = s_bukrs[]
    iv_gjahr     = p_gjahr
    iv_monat     = p_monat
    iv_cmp_gjahr = p_cgjahr
    iv_level     = p_level
    iv_zero      = p_zero ).

  TRY.
      zcl_flifm_fetch=>get_instance( )->set_data_init( ).
      zcl_flifm_fetch=>get_instance( )->fetch_data( ).
    CATCH zcx_flifm_exception INTO lx_exception.
      MESSAGE lx_exception TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  TRY.
      CREATE OBJECT go_main
        EXPORTING
          iv_dynnr = '0100'.
    CATCH zcx_flifm_exception INTO lx_exception.
      MESSAGE lx_exception TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

*----------------------------------------------------------------------*
*  MODULE pbo OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pbo OUTPUT.
  go_main->pbo( ).
ENDMODULE.                 " PBO  OUTPUT

*----------------------------------------------------------------------*
*  MODULE pai INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pai INPUT.
  go_main->pai( sy-ucomm ).
ENDMODULE.                 " PAI  INPUT
