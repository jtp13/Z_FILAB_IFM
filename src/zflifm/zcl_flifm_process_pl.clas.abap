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
class ZCL_FLIFM_PROCESS_PL definition
  public
  inheriting from ZCL_FLIFM_PROCESS_BSPL
  create public .

public section.

  methods BUILD_DATA
    redefinition .
  methods SET_TOP_LINE_COLOR
    redefinition .
  methods ZIF_FLIFM_PROCESS~CALC_RATIO_RATE
    redefinition .
  methods CREATE_DATA
    redefinition .
protected section.
  PRIVATE SECTION.

    METHODS _build_pl_rp_cysp_data
      IMPORTING
        !it_gl_tot TYPE zcl_flifm_fetch=>tyt_gl_tot
      RAISING
        zcx_flifm_exception .
    METHODS _build_pl_try_tcy_data
      IMPORTING
        !it_gl_tot TYPE zcl_flifm_fetch=>tyt_gl_tot
      RAISING
        zcx_flifm_exception .
    METHODS _build_pl_trend_data
      IMPORTING
        !it_gl_tot TYPE zcl_flifm_fetch=>tyt_gl_tot
      RAISING
        zcx_flifm_exception .
ENDCLASS.



CLASS ZCL_FLIFM_PROCESS_PL IMPLEMENTATION.


  METHOD build_data.

    create_data( ).

    build_fsv_data( mo_fetch->get_ifm_node_tab_pl( ) ).
    "If you want to display total in the PL Menu, delete comment
*    build_total_data( ).


  ENDMETHOD.


  METHOD calc_ratio_rate.


    DATA: lx_root TYPE REF TO cx_root.

    DATA: lr_data_fsv TYPE REF TO data.

    DATA: lt_fields TYPE tyt_dfies,
          ls_fields LIKE LINE OF lt_fields.

    FIELD-SYMBOLS: <lt_fsv>       TYPE table,
                   <ls_fsv>       TYPE data,
                   <ls_fsv_total> TYPE data,
                   <lv_per>       TYPE any,
                   <lv_total>     TYPE any,
                   <lv_amt>       TYPE any,
                   <lv_cyspamt>   TYPE any,
                   <lv_rpamt>     TYPE any,
                   <lv_idamt>     TYPE any.

    DEFINE _calc_ratio.
      IF &3 IS NOT INITIAL.
        TRY.
            &1 = &2 / &3 * 100.
          CATCH  cx_root INTO lx_root.
            &1 = 0.
        ENDTRY.
      ENDIF.
    END-OF-DEFINITION.

    DEFINE _calc_rate.
*      IF &3 IS NOT INITIAL.
      TRY.
          &1 = ( ( &3 - &2 ) / abs( &2 ) ) * 100.
        CATCH  cx_root INTO lx_root.
          &1 = 0.
      ENDTRY.
*      ENDIF.
    END-OF-DEFINITION.

*// Get fields

    CREATE DATA lr_data_fsv LIKE LINE OF ct_table.
    ASSIGN lr_data_fsv->* TO <ls_fsv>.
    ASSIGN lr_data_fsv->* TO <ls_fsv_total>.

    IF is_data IS INITIAL.
      READ TABLE ct_table INTO <ls_fsv_total> INDEX 1.
    ELSE.
      MOVE-CORRESPONDING is_data TO <ls_fsv_total>.
    ENDIF.


    LOOP AT ct_table ASSIGNING <ls_fsv>.

      CASE mv_menu.
        WHEN zif_flifm_definitions=>c_flifm_menu_type-pl_rp_cysp.

          ASSIGN COMPONENT 'RPAMT' OF STRUCTURE <ls_fsv_total> TO <lv_total>.

          ASSIGN COMPONENT 'RPAMT' OF STRUCTURE <ls_fsv> TO <lv_rpamt>.

          ASSIGN COMPONENT 'RPPER' OF STRUCTURE <ls_fsv> TO <lv_per>.

          _calc_ratio: <lv_per> <lv_rpamt> <lv_total>.

          ASSIGN COMPONENT 'CYSPAMT' OF STRUCTURE <ls_fsv_total> TO <lv_total>.

          ASSIGN COMPONENT 'CYSPAMT' OF STRUCTURE <ls_fsv> TO <lv_cyspamt>.

          ASSIGN COMPONENT 'CYSPPER' OF STRUCTURE <ls_fsv> TO <lv_per>.

          _calc_ratio: <lv_per> <lv_cyspamt> <lv_total>.

          ASSIGN COMPONENT 'IDAMT' OF STRUCTURE <ls_fsv> TO <lv_idamt>.

          <lv_idamt> = <lv_rpamt> - <lv_cyspamt>.

          ASSIGN COMPONENT 'IDPER' OF STRUCTURE <ls_fsv> TO <lv_per>.

          _calc_rate: <lv_per> <lv_cyspamt> <lv_rpamt>.

        WHEN zif_flifm_definitions=>c_flifm_menu_type-pl_try_tcy.

          ASSIGN COMPONENT 'TRYAMT' OF STRUCTURE <ls_fsv_total> TO <lv_total>.

          ASSIGN COMPONENT 'TRYAMT' OF STRUCTURE <ls_fsv> TO <lv_rpamt>.

          ASSIGN COMPONENT 'TRYPER' OF STRUCTURE <ls_fsv> TO <lv_per>.

          _calc_ratio: <lv_per> <lv_rpamt> <lv_total>.

          ASSIGN COMPONENT 'TCYAMT' OF STRUCTURE <ls_fsv_total> TO <lv_total>.

          ASSIGN COMPONENT 'TCYAMT' OF STRUCTURE <ls_fsv> TO <lv_cyspamt>.

          ASSIGN COMPONENT 'TCYPER' OF STRUCTURE <ls_fsv> TO <lv_per>.

          _calc_ratio: <lv_per> <lv_cyspamt> <lv_total>.

          ASSIGN COMPONENT 'IDAMT' OF STRUCTURE <ls_fsv> TO <lv_idamt>.

          <lv_idamt> = <lv_rpamt> - <lv_cyspamt>.

          ASSIGN COMPONENT 'IDPER' OF STRUCTURE <ls_fsv> TO <lv_per>.

          _calc_rate: <lv_per> <lv_cyspamt> <lv_rpamt>.

        WHEN zif_flifm_definitions=>c_flifm_menu_type-pl_ry_trend.

        WHEN zif_flifm_definitions=>c_flifm_menu_type-pl_cy_trend.

      ENDCASE.


    ENDLOOP.


  ENDMETHOD.


  method CREATE_DATA.

    DATA: lt_gl_tot TYPE zcl_flifm_fetch=>tyt_gl_tot,
          lr_bukrs  TYPE RANGE OF t001-bukrs.

    DATA: lv_type_name TYPE string.

    DATA: lx_error TYPE REF TO cx_sy_create_data_error.

    DATA: lv_msg TYPE string.

    TRY.

        DATA: lv_sub_menu TYPE zif_flifm_definitions=>ty_flifm_menu_type.

        lv_sub_menu = zcl_flifm_utils=>split_menu( iv_menu = mv_menu
                                                     iv_sub = abap_true ).

        CONCATENATE 'TYT_' lv_sub_menu INTO lv_type_name.
        CREATE DATA gr_data TYPE (lv_type_name).

        CLEAR lv_type_name.
        CONCATENATE 'TYT_' lv_sub_menu '_FSV' INTO lv_type_name.
        CREATE DATA gr_data_fsv        TYPE (lv_type_name).
        CREATE DATA gr_data_np_fsv     TYPE (lv_type_name).
        CREATE DATA gr_data_np_ytd_fsv TYPE (lv_type_name).

        CLEAR lv_type_name.
        CONCATENATE 'TYT_' lv_sub_menu '_DISPLAY' INTO lv_type_name.
        CREATE DATA gr_data_display      TYPE (lv_type_name).
        CREATE DATA gr_data_display_line TYPE LINE OF (lv_type_name).

        CLEAR lv_type_name.
        CONCATENATE 'TYT_' lv_sub_menu '_NP' INTO lv_type_name.
        CREATE DATA gr_data_np     TYPE (lv_type_name).
        CREATE DATA gr_data_np_ytd TYPE (lv_type_name).

      CATCH cx_sy_create_data_error INTO lx_error.
        lv_msg = lx_error->get_text( ).
        zcx_flifm_exception=>raise_msg( lv_msg ).
    ENDTRY.

    lt_gl_tot = mo_fetch->get_gl_tot( ).

    CASE mv_menu.
      WHEN zif_flifm_definitions=>c_flifm_menu_type-pl_rp_cysp.

        _build_pl_rp_cysp_data( lt_gl_tot ).

        gv_strname = mc_rp_cysp_tabname.

      WHEN zif_flifm_definitions=>c_flifm_menu_type-pl_try_tcy.

        _build_pl_try_tcy_data( lt_gl_tot ).

        gv_strname = mc_try_tcy_tabname.

      WHEN zif_flifm_definitions=>c_flifm_menu_type-pl_ry_trend OR
           zif_flifm_definitions=>c_flifm_menu_type-pl_cy_trend.

        _build_pl_trend_data( lt_gl_tot ).

        gv_strname = mc_trend_tabname.

      WHEN OTHERS.
    ENDCASE.

  endmethod.


  METHOD set_top_line_color.


    FIELD-SYMBOLS: <lv_amt00> TYPE any.

    CASE mv_menu.
      WHEN zif_flifm_definitions=>c_flifm_menu_type-pl_rp_cysp.

        FIELD-SYMBOLS: <lv_cyspamt> TYPE any,
                       <lv_rpamt>   TYPE any.

        ASSIGN COMPONENT 'CYSPAMT' OF STRUCTURE is_data TO <lv_cyspamt>.
        ASSIGN COMPONENT 'RPAMT' OF STRUCTURE is_data TO <lv_rpamt>.

        IF ( <lv_cyspamt> = 0 ) AND ( <lv_rpamt> = 0 ).
          mv_top_color = mc_tree_line_color-green.
        ELSE.
          mv_top_color = mc_tree_line_color-red.
        ENDIF.

      WHEN zif_flifm_definitions=>c_flifm_menu_type-pl_try_tcy.

        FIELD-SYMBOLS: <lv_tryamt> TYPE any,
                       <lv_tcyamt> TYPE any.

        ASSIGN COMPONENT 'TRYAMT' OF STRUCTURE is_data TO <lv_tryamt>.
        ASSIGN COMPONENT 'TCYAMT' OF STRUCTURE is_data TO <lv_tcyamt>.

        IF <lv_tryamt> = 0 AND <lv_tcyamt> = 0.
          mv_top_color = mc_tree_line_color-green.
        ELSE.
          mv_top_color = mc_tree_line_color-red.
        ENDIF.

      WHEN zif_flifm_definitions=>c_flifm_menu_type-pl_ry_trend OR
           zif_flifm_definitions=>c_flifm_menu_type-pl_cy_trend.

        ASSIGN COMPONENT 'AMT00' OF STRUCTURE is_data TO <lv_amt00>.

        IF <lv_amt00> = 0.
          mv_top_color = mc_tree_line_color-green.
        ELSE.
          mv_top_color = mc_tree_line_color-red.
        ENDIF.

    ENDCASE.


  ENDMETHOD.


  METHOD _build_pl_rp_cysp_data.


    DATA: ls_pl_rp_cysp    TYPE tys_rp_cysp,
          ls_pl_rp_cysp_np TYPE tys_rp_cysp_np.

    DATA: ls_gl_tot LIKE LINE OF it_gl_tot.

    DATA: lv_gjahr       TYPE gjahr,
          lv_cmp_gjahr   TYPE gjahr,
          lv_monat       TYPE monat,
          lv_inter_waers TYPE waers,
          lv_tcurr       TYPE waers,
          lr_bukrs       TYPE tyr_bukrs.

    DATA: lt_company TYPE zcl_flifm_fetch=>tyt_company,
          ls_company LIKE LINE OF lt_company.

    DATA: lv_ukurs TYPE zflifme_ukurs.

    FIELD-SYMBOLS: <lt_pl_rp_cysp>    TYPE table,
                   <lt_pl_rp_cysp_np> TYPE table.

    ASSIGN gr_data->*    TO <lt_pl_rp_cysp>.
    ASSIGN gr_data_np->* TO <lt_pl_rp_cysp_np>.

    lv_gjahr       = zcl_flifm_selection=>get_gjahr( ).
    lv_cmp_gjahr   = zcl_flifm_selection=>get_cmp_gjahr( ).
    lv_monat       = zcl_flifm_selection=>get_monat( ).
    lv_inter_waers = mo_fetch->get_inter_waers( ).
    lr_bukrs       = get_select_company( ).
    lv_tcurr       = get_to_currency( ).

    LOOP AT it_gl_tot INTO ls_gl_tot WHERE monat = lv_monat
                                       AND bukrs IN lr_bukrs.

      CLEAR ls_pl_rp_cysp.

      ls_pl_rp_cysp-hkont = ls_gl_tot-hkont.

      lv_ukurs = mo_fetch->get_tcurr( iv_gjahr = lv_gjahr
                                      iv_monat = lv_monat
                                      iv_fcurr = ls_gl_tot-waers
                                      iv_tcurr = lv_tcurr ).

      CASE ls_gl_tot-gjahr.
        WHEN lv_cmp_gjahr.
          "Same month of the comparison year
          ls_pl_rp_cysp-cyspamt = ls_gl_tot-mbamt * lv_ukurs * -1.

        WHEN lv_gjahr.

          ls_pl_rp_cysp-rpamt = ls_gl_tot-mbamt * lv_ukurs * -1.

      ENDCASE.

      ls_pl_rp_cysp-waers = lv_tcurr.

      COLLECT : ls_pl_rp_cysp INTO <lt_pl_rp_cysp>.

      IF ls_gl_tot-komok IS NOT INITIAL.
        CLEAR ls_pl_rp_cysp_np.
        MOVE-CORRESPONDING ls_pl_rp_cysp TO ls_pl_rp_cysp_np.
        ls_pl_rp_cysp_np-komok = ls_gl_tot-komok.
        ls_pl_rp_cysp_np-konts = ls_gl_tot-konts.
        COLLECT: ls_pl_rp_cysp_np INTO <lt_pl_rp_cysp_np>.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD _build_pl_trend_data.


    DATA: ls_pl_ry_trend        TYPE tys_ry_trend,
          ls_pl_ry_trend_np     TYPE tys_ry_trend_np,
          ls_pl_ry_trend_np_ytd TYPE tys_ry_trend_np.

    DATA: ls_gl_tot LIKE LINE OF it_gl_tot.

    DATA: lv_gjahr       TYPE gjahr,
          lv_monat       TYPE monat,
          lv_inter_waers TYPE waers,
          lv_tcurr       TYPE waers.

    DATA: lr_bukrs TYPE tyr_bukrs.

    DATA: lt_company TYPE zcl_flifm_fetch=>tyt_company,
          ls_company LIKE LINE OF lt_company.

    DATA lv_ukurs TYPE zflifme_ukurs.

    DATA lv_monat_amt TYPE string.

    FIELD-SYMBOLS: <lv_monat_amt> TYPE any.

    FIELD-SYMBOLS: <lt_pl_ry_trend>        TYPE table,
                   <lt_pl_ry_trend_np>     TYPE table,
                   <lt_pl_ry_trend_np_ytd> TYPE table.

    ASSIGN gr_data->*        TO <lt_pl_ry_trend>.
    ASSIGN gr_data_np->*     TO <lt_pl_ry_trend_np>.
    ASSIGN gr_data_np_ytd->* TO <lt_pl_ry_trend_np_ytd>.

    IF mv_menu = zif_flifm_definitions=>c_flifm_menu_type-pl_ry_trend.
      lv_gjahr = zcl_flifm_selection=>get_gjahr( ).
    ELSE.
      lv_gjahr = zcl_flifm_selection=>get_cmp_gjahr( ).
    ENDIF.
    lv_monat = zcl_flifm_selection=>get_monat( ).
    lv_inter_waers = mo_fetch->get_inter_waers( ).
    lr_bukrs = get_select_company( ).
    lv_tcurr = get_to_currency( ).

    LOOP AT it_gl_tot INTO ls_gl_tot WHERE gjahr = lv_gjahr
                                       AND bukrs IN lr_bukrs.

      CLEAR ls_pl_ry_trend.

      ls_pl_ry_trend-hkont = ls_gl_tot-hkont.

      lv_ukurs = mo_fetch->get_tcurr( iv_gjahr = lv_gjahr
                                      iv_monat = lv_monat
                                      iv_fcurr = ls_gl_tot-waers
                                      iv_tcurr = lv_tcurr ).

      IF ls_gl_tot-monat = lv_monat.
        ls_pl_ry_trend-amt00 = ls_gl_tot-ycbamt * lv_ukurs * -1.
      ENDIF.

      CONCATENATE 'LS_PL_RY_TREND-AMT' ls_gl_tot-monat INTO lv_monat_amt.
      ASSIGN (lv_monat_amt) TO <lv_monat_amt>.
      <lv_monat_amt> = ls_gl_tot-mbamt * lv_ukurs * -1.

      ls_pl_ry_trend-waers = lv_tcurr.

      COLLECT ls_pl_ry_trend INTO <lt_pl_ry_trend>.

      UNASSIGN <lv_monat_amt>.

      IF ls_gl_tot-komok IS NOT INITIAL.
        CLEAR: ls_pl_ry_trend_np, ls_pl_ry_trend_np_ytd.

        ls_pl_ry_trend_np-komok = ls_pl_ry_trend_np_ytd-komok = ls_gl_tot-komok.
        ls_pl_ry_trend_np-konts = ls_pl_ry_trend_np_ytd-konts = ls_gl_tot-konts.
        ls_pl_ry_trend_np-hkont = ls_pl_ry_trend_np_ytd-hkont = ls_gl_tot-hkont.
        ls_pl_ry_trend_np-waers = ls_pl_ry_trend_np_ytd-waers = lv_tcurr.

        IF ls_gl_tot-monat = lv_monat.
          ls_pl_ry_trend_np-amt00     =
          ls_pl_ry_trend_np_ytd-amt00 = ls_gl_tot-ycbamt * lv_ukurs * -1.
        ENDIF.

        CONCATENATE 'LS_PL_RY_TREND_NP-AMT' ls_gl_tot-monat INTO lv_monat_amt.
        ASSIGN (lv_monat_amt) TO <lv_monat_amt>.
        <lv_monat_amt> = ls_gl_tot-mbamt * lv_ukurs * -1.

        COLLECT: ls_pl_ry_trend_np INTO <lt_pl_ry_trend_np>.

        UNASSIGN <lv_monat_amt>.

        CONCATENATE 'LS_PL_RY_TREND_NP_YTD-AMT' ls_gl_tot-monat INTO lv_monat_amt.
        ASSIGN (lv_monat_amt) TO <lv_monat_amt>.
        <lv_monat_amt> = ls_gl_tot-ycbamt * lv_ukurs * -1.

        COLLECT: ls_pl_ry_trend_np_ytd INTO <lt_pl_ry_trend_np_ytd>.
      ENDIF.

      UNASSIGN <lv_monat_amt>.

    ENDLOOP.


  ENDMETHOD.


  METHOD _build_pl_try_tcy_data.


    DATA: ls_pl_try_tcy    TYPE tys_try_tcy,
          ls_pl_try_tcy_np TYPE tys_try_tcy_np.

    DATA: ls_gl_tot LIKE LINE OF it_gl_tot.

    DATA: lv_gjahr       TYPE gjahr,
          lv_cmp_gjahr   TYPE gjahr,
          lv_monat       TYPE monat,
          lv_inter_waers TYPE waers,
          lv_tcurr       TYPE waers,
          lr_bukrs       TYPE tyr_bukrs.

    DATA: lt_company TYPE zcl_flifm_fetch=>tyt_company,
          ls_company LIKE LINE OF lt_company.

    DATA: lv_ukurs TYPE zflifme_ukurs.

    FIELD-SYMBOLS: <lt_pl_try_tcy>    TYPE table,
                   <lt_pl_try_tcy_np> TYPE table.

    ASSIGN gr_data->*    TO <lt_pl_try_tcy>.
    ASSIGN gr_data_np->* TO <lt_pl_try_tcy_np>.

    lv_gjahr       = zcl_flifm_selection=>get_gjahr( ).
    lv_cmp_gjahr   = zcl_flifm_selection=>get_cmp_gjahr( ).
    lv_monat       = zcl_flifm_selection=>get_monat( ).
    lv_inter_waers = mo_fetch->get_inter_waers( ).
    lr_bukrs       = get_select_company( ).
    lv_tcurr       = get_to_currency( ).

    LOOP AT it_gl_tot INTO ls_gl_tot WHERE monat = lv_monat
                                       AND bukrs IN lr_bukrs.

      CLEAR ls_pl_try_tcy.

      ls_pl_try_tcy-hkont = ls_gl_tot-hkont.

      lv_ukurs = mo_fetch->get_tcurr( iv_gjahr = lv_gjahr
                                      iv_monat = lv_monat
                                      iv_fcurr = ls_gl_tot-waers
                                      iv_tcurr = lv_tcurr ).

      CASE ls_gl_tot-gjahr.
        WHEN lv_cmp_gjahr.
          "Same month of the comparison year
          ls_pl_try_tcy-tcyamt = ls_gl_tot-mcbamt * lv_ukurs * -1.

        WHEN lv_gjahr.

          ls_pl_try_tcy-tryamt = ls_gl_tot-mcbamt * lv_ukurs * -1.

      ENDCASE.

      ls_pl_try_tcy-waers = lv_tcurr.

      COLLECT : ls_pl_try_tcy INTO <lt_pl_try_tcy>.

      IF ls_gl_tot-komok IS NOT INITIAL.
        CLEAR ls_pl_try_tcy_np.
        MOVE-CORRESPONDING ls_pl_try_tcy TO ls_pl_try_tcy_np.
        ls_pl_try_tcy_np-komok = ls_gl_tot-komok.
        ls_pl_try_tcy_np-konts = ls_gl_tot-konts.
        COLLECT: ls_pl_try_tcy_np INTO <lt_pl_try_tcy_np>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
