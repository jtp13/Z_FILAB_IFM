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
CLASS zcl_flifm_fetch DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF tys_company,
        bukrs TYPE bseg-bukrs,
        butxt TYPE butxt,
        land1 TYPE t001-land1,
        waers TYPE t001-waers,
        ktopl TYPE t001-ktopl,
      END OF tys_company .
    TYPES:
      tyt_company TYPE HASHED TABLE OF tys_company WITH UNIQUE KEY bukrs .
    TYPES:
      tyt_dd03t TYPE STANDARD TABLE OF dd03t WITH DEFAULT KEY .
    TYPES:
      tyt_dd07t TYPE STANDARD TABLE OF dd07t WITH DEFAULT KEY .
    TYPES:
      tyt_gl_tot TYPE STANDARD TABLE OF zif_flifm_definitions=>tys_gl_tot WITH DEFAULT KEY .
    TYPES:
      BEGIN OF tys_tcurr,
        gjahr	TYPE gjahr,
        monat	TYPE monat,
        fcurr	TYPE fcurr_curr,
        tcurr	TYPE tcurr_curr,
        frate	TYPE zflifme_frate,
        trate	TYPE zflifme_trate,
        ukurs TYPE zflifme_ukurs,
      END OF tys_tcurr .
    TYPES:
      tyt_tcurr TYPE STANDARD TABLE OF tys_tcurr WITH DEFAULT KEY .

    DATA snd_jobs TYPE i VALUE 1 ##NO_TEXT.
    DATA rcv_jobs TYPE i VALUE 1 ##NO_TEXT.
    DATA started_jobs TYPE i .
    DATA completed_jobs TYPE i .
    DATA mv_running_arfc_wps TYPE i .
    DATA taskname TYPE syindex .
    DATA:
      excp_flag(1)        TYPE c .
    DATA mv_message TYPE bapiret2-message .
    CONSTANTS mc_rldnr TYPE faglflext-rldnr VALUE '0L' ##NO_TEXT.
    CONSTANTS mc_rrcty TYPE faglflext-rrcty VALUE '0' ##NO_TEXT.
    CONSTANTS mc_rvers TYPE faglflext-rvers VALUE '001' ##NO_TEXT.
    CONSTANTS mc_group TYPE rzlli_apcl VALUE 'parallel_generators' ##NO_TEXT.

    CLASS-METHODS get_instance
      IMPORTING
        !io_selection   TYPE REF TO zcl_flifm_selection OPTIONAL
      RETURNING
        VALUE(ro_fetch) TYPE REF TO zcl_flifm_fetch .
    METHODS set_data_init
      RAISING
        zcx_flifm_exception .
    METHODS fetch_data .
    METHODS fetch_data_single .
    METHODS fetch_data_parallel
      IMPORTING
        !iv_group TYPE rzlli_apcl .
    METHODS return_info
      IMPORTING
        !p_task TYPE clike .
    METHODS get_ifm_node_tab_bs
      RETURNING
        VALUE(rt_ifm_node_tab_bs) TYPE zif_flifm_definitions=>tyt_node_tab .
    METHODS get_ifm_node_tab_pl
      RETURNING
        VALUE(rt_ifm_node_tab_pl) TYPE zif_flifm_definitions=>tyt_node_tab .
    METHODS get_ifm_node_tab_tb
      RETURNING
        VALUE(rt_ifm_node_tab_tb) TYPE zif_flifm_definitions=>tyt_node_tab .
    METHODS get_company
      RETURNING
        VALUE(rt_company) TYPE tyt_company .
    METHODS get_dd03t
      RETURNING
        VALUE(rt_dd03t) TYPE tyt_dd03t .
    METHODS get_gl_tot
      RETURNING
        VALUE(rt_gl_tot) TYPE tyt_gl_tot .
    METHODS get_skat
      RETURNING
        VALUE(rt_skat) TYPE zif_flifm_definitions=>tyt_skat .
    METHODS get_layout
      RETURNING
        VALUE(rt_layout) TYPE zif_flifm_definitions=>tyt_layout .
    METHODS get_saved_layout
      RETURNING
        VALUE(rs_saved_layout) TYPE zif_flifm_definitions=>ty_saved_layout .
    METHODS get_inter_waers
      RETURNING
        VALUE(rv_inter_waers) TYPE waers .
    METHODS get_tcurr
      IMPORTING
        !iv_gjahr       TYPE gjahr
        !iv_monat       TYPE monat
        !iv_fcurr       TYPE fcurr_curr
        !iv_tcurr       TYPE tcurr_curr
      RETURNING
        VALUE(rv_ukurs) TYPE zflifme_ukurs
      RAISING
        zcx_flifm_exception .
    METHODS get_t030
      RETURNING
        VALUE(rt_t030) TYPE zif_flifm_definitions=>tyt_t030 .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF tys_company_select,
        bukrs TYPE bukrs,
        gjahr TYPE gjahr,
      END OF tys_company_select .
    TYPES:
      tyt_company_select TYPE STANDARD TABLE OF tys_company_select WITH DEFAULT KEY .
    TYPES:
      BEGIN OF tys_re, "Retained earnings items
        saknr TYPE saknr,
        gvtyp TYPE gvtyp,
        ktopl TYPE ktopl,
        komok TYPE komok,
        konts TYPE saknr,
      END OF tys_re .
    TYPES:
      tyt_re TYPE HASHED TABLE OF tys_re WITH UNIQUE KEY saknr .
    TYPES:
      BEGIN OF tys_faglflext,
        rbukrs TYPE faglflext-rbukrs,
        ryear  TYPE faglflext-ryear,
        drcrk  TYPE faglflext-drcrk,
        racct  TYPE faglflext-racct,
        rbusa  TYPE faglflext-rbusa,
        prctr  TYPE faglflext-prctr,
        hslvt  TYPE faglflext-hslvt,
        hsl01  TYPE faglflext-hsl01,
        hsl02  TYPE faglflext-hsl02,
        hsl03  TYPE faglflext-hsl03,
        hsl04  TYPE faglflext-hsl04,
        hsl05  TYPE faglflext-hsl05,
        hsl06  TYPE faglflext-hsl06,
        hsl07  TYPE faglflext-hsl07,
        hsl08  TYPE faglflext-hsl08,
        hsl09  TYPE faglflext-hsl09,
        hsl10  TYPE faglflext-hsl10,
        hsl11  TYPE faglflext-hsl11,
        hsl12  TYPE faglflext-hsl12,
        hsl13  TYPE faglflext-hsl13,
        hsl14  TYPE faglflext-hsl14,
        hsl15  TYPE faglflext-hsl15,
        hsl16  TYPE faglflext-hsl16,
      END OF tys_faglflext .
    TYPES:
      tyt_faglflext TYPE STANDARD TABLE OF tys_faglflext WITH DEFAULT KEY .
    TYPES:
      tyr_bukrs TYPE RANGE OF bukrs .
    TYPES:
      tyr_gjahr TYPE RANGE OF gjahr .

    CLASS-DATA go_fetch TYPE REF TO zcl_flifm_fetch .
    DATA mt_gl_tot TYPE tyt_gl_tot .
    DATA mt_company TYPE tyt_company .
    DATA:
      ms_company        TYPE LINE OF tyt_company .
    DATA mt_company_select TYPE tyt_company_select .
    DATA:
      ms_company_select TYPE LINE OF tyt_company_select .
    DATA mt_t030 TYPE zif_flifm_definitions=>tyt_t030 .
    DATA mt_re TYPE tyt_re .
    DATA mt_skat TYPE zif_flifm_definitions=>tyt_skat .
    DATA mt_layout TYPE zif_flifm_definitions=>tyt_layout .
    DATA ms_saved_layout TYPE zif_flifm_definitions=>ty_saved_layout .
    DATA mt_dd03t TYPE tyt_dd03t .
    DATA mt_dd07t TYPE tyt_dd07t .
    DATA mt_ifm_node_tab_bs TYPE zif_flifm_definitions=>tyt_node_tab .
    DATA mt_ifm_node_tab_pl TYPE zif_flifm_definitions=>tyt_node_tab .
    DATA mt_ifm_node_tab_tb TYPE zif_flifm_definitions=>tyt_node_tab .
    DATA mv_inter_waers TYPE waers .
    DATA mt_tcurr TYPE tyt_tcurr .

    METHODS _select_setting
      RAISING
        zcx_flifm_exception .
    METHODS _check_company_code
      RAISING
        zcx_flifm_exception .
    METHODS _build_re_accounts .
    METHODS _get_account_text .
    METHODS _get_field_text .
    METHODS _build_ifm_fsv_layout
      RAISING
        zcx_flifm_exception .
    METHODS _build_fsv_layout
      RAISING
        zcx_flifm_exception .
    METHODS _build_currency
      RAISING
        zcx_flifm_exception .
ENDCLASS.



CLASS ZCL_FLIFM_FETCH IMPLEMENTATION.


  METHOD fetch_data.

    zcl_flifm_progress=>show( iv_text = |Select data...| ).

*    IF zcl_flifm_selection=>get_parallel( ) EQ abap_true.
*      fetch_data_parallel( mc_group ).
*    ELSE.
    fetch_data_single( ).
*    ENDIF.

    SORT mt_gl_tot BY gjahr monat bukrs waers hkont.

    zcl_flifm_progress=>show( iv_text = |Preparing output...| ).

  ENDMETHOD.


  METHOD fetch_data_parallel.

  ENDMETHOD.


  METHOD fetch_data_single.


*// Build select data
    DATA: lr_bukrs  TYPE tyr_bukrs,
          lrs_bukrs LIKE LINE OF lr_bukrs,
          lr_gjahr  TYPE tyr_gjahr,
          lrs_gjahr LIKE LINE OF lr_gjahr.

    DATA lv_to_period TYPE zflifme_to_period.

    CLEAR lrs_gjahr.
    lrs_gjahr-sign = 'I'.
    lrs_gjahr-option = 'EQ'.
    lrs_gjahr-low = zcl_flifm_selection=>get_cmp_gjahr( ).
    APPEND lrs_gjahr TO lr_gjahr.

    CLEAR lrs_gjahr.
    lrs_gjahr-sign = 'I'.
    lrs_gjahr-option = 'EQ'.
    lrs_gjahr-low = zcl_flifm_selection=>get_gjahr( ).
    APPEND lrs_gjahr TO lr_gjahr.

    LOOP AT mt_company INTO ms_company.

      CLEAR lrs_bukrs.
      lrs_bukrs-sign = 'I'.
      lrs_bukrs-option = 'EQ'.
      lrs_bukrs-low = ms_company-bukrs.
      APPEND lrs_bukrs TO lr_bukrs.

    ENDLOOP.

    lv_to_period = zcl_flifm_selection=>get_to_period( ).

*// Select Data
    DATA: lt_faglflext TYPE tyt_faglflext,
          ls_faglflext LIKE LINE OF lt_faglflext.

    SELECT rbukrs
           ryear
           drcrk
           racct
           SUM( hslvt ) AS hslvt
           SUM( hsl01 ) AS hsl01
           SUM( hsl02 ) AS hsl02
           SUM( hsl03 ) AS hsl03
           SUM( hsl04 ) AS hsl04
           SUM( hsl05 ) AS hsl05
           SUM( hsl06 ) AS hsl06
           SUM( hsl07 ) AS hsl07
           SUM( hsl08 ) AS hsl08
           SUM( hsl09 ) AS hsl09
           SUM( hsl10 ) AS hsl10
           SUM( hsl11 ) AS hsl11
           SUM( hsl12 ) AS hsl12
           SUM( hsl13 ) AS hsl13
           SUM( hsl14 ) AS hsl14
           SUM( hsl15 ) AS hsl15
           SUM( hsl16 ) AS hsl16
      INTO CORRESPONDING FIELDS OF TABLE lt_faglflext
      FROM faglflext
     WHERE rldnr  EQ mc_rldnr
       AND rrcty  EQ mc_rrcty
       AND rvers  EQ mc_rvers
       AND rbukrs IN lr_bukrs
       AND ryear  IN lr_gjahr
     GROUP BY rbukrs
             ryear
             drcrk
             racct.

*// Build Total Data
    DATA ls_gl_tot LIKE LINE OF mt_gl_tot.
    DATA ls_re LIKE LINE OF mt_re.
    DATA ls_skat LIKE LINE OF mt_skat.

    FIELD-SYMBOLS: <lv_hsl>  TYPE any.
    DATA: lv_cnt(2)  TYPE n,
          lv_fnm(20) TYPE c.

    LOOP AT lt_faglflext INTO ls_faglflext.

      CLEAR: ms_company, ls_gl_tot.
      READ TABLE mt_company INTO ms_company WITH TABLE KEY bukrs = ls_faglflext-rbukrs.

      IF sy-subrc = 0.
        ls_gl_tot-waers = ms_company-waers.
      ENDIF.

      ls_gl_tot-gjahr = ls_faglflext-ryear.
      ls_gl_tot-bukrs = ls_faglflext-rbukrs.
      ls_gl_tot-hkont = ls_faglflext-racct.

      " Opening Balance
      ls_gl_tot-obamt = ls_faglflext-hslvt.

      CASE ls_faglflext-drcrk.
        WHEN 'S'.
          " Debit Opening Balance
          ls_gl_tot-dobamt = ls_faglflext-hslvt.
        WHEN 'H'.
          " Credit Opening Balance
          ls_gl_tot-cobamt = ls_faglflext-hslvt.
      ENDCASE.

      " Retained earnings
      READ TABLE mt_re INTO ls_re WITH TABLE KEY saknr = ls_faglflext-racct.

      IF sy-subrc = 0.
        ls_gl_tot-komok = ls_re-komok.
        ls_gl_tot-konts = ls_re-konts.
      ENDIF.

      " Total
      CLEAR : lv_cnt, lv_fnm.
      DO lv_to_period TIMES.
        lv_cnt = lv_cnt + 1.

        CONCATENATE 'LS_FAGLFLEXT-HSL' lv_cnt INTO lv_fnm.
        ASSIGN (lv_fnm)  TO  <lv_hsl>.

        " Period
        ls_gl_tot-monat = lv_cnt.
        " Monthly Balance
        ls_gl_tot-mbamt = <lv_hsl>.
        " Monthly Cumulative Balance
        ls_gl_tot-mcbamt = ls_gl_tot-mcbamt + <lv_hsl>.
        " Yearly Cumulative Balance = Opening Balance + Monthly Cumulative Balance
        ls_gl_tot-ycbamt = ls_gl_tot-obamt + ls_gl_tot-mcbamt.

        " T/B
        CASE ls_faglflext-drcrk.
          WHEN 'S'.
            "  Debit Balance
            ls_gl_tot-dbamt = <lv_hsl>.
            "  Debit Total
            ls_gl_tot-dtamt = ls_gl_tot-dtamt + <lv_hsl>.
          WHEN 'H'.
            "  Credit Balance
            ls_gl_tot-cbamt = <lv_hsl>.
            "  Credit Total
            ls_gl_tot-ctamt = ls_gl_tot-ctamt + <lv_hsl>.
        ENDCASE.

        COLLECT ls_gl_tot INTO mt_gl_tot.

      ENDDO.

    ENDLOOP.


  ENDMETHOD.


  METHOD get_company.

    rt_company = mt_company.

  ENDMETHOD.


  METHOD get_dd03t.

    rt_dd03t = mt_dd03t.

  ENDMETHOD.


  METHOD get_gl_tot.

    rt_gl_tot = mt_gl_tot.

  ENDMETHOD.


  METHOD get_ifm_node_tab_bs.

    rt_ifm_node_tab_bs = mt_ifm_node_tab_bs.

  ENDMETHOD.


  METHOD get_ifm_node_tab_pl.

    rt_ifm_node_tab_pl = mt_ifm_node_tab_pl.

  ENDMETHOD.


  METHOD get_ifm_node_tab_tb.

    rt_ifm_node_tab_tb = mt_ifm_node_tab_tb.

  ENDMETHOD.


  METHOD get_instance.

    IF go_fetch IS NOT BOUND.
      CREATE OBJECT go_fetch.
    ENDIF.

    ro_fetch = go_fetch.

  ENDMETHOD.


  METHOD get_inter_waers.

    rv_inter_waers = mv_inter_waers.

  ENDMETHOD.


  METHOD get_layout.

    rt_layout = mt_layout.

  ENDMETHOD.


  METHOD get_saved_layout.

    rs_saved_layout = ms_saved_layout.

  ENDMETHOD.


  METHOD get_skat.

    rt_skat = mt_skat.

  ENDMETHOD.


  METHOD get_t030.

    rt_t030 = mt_t030.

  ENDMETHOD.


  METHOD get_tcurr.


    DATA: ls_tcurr LIKE LINE OF mt_tcurr.

    READ TABLE mt_tcurr INTO ls_tcurr WITH KEY gjahr = iv_gjahr
                                               monat = iv_monat
                                               fcurr = iv_fcurr
                                               tcurr = iv_tcurr BINARY SEARCH.

    IF ls_tcurr-ukurs IS INITIAL.

      READ TABLE mt_tcurr INTO ls_tcurr WITH KEY gjahr = iv_gjahr
                                               monat = '0'
                                               fcurr = iv_fcurr
                                               tcurr = iv_tcurr BINARY SEARCH.

      IF ls_tcurr-ukurs IS INITIAL.

        zcx_flifm_exception=>raise_t100( iv_msgno = 007
                                         iv_msgv1 = |{ iv_fcurr }|
                                         iv_msgv2 = |{ iv_tcurr }|
                                         iv_msgv3 = |{ iv_monat } or 0|
                                         iv_msgv4 = |{ iv_gjahr }| ).
      ENDIF.

    ENDIF.

    rv_ukurs = ls_tcurr-ukurs.


  ENDMETHOD.


  METHOD return_info.

  ENDMETHOD.


  METHOD set_data_init.

    _select_setting( ).

    _check_company_code( ).

    _build_re_accounts( ).

    _get_account_text( ).

    _get_field_text( ).

    _build_ifm_fsv_layout( ).

*// FSV Layout / T-Code : FSE3
    _build_fsv_layout( ).

    _build_currency( ).

  ENDMETHOD.


  METHOD _build_currency.

*// Check currency key
    mv_inter_waers = zcl_flifm_selection=>get_waers( ).

    IF mv_inter_waers IS INITIAL.
      zcx_flifm_exception=>raise_t100( iv_msgno = 006 ).
    ENDIF.

*// Get the set exchange rate.
    DATA: lv_gjahr     TYPE gjahr,
          lv_cmp_gjahr TYPE gjahr,
          lv_monat     TYPE monat.

    lv_gjahr = zcl_flifm_selection=>get_gjahr( ).
    lv_cmp_gjahr = zcl_flifm_selection=>get_cmp_gjahr( ).
    lv_monat = zcl_flifm_selection=>get_monat( ).

    SELECT * INTO CORRESPONDING FIELDS OF TABLE mt_tcurr
      FROM zflifmt_tcurr
      WHERE gjahr IN (lv_gjahr, lv_cmp_gjahr)
    AND monat IN (lv_monat, '00').

    IF sy-subrc <> 0.
*-- If not set intetgrated currency....
*      zcx_flifm_exception=>raise_t100( iv_msgno = 008 ).
    ENDIF.

    DATA: lv_tdec   TYPE currdec,
          lv_fdec   TYPE currdec,
          lv_tmpdec TYPE i.

    FIELD-SYMBOLS: <ls_tcurr>     LIKE LINE OF mt_tcurr,
                   <ls_opp_tcurr> LIKE LINE OF mt_tcurr.

    LOOP AT mt_tcurr ASSIGNING <ls_tcurr>.

      CLEAR : lv_fdec, lv_tdec.

      <ls_tcurr>-ukurs = <ls_tcurr>-trate / <ls_tcurr>-frate.

      SELECT SINGLE currdec INTO lv_fdec
              FROM tcurx WHERE currkey = <ls_tcurr>-fcurr.

      IF sy-subrc <> 0.
        lv_fdec = 2.
      ENDIF.

      SELECT SINGLE currdec INTO lv_tdec
              FROM tcurx WHERE currkey = <ls_tcurr>-tcurr.

      IF sy-subrc <> 0.
        lv_tdec = 2.
      ENDIF.

      IF lv_fdec <> lv_tdec AND lv_fdec <> 0.
        lv_tmpdec = 10 ** lv_fdec.
        <ls_tcurr>-ukurs = <ls_tcurr>-ukurs / lv_tmpdec.
      ENDIF.

    ENDLOOP.

*// Set input currecny to reverse.- Except for manual input.
    DATA: ls_tcurr LIKE LINE OF mt_tcurr.

    LOOP AT mt_tcurr ASSIGNING <ls_tcurr>.

      MOVE-CORRESPONDING <ls_tcurr> TO ls_tcurr.

      READ TABLE mt_tcurr TRANSPORTING NO FIELDS
        WITH KEY monat = ls_tcurr-monat fcurr = ls_tcurr-tcurr tcurr = ls_tcurr-fcurr.

      IF sy-subrc NE 0.

        APPEND INITIAL LINE TO mt_tcurr ASSIGNING <ls_opp_tcurr>.

        CLEAR : lv_fdec, lv_tdec.

        <ls_opp_tcurr>-gjahr = <ls_tcurr>-gjahr.
        <ls_opp_tcurr>-monat = <ls_tcurr>-monat.
        <ls_opp_tcurr>-fcurr = <ls_tcurr>-tcurr.
        <ls_opp_tcurr>-tcurr = <ls_tcurr>-fcurr.
        <ls_opp_tcurr>-frate = <ls_tcurr>-trate.
        <ls_opp_tcurr>-trate = <ls_tcurr>-frate.

        <ls_opp_tcurr>-ukurs = <ls_opp_tcurr>-trate / <ls_opp_tcurr>-frate.

        SELECT SINGLE currdec INTO lv_fdec
                FROM tcurx WHERE currkey = <ls_opp_tcurr>-fcurr.
        IF sy-subrc <> 0.
          lv_fdec = 2.
        ENDIF.

        SELECT SINGLE currdec INTO lv_tdec
                FROM tcurx WHERE currkey = <ls_opp_tcurr>-tcurr.
        IF sy-subrc <> 0.
          lv_tdec = 2.
        ENDIF.

        IF lv_fdec <> lv_tdec AND lv_fdec <> 0.
          lv_tmpdec = 10 ** lv_fdec.
          <ls_opp_tcurr>-ukurs = <ls_opp_tcurr>-ukurs /  lv_tmpdec.
        ENDIF.

      ENDIF.

    ENDLOOP.

*// Create lines with FCUURR = TCURR
    LOOP AT mt_company INTO ms_company.

      APPEND INITIAL LINE TO mt_tcurr ASSIGNING <ls_tcurr>.

      <ls_tcurr>-gjahr = lv_gjahr.
      <ls_tcurr>-monat = lv_monat.
      <ls_tcurr>-tcurr = <ls_tcurr>-fcurr = ms_company-waers.
      <ls_tcurr>-ukurs = '1.00'.

      APPEND INITIAL LINE TO mt_tcurr ASSIGNING <ls_tcurr>.

      <ls_tcurr>-gjahr = lv_gjahr.
      <ls_tcurr>-monat = '00'.
      <ls_tcurr>-tcurr = <ls_tcurr>-fcurr = ms_company-waers.
      <ls_tcurr>-ukurs = '1.00'.

      APPEND INITIAL LINE TO mt_tcurr ASSIGNING <ls_tcurr>.

      <ls_tcurr>-gjahr = lv_cmp_gjahr.
      <ls_tcurr>-monat = lv_monat.
      <ls_tcurr>-tcurr = <ls_tcurr>-fcurr = ms_company-waers.
      <ls_tcurr>-ukurs = '1.00'.

      APPEND INITIAL LINE TO mt_tcurr ASSIGNING <ls_tcurr>.

      <ls_tcurr>-gjahr = lv_cmp_gjahr.
      <ls_tcurr>-monat = '00'.
      <ls_tcurr>-tcurr = <ls_tcurr>-fcurr = ms_company-waers.
      <ls_tcurr>-ukurs = '1.00'.

    ENDLOOP.

    SORT mt_tcurr BY gjahr monat fcurr tcurr.

  ENDMETHOD.


  METHOD _build_fsv_layout.

    mt_ifm_node_tab_bs = zcl_flifm_fsv=>get_fsv_tree(
                                          it_layout = mt_layout
                                          iv_menu = zif_flifm_definitions=>c_flifm_menu_type-bs
                                          iv_versn = zcl_flifm_selection=>get_versn( ) ).

    mt_ifm_node_tab_pl = zcl_flifm_fsv=>get_fsv_tree(
                                          it_layout = mt_layout
                                          iv_menu = zif_flifm_definitions=>c_flifm_menu_type-pl
                                          iv_versn = zcl_flifm_selection=>get_versn( ) ).

    mt_ifm_node_tab_tb = zcl_flifm_fsv=>get_fsv_tree(
                                          it_layout = mt_layout
                                          iv_menu = zif_flifm_definitions=>c_flifm_menu_type-tb
                                          iv_versn = zcl_flifm_selection=>get_versn( ) ).

  ENDMETHOD.


  METHOD _build_ifm_fsv_layout.


*-> Financial Statement Version / T-Code : FSE3
*-- If you want to see the item keys, go to Menu: Settings-Change.
*-- Check Item keys visible.
*-- If 'Item keys visible' is disable, your fsv is not automatically allocate item keys.
*-- Check table T011(Financial Statement Versions). Field: XAUTO

    DATA: ls_layout LIKE LINE OF mt_layout.

    DATA lv_versn TYPE versn_011.
    DATA ls_saved_layout TYPE zif_flifm_definitions=>ty_saved_layout.

    lv_versn = zcl_flifm_selection=>get_versn( ).

    SELECT SINGLE * INTO ms_saved_layout
      FROM zflifmt_layout
      WHERE versn = lv_versn.

    IF sy-subrc <> 0.
      zcx_flifm_exception=>raise_t100( iv_msgno = 009 ).
    ENDIF.

    ls_layout-item = zif_flifm_definitions=>c_flifm_menu_type-bs.
    ls_layout-name = ms_saved_layout-assets.
    APPEND ls_layout TO mt_layout.
    ls_layout-item = zif_flifm_definitions=>c_flifm_menu_type-tb.
    APPEND ls_layout TO mt_layout.

    CLEAR ls_layout.
    ls_layout-item = zif_flifm_definitions=>c_flifm_menu_type-bs.
    ls_layout-name = ms_saved_layout-liab_equity.
    APPEND ls_layout TO mt_layout.
    ls_layout-item = zif_flifm_definitions=>c_flifm_menu_type-tb.
    APPEND ls_layout TO mt_layout.

    CLEAR ls_layout.
    ls_layout-item = zif_flifm_definitions=>c_flifm_menu_type-pl.
    ls_layout-name = ms_saved_layout-pl.
    APPEND ls_layout TO mt_layout.
    ls_layout-item = zif_flifm_definitions=>c_flifm_menu_type-tb.
    APPEND ls_layout TO mt_layout.

    SORT mt_layout BY item.


  ENDMETHOD.


  METHOD _build_re_accounts.


    DATA: ls_t030 LIKE LINE OF mt_t030,
          lt_ska1 TYPE STANDARD TABLE OF ska1 WITH DEFAULT KEY,
          ls_ska1 LIKE LINE OF lt_ska1,
          ls_re   TYPE tys_re.

    DATA: lv_ktopl TYPE ktopl.

    lv_ktopl = zcl_flifm_selection=>get_ktopl( ).

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE mt_t030
      FROM t030
      WHERE ktopl = lv_ktopl
      AND ktosl = 'BIL'.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_ska1
      FROM ska1
      WHERE ktopl = lv_ktopl
      AND xbilk = space.

    LOOP AT lt_ska1 INTO ls_ska1.

      MOVE-CORRESPONDING ls_ska1 TO ls_re.

      CLEAR ls_t030.
      READ TABLE mt_t030 INTO ls_t030 WITH KEY komok = ls_ska1-gvtyp.

      IF sy-subrc = 0.
        ls_re-komok = ls_t030-komok.
        ls_re-konts = ls_t030-konts.
      ENDIF.

      INSERT ls_re INTO TABLE mt_re.

    ENDLOOP.


  ENDMETHOD.


  METHOD _check_company_code.

    DATA: lr_bukrs TYPE zcl_flifm_selection=>tyr_bukrs.
    DATA: lv_ktopl TYPE ktopl.

    lv_ktopl = zcl_flifm_selection=>get_ktopl( ).
    lr_bukrs = zcl_flifm_selection=>get_bukrs( ).

    SELECT bukrs butxt land1 waers ktopl
      FROM t001
    INTO CORRESPONDING FIELDS OF TABLE mt_company
      WHERE bukrs IN lr_bukrs
      AND ktopl = lv_ktopl.

    IF sy-subrc <> 0.
      zcx_flifm_exception=>raise_t100( iv_msgno = 005 ).
    ENDIF.

  ENDMETHOD.


  METHOD _get_account_text.

    DATA: lv_ktopl TYPE ktopl.
    DATA: lr_bukrs TYPE zcl_flifm_selection=>tyr_bukrs.

    lv_ktopl = zcl_flifm_selection=>get_ktopl( ).
    lr_bukrs = zcl_flifm_selection=>get_bukrs( ).

    SELECT DISTINCT b~saknr b~txt50
      INTO TABLE mt_skat
     FROM t001 AS a INNER JOIN skat AS b ON a~ktopl = b~ktopl
      WHERE b~spras = sy-langu
      AND a~bukrs IN lr_bukrs
      AND a~ktopl = lv_ktopl.

    SORT mt_skat.

  ENDMETHOD.


  METHOD _get_field_text.

    SELECT * INTO TABLE mt_dd03t
         FROM dd03t
        WHERE tabname    LIKE 'ZFLIFM%'
          AND as4local   = 'A'
          AND ddlanguage = sy-langu.

    IF sy-subrc <> 0.
      SELECT * INTO TABLE mt_dd03t
               FROM dd03t
              WHERE tabname    LIKE 'ZFLIFM%'
                AND as4local   = 'A'
                AND ddlanguage = 'E'.
    ENDIF.

    SORT mt_dd03t BY tabname fieldname.


  ENDMETHOD.


  METHOD _select_setting.


    DATA: ls_setting TYPE zflifmt_setting.

    SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_setting
      FROM zflifmt_setting.

    IF sy-subrc = 0.

      zcl_flifm_selection=>set_data(
        iv_ktopl     = ls_setting-ktopl
        iv_versn     = ls_setting-versn
        iv_waers     = ls_setting-waers
        iv_to_period = ls_setting-to_period
        iv_parallel  = ls_setting-parallel ).

    ELSE.
      zcx_flifm_exception=>raise_t100( iv_msgno = 011 ).
    ENDIF.


  ENDMETHOD.
ENDCLASS.
