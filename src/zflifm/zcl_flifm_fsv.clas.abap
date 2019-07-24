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
class ZCL_FLIFM_FSV definition
  public
  create public .

public section.

  types TY_BS_TYPE type I .
  types:
    BEGIN OF tys_bs_info,
*     info about BS given out to clients
        ktopl TYPE ktopl_011,
        xauto TYPE xauto_011,
        type  TYPE ty_bs_type,
      END OF tys_bs_info .
  types:
    tyt_node_tab TYPE STANDARD TABLE OF snodetext WITH DEFAULT KEY .
  types T_BS_NODE_ID type FIBS_BS_NODE_ID .
  types:
    tyt_t_bs_node_tab TYPE STANDARD TABLE OF t_bs_node_id WITH DEFAULT KEY .
  types:
    BEGIN OF tys_split_name,
        ktopl     TYPE ktopl,
        acct_from TYPE vonkt_011z,
        pos       TYPE c LENGTH 3,
        acct_to   TYPE biskt_011z,
      END OF tys_split_name .

  class-methods GET_FSV_TREE
    importing
      value(IT_LAYOUT) type ZIF_FLIFM_DEFINITIONS=>TYT_LAYOUT
      value(IV_MENU) type ZIF_FLIFM_DEFINITIONS=>TY_FLIFM_MENU_TYPE
      value(IV_VERSN) type VERSN_011
    returning
      value(RT_IFM_NODE_TAB) type ZIF_FLIFM_DEFINITIONS=>TYT_NODE_TAB
    raising
      ZCX_FLIFM_EXCEPTION .
protected section.
private section.

  class-data MT_LAYOUT type ZIF_FLIFM_DEFINITIONS=>TYT_LAYOUT .
  class-data MS_RFGBST type RFGBST .
  class-data MV_VERSN type T011-VERSN .
  class-data MS_BS_INFO type TYS_BS_INFO .
  class-data MT_NODE_TAB type TYT_NODE_TAB .
  class-data MS_NODE_TAB type SNODETEXT .
  class-data MV_LEVEL type I .

  class-methods INIT_FIRST .
  class-methods LOAD_VERSN
    raising
      ZCX_FLIFM_EXCEPTION .
  class-methods LOAD_VERSN_TEXT .
  class-methods CREATE_NODE_TAB
    returning
      value(RT_NODE_TAB) type TYT_NODE_TAB .
  class-methods ADD_NODE_WITH_SUBTREE
    importing
      !IV_LEVEL type I
      !IS_NODE_ID type T_BS_NODE_ID
    changing
      !CT_NODE_TAB type TYT_NODE_TAB .
  class-methods CREATE_IFM_NODE_TAB
    importing
      !IT_NODE_TAB type TYT_NODE_TAB
    returning
      value(RT_IFM_NODE_TAB) type ZIF_FLIFM_DEFINITIONS=>TYT_NODE_TAB
    raising
      ZCX_FLIFM_EXCEPTION .
  class-methods CREATE_IFM_NODE_TAB_ONE_STEP
    importing
      !IT_NODE_TAB type TYT_NODE_TAB
    returning
      value(RT_TEMP_NODE_TAB) type TYT_NODE_TAB
    raising
      ZCX_FLIFM_EXCEPTION .
  class-methods CREATE_IFM_NODE_TAB_TWO_STEP
    importing
      !IT_TEMP_NODE_TAB type TYT_NODE_TAB
    returning
      value(RT_IFM_NODE_TAB) type ZIF_FLIFM_DEFINITIONS=>TYT_NODE_TAB .
ENDCLASS.



CLASS ZCL_FLIFM_FSV IMPLEMENTATION.


  method ADD_NODE_WITH_SUBTREE.


    DATA: children_node_tab TYPE tyt_t_bs_node_tab,
          children_node     TYPE t_bs_node_id,
          lv_level          TYPE i.

* Get the display info
    CALL FUNCTION 'FI_BS_NODE_GET_SNODETEXT'
      EXPORTING
        node_id          = is_node_id
        with_gl_accounts = ms_rfgbst-glac_on
      IMPORTING
        attr             = ms_node_tab
      EXCEPTIONS
        node_not_found   = 01.

    ms_node_tab-tlevel = iv_level.
    APPEND ms_node_tab TO ct_node_tab.

* And do the same for its children
    lv_level = iv_level + 1.
    CALL FUNCTION 'FI_BS_NODE_GET_CHILDREN'
      EXPORTING
        node              = is_node_id
        with_gl_accounts  = ms_rfgbst-glac_on
      TABLES
        children_node_tab = children_node_tab
      EXCEPTIONS
        node_not_found    = 01.

    LOOP AT children_node_tab INTO children_node.
      add_node_with_subtree( EXPORTING iv_level = lv_level is_node_id = children_node
                       CHANGING ct_node_tab = ct_node_tab ).
    ENDLOOP.


  endmethod.


  method CREATE_IFM_NODE_TAB.


*// Make a IFM FSV Tree from here.
    DATA: ls_node_tab      LIKE LINE OF it_node_tab,
          ls_temp_node_tab TYPE LINE OF tyt_node_tab.

    DATA: lv_hkont TYPE hkont,
          lv_text1 TYPE seu_text.

    DATA ls_split_name TYPE tys_split_name.

    DATA: lv_id     TYPE seu_id,
          lv_tlevel TYPE seu_level,
          lv_parent TYPE seu_id.

    FIELD-SYMBOLS: <ls_layout>       LIKE LINE OF mt_layout,
                   <ls_ifm_node_tab> LIKE LINE OF rt_ifm_node_tab.

*// Make accounts.
    READ TABLE it_node_tab INTO ls_node_tab WITH KEY type = zif_flifm_definitions=>c_node_type-root.
    IF sy-subrc = 0.
      APPEND INITIAL LINE TO rt_ifm_node_tab ASSIGNING <ls_ifm_node_tab>.
      MOVE-CORRESPONDING ls_node_tab TO <ls_ifm_node_tab>.
    ENDIF.

    LOOP AT mt_layout ASSIGNING <ls_layout>.

      "If layout does not have name value, it is all collect.
      READ TABLE it_node_tab INTO ls_node_tab WITH KEY name = <ls_layout>-name.

      IF sy-subrc = 0.

        "If name have a value, it is stored up to the next highest level.
        LOOP AT it_node_tab INTO ls_node_tab FROM sy-tabix.

          IF ls_node_tab-name <> <ls_layout>-name AND ls_node_tab-parent = 1.
            EXIT.
          ENDIF.

          CLEAR ls_temp_node_tab.
          MOVE-CORRESPONDING ls_node_tab TO ls_temp_node_tab.

          CASE ls_temp_node_tab-type.

            WHEN zif_flifm_definitions=>c_node_type-acct_interv.

              ls_split_name = ls_temp_node_tab-name.

              "Check account intervals. if it's not intervals, it's one account.
              IF ls_split_name-acct_from = ls_split_name-acct_to.
                ls_temp_node_tab-name = ls_split_name-acct_from.
                ls_temp_node_tab-type = zif_flifm_definitions=>c_node_type-gl."Change Type. A -> G
                CONCATENATE ls_temp_node_tab-name ls_temp_node_tab-text+4
                  INTO ls_temp_node_tab-text SEPARATED BY space.
              ELSE.
                lv_id     = ls_temp_node_tab-id.
                lv_tlevel = ls_temp_node_tab-tlevel.
                lv_parent = ls_temp_node_tab-parent.
                CONTINUE.
              ENDIF.

            WHEN zif_flifm_definitions=>c_node_type-gl.

              IF lv_id = ls_temp_node_tab-parent.
                ls_temp_node_tab-tlevel = lv_tlevel.
                ls_temp_node_tab-parent = lv_parent.
              ENDIF.

              CONCATENATE ls_temp_node_tab-name ls_temp_node_tab-text
                INTO ls_temp_node_tab-text SEPARATED BY space.

          ENDCASE.

          APPEND INITIAL LINE TO rt_ifm_node_tab ASSIGNING <ls_ifm_node_tab>.
          MOVE-CORRESPONDING ls_temp_node_tab TO <ls_ifm_node_tab>.

        ENDLOOP.

      ELSE.

        zcx_flifm_exception=>raise_t100( iv_msgno = 010 iv_msgv1 = |{ <ls_layout>-name }| ).

      ENDIF.

    ENDLOOP.


  endmethod.


  method CREATE_IFM_NODE_TAB_ONE_STEP.

*// Make a IFM FSV Tree from here.
    DATA ls_node_tab LIKE LINE OF it_node_tab.

    DATA: lv_hkont TYPE hkont,
          lv_text1 TYPE seu_text.

    DATA ls_split_name TYPE tys_split_name.

    FIELD-SYMBOLS: <ls_layout>        LIKE LINE OF mt_layout,
                   <ls_temp_node_tab> LIKE LINE OF rt_temp_node_tab.

*// Make accounts.
    READ TABLE it_node_tab INTO ls_node_tab WITH KEY type = zif_flifm_definitions=>c_node_type-root.
    IF sy-subrc = 0.
      APPEND INITIAL LINE TO rt_temp_node_tab ASSIGNING <ls_temp_node_tab>.
      MOVE ls_node_tab TO <ls_temp_node_tab>.
    ENDIF.

    LOOP AT mt_layout ASSIGNING <ls_layout>.

      "If layout does not have name value, it is all collect.
      READ TABLE it_node_tab INTO ls_node_tab WITH KEY name = <ls_layout>-name.

      IF sy-subrc = 0.

        "If name have a value, it is stored up to the next highest level.
        LOOP AT it_node_tab INTO ls_node_tab FROM sy-tabix.

          IF ls_node_tab-name <> <ls_layout>-name AND ls_node_tab-parent = 1.
            EXIT.
          ENDIF.

          APPEND INITIAL LINE TO rt_temp_node_tab ASSIGNING <ls_temp_node_tab>.

          MOVE ls_node_tab TO <ls_temp_node_tab>.

          CASE <ls_temp_node_tab>-type.

            WHEN zif_flifm_definitions=>c_node_type-position.

            WHEN zif_flifm_definitions=>c_node_type-acct_interv.

              ls_split_name = <ls_temp_node_tab>-name.

              "Check account intervals. if it's not intervals, it's one account.
              IF ls_split_name-acct_from = ls_split_name-acct_to.
                <ls_temp_node_tab>-name = ls_split_name-acct_from.
              ELSE.

              ENDIF.

            WHEN zif_flifm_definitions=>c_node_type-gl.

          ENDCASE.

        ENDLOOP.

      ELSE.

        zcx_flifm_exception=>raise_t100( iv_msgno = 010 iv_msgv1 = |{ <ls_layout>-name }| ).

      ENDIF.

    ENDLOOP.


  endmethod.


  method CREATE_IFM_NODE_TAB_TWO_STEP.


    DATA: lv_tlevel TYPE seu_level,
          lv_parent TYPE seu_id,
          lv_child  TYPE seu_id,
          lv_text   TYPE seu_text.

    DATA ls_temp_node_tab LIKE LINE OF it_temp_node_tab.

    FIELD-SYMBOLS <ls_ifm_node_tab> LIKE LINE OF rt_ifm_node_tab.

    LOOP AT it_temp_node_tab INTO ls_temp_node_tab.

      IF ls_temp_node_tab-type = 'A'.
        CLEAR : lv_tlevel, lv_parent, lv_child.

        IF ls_temp_node_tab-text = 'X|X' OR ls_temp_node_tab-text = '_|X' OR ls_temp_node_tab-text = 'X|_'. "?
          lv_tlevel = ls_temp_node_tab-tlevel.
          lv_parent = ls_temp_node_tab-parent.
          lv_child = ls_temp_node_tab-child.
          CONTINUE.
        ELSE.
          IF ls_temp_node_tab-text+4 <> space.
            CONCATENATE ls_temp_node_tab-name ls_temp_node_tab-text+4 INTO ls_temp_node_tab-text SEPARATED BY space.
          ENDIF.
        ENDIF.

      ENDIF.

      IF lv_tlevel <> '00'.
        IF lv_tlevel < ls_temp_node_tab-tlevel.
          ls_temp_node_tab-tlevel = lv_tlevel.
          ls_temp_node_tab-parent = lv_parent.
          ls_temp_node_tab-child = lv_child.
        ENDIF.
      ENDIF.

      IF ls_temp_node_tab-type = 'G'.
        CONCATENATE ls_temp_node_tab-name ls_temp_node_tab-text INTO ls_temp_node_tab-text SEPARATED BY space.
      ENDIF.

      IF ls_temp_node_tab-type = 'P'.
        IF ls_temp_node_tab-text1 IS INITIAL.
        ELSE.
          ls_temp_node_tab-text = ls_temp_node_tab-text1.
        ENDIF.
      ENDIF.

      APPEND INITIAL LINE TO rt_ifm_node_tab ASSIGNING <ls_ifm_node_tab>.
      MOVE-CORRESPONDING ls_temp_node_tab TO <ls_ifm_node_tab>.

    ENDLOOP.

    SORT rt_ifm_node_tab BY id.


  endmethod.


  method CREATE_NODE_TAB.


    DATA: root_id TYPE t_bs_node_id.

    CLEAR: mt_node_tab, mv_level.

* Get the root
    CALL FUNCTION 'FI_BS_GET_ROOT'
      IMPORTING
        root_id   = root_id
      EXCEPTIONS
        not_found = 01.

    add_node_with_subtree( EXPORTING iv_level = 1 is_node_id = root_id
                           CHANGING ct_node_tab = rt_node_tab ).

    CALL FUNCTION 'RS_TREE_CONSTRUCT'
      TABLES
        nodetab            = rt_node_tab
      EXCEPTIONS
        tree_failure       = 1
        id_not_found       = 2
        wrong_relationship = 3
        OTHERS             = 4.


  endmethod.


  method GET_FSV_TREE.

*// Reference: Program RFGSBSTR

    DATA: lv_menu TYPE zflb1t_layout-gubun.

    init_first( ).

    mt_layout = it_layout.
    lv_menu   = iv_menu.
    mv_versn  = iv_versn.

    DELETE mt_layout WHERE item <> lv_menu.

    load_versn( ).

    rt_ifm_node_tab = create_ifm_node_tab( create_node_tab( ) ).


  endmethod.


  method INIT_FIRST.


    CLEAR: mt_layout, mv_versn, ms_rfgbst, ms_bs_info.

* g/l accounts visible (changed in 3.0d)
    ms_rfgbst-glac_on = abap_true.

* Position keys visible
    ms_rfgbst-key_on = abap_true.

* Program mode 1 ( main/ planning/ translation )

    ms_rfgbst-prog_mode1 = 'M'.

* Program mode 2 ( update/ display )
    ms_rfgbst-prog_mode2 = 'D'.

* Defaults for checking popup (800)
    ms_rfgbst-chck_side  = abap_true.
    ms_rfgbst-chck_ktopl = abap_true.
    ms_rfgbst-chck_aktps = abap_true.


  endmethod.


  method LOAD_VERSN.


* load the version
    CALL FUNCTION 'FI_BS_LOAD'
      EXPORTING
        version         = mv_versn
      IMPORTING
        bs_info         = ms_bs_info
      EXCEPTIONS
        not_found       = 1
        pos_not_found   = 2
        langu_not_found = 3
        OTHERS          = 4.

    IF sy-subrc <> 0.

      DATA : lt_abap_stack TYPE abap_callstack,
             lt_syst_stack TYPE sys_callst.
      CALL FUNCTION 'SYSTEM_CALLSTACK'
        IMPORTING
          callstack    = lt_abap_stack
          et_callstack = lt_syst_stack.

      zcx_flifm_exception=>raise_t100( iv_msgno = 001
                                       iv_msgv1 = 'FI_BS_LOAD'
                                       iv_msgv2 = |{ sy-subrc }| ).

    ENDIF.

* Load the texts
    load_versn_text( ).


  endmethod.


  method LOAD_VERSN_TEXT.


    DATA: flg_langu_maint       TYPE c VALUE abap_false,
          flg_sylangu_not_found TYPE c,
          maint_langu           LIKE sy-langu.

    CALL FUNCTION 'FI_BS_LOAD_LANGU'
      EXPORTING
        flg_langu_maint       = flg_langu_maint
      IMPORTING
        flg_sylangu_not_found = flg_sylangu_not_found
        maint_langu           = maint_langu
      EXCEPTIONS
        OTHERS                = 1.

* if maintenance language was loaded, because sy-langu was not found
    IF flg_sylangu_not_found = abap_true.
      MESSAGE s741(fe) WITH sy-langu maint_langu.
    ENDIF.

* if maintenance language is wanted, but is different from SY-LANGU
* tell him
    IF flg_langu_maint = abap_true AND maint_langu <> sy-langu.
      MESSAGE s742(fe) WITH maint_langu mv_versn.
    ENDIF.


  endmethod.
ENDCLASS.
