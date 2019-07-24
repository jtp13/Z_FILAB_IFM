*----------------------------------------------------------------------*
***INCLUDE LZFLIFM_FG_VIEWF01.
*----------------------------------------------------------------------*

FORM zflifmv_layout_input.

  DATA : ls_setting TYPE zflifmt_setting.

  SELECT SINGLE * INTO ls_setting
    FROM zflifmt_setting.

  IF sy-subrc = 0 AND zflifmv_layout-versn IS INITIAL.
    zflifmv_layout-versn = ls_setting-versn.
  ENDIF.

  zflifmv_layout-erdat = sy-datlo.
  zflifmv_layout-erzet = sy-timlo.
  zflifmv_layout-ernam = sy-uname.

ENDFORM.

FORM zflifmv_tcurr_input.

  zflifmv_tcurr-erdat = sy-datlo.
  zflifmv_tcurr-erzet = sy-timlo.
  zflifmv_tcurr-ernam = sy-uname.

ENDFORM.
